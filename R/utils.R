# ---- shared helpers -------------------------------------------------------------

html_escape <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  gsub("\"", "&quot;", text, fixed = TRUE)
}

# Undoes html_escape(), and the escaping of `tools::Rd2HTML()` and
# short_name().
html_unescape <- function(text) {
  text <- gsub("&lt;", "<", text, fixed = TRUE)
  text <- gsub("&gt;", ">", text, fixed = TRUE)
  text <- gsub("&quot;", "\"", text, fixed = TRUE)
  text <- gsub("&#39;", "'", text, fixed = TRUE)
  text <- gsub("&#96;", "`", text, fixed = TRUE)
  gsub("&amp;", "&", text, fixed = TRUE) # must come last
}

read_utf8 <- function(file, ...) {
  readLines(file, warn = FALSE, encoding = "UTF-8", ...)
}

write_utf8 <- function(lines, file, ...) {
  writeLines(enc2utf8(lines), file, useBytes = TRUE, ...)
}

is_string <- function(x) is.character(x) && length(x) == 1

# `x` with runs of whitespace made one space, and none at the ends.
squish <- function(x) trimws(gsub("\\s+", " ", x))

# `x` with its braces escaped, so cli doesn't interpolate them.
cli_escape <- function(x) gsub("([{}])", "\\1\\1", x)

# The lines `x` as cli bullets of the kind `bullet`, shown as is.
cli_lines <- function(x, bullet = " ") {
  structure(cli_escape(x), names = rep(bullet, length(x)))
}

# Each of `text` with every match of the Perl regex `pattern` replaced by
# `fn(match, groups)`, `groups` being its capture groups (NA where a group
# didn't take part), and the text between matches by `between()`.
gsub_fn <- function(text, pattern, fn, between = identity) {
  one <- function(text) {
    m <- gregexpr(pattern, text, perl = TRUE)[[1]]
    if (m[1] == -1) {
      return(between(text))
    }
    starts <- as.integer(m)
    ends <- starts + attr(m, "match.length") - 1L
    # Without capture groups, there are no capture attributes.
    cap_starts <- attr(m, "capture.start")
    cap_lens <- attr(m, "capture.length")
    if (is.null(cap_starts)) {
      cap_starts <- cap_lens <- matrix(0L, length(starts), 0)
    }
    cap_ends <- cap_starts + cap_lens - 1L
    pieces <- character(2L * length(starts) + 1L)
    pos <- 1L
    for (i in seq_along(starts)) {
      groups <- character()
      if (ncol(cap_starts) > 0) {
        groups <- substring(text, cap_starts[i, ], cap_ends[i, ])
        groups[cap_starts[i, ] < 1] <- NA
      }
      pieces[[2L * i - 1L]] <- between(substr(text, pos, starts[i] - 1L))
      pieces[[2L * i]] <- fn(substr(text, starts[i], ends[i]), unname(groups))
      pos <- ends[i] + 1L
    }
    pieces[[length(pieces)]] <- between(substr(text, pos, nchar(text)))
    paste(pieces, collapse = "")
  }
  vapply(text, one, character(1), USE.NAMES = FALSE)
}

split_lines <- function(text) {
  # The sentinel keeps trailing empty lines, which strsplit() would drop.
  utils::head(strsplit(paste0(text, "\n."), "\n", fixed = TRUE)[[1]], -1)
}

# For each of the Markdown `lines`, "open" or "close" for a code block's
# fences, "code" inside one, and "" outside. A block is closed by a fence of
# the same character, at least as long as the one that opened it.
code_block_lines <- function(lines) {
  kinds <- character(length(lines))
  fence <- NULL # The open code block's fence, if any.
  for (i in seq_along(lines)) {
    if (!is.null(fence)) {
      closing <- grepl(paste0("^\\s*", fence, "+\\s*$"), lines[[i]])
      kinds[[i]] <- if (closing) "close" else "code"
      if (closing) fence <- NULL
    } else if (grepl("^\\s*(```+|~~~+)", lines[[i]])) {
      fence <- sub("^\\s*(```+|~~~+).*$", "\\1", lines[[i]])
      kinds[[i]] <- "open"
    }
  }
  kinds
}

# knitr runs inline `r expr` and code chunks even where Markdown only shows
# them: in a code span, and a chunk's fence inside a code block. Text that
# wasn't written for knitr (a README.md, Rd) shows them as raw HTML instead,
# with no backticks for knitr to find. Simplified from
# `knitr::all_patterns$md`.
knitr_inline_pattern <- "`r[ #][^`]+`"
knitr_chunk_pattern <- "^[\t >]*```+\\s*\\{"

# `code` as raw HTML `<code>`, with its punctuation as character references,
# so Pandoc reads none of it as Markdown.
html_code <- function(code) {
  paste0("<code>", html_text(code), "</code>")
}

# `text` with every ASCII punctuation character as an entity, so neither HTML,
# Markdown, Quarto shortcodes (`{{<`) nor knitr read anything in it.
html_text <- function(text) {
  vapply(
    strsplit(text, "", fixed = TRUE),
    function(chars) {
      punct <- grepl("^[!-/:-@[-`{-~]$", chars)
      chars[punct] <- sprintf(
        "&#%d;",
        vapply(chars[punct], utf8ToInt, integer(1))
      )
      paste(chars, collapse = "")
    },
    character(1)
  )
}

# The `code` lines as a raw HTML code block in `lang`, the lines of a fenced
# one otherwise.
html_code_block <- function(code, lang = "") {
  code <- gsub("`", "&#96;", html_escape(code), fixed = TRUE)
  class <- if (nzchar(lang)) sprintf(" class=\"sourceCode %s\"", lang) else ""
  code[1] <- sprintf("<pre%s><code>%s", class, code[1])
  code[length(code)] <- paste0(code[length(code)], "</code></pre>")
  code
}

# `lines` of Markdown with each code block holding a chunk's fence as raw
# HTML (see html_code_block()).
escape_knitr_blocks <- function(lines) {
  kinds <- code_block_lines(lines)
  opens <- rev(which(kinds == "open"))
  for (open in opens) {
    end <- open
    while (end < length(lines) && kinds[[end + 1L]] == "code") {
      end <- end + 1L
    }
    code <- lines[seq_len(end - open) + open]
    if (!any(grepl(knitr_chunk_pattern, code))) {
      next
    }
    lang <- sub(
      "^\\s*(```+|~~~+)\\s*\\{?\\.?([A-Za-z0-9_+-]*).*$",
      "\\2",
      lines[[open]]
    )
    closed <- end < length(lines) && kinds[[end + 1L]] == "close"
    lines <- c(
      lines[seq_len(open - 1L)],
      "",
      html_code_block(code, lang),
      "",
      lines[-seq_len(end + closed)]
    )
  }
  lines
}

# `lines` as its YAML `front_matter` (`---` to `---` or `...`, after any
# blank lines, empty if there's none) and the `body` after it.
split_front_matter <- function(lines) {
  first <- which(nzchar(trimws(lines)))[1]
  end <- 0L
  if (!is.na(first) && trimws(lines[first]) == "---") {
    close <- grep("^(---|\\.\\.\\.)\\s*$", lines[-seq_len(first)])[1]
    if (!is.na(close)) end <- first + close
  }
  list(
    front_matter = lines[seq_len(end)],
    body = lines[seq_len(length(lines) - end) + end]
  )
}
