# ---- hover tooltips -----------------------------------------------------------
#
# Links references to the package's own topics in docs/ to their reference
# page, with a tooltip showing the signature and a one-line summary. Any
# existing downlit href is replaced, as downlit can't resolve the package's
# own, unpublished topics. Matched are `fu`/`va`/`kw` spans and `%op%`
# `op`/`sc` spans in code blocks, and inline `<code>`, linked or not, in the
# HTML as xml2 writes it (see postprocess_pages()). The tooltip's style and
# script are in quartodown.scss and quartodown-after-body.html.

tooltip_link_re <- paste0(
  # One group number each for the class and the name in either kind of span.
  # Pandoc's R syntax highlights some functions (e.g. R6Class) as keywords.
  "(?|<span class=\"(fu|va|kw)\">(?:<a href=\"[^\"]*\">)?([^<]*?)(?:</a>)?</span>",
  "|<span class=\"(op|sc)\">(?:<a href=\"[^\"]*\">)?(%[^%<]+%)(?:</a>)?</span>)",
  "|",
  # One group number for the name in either shape of inline code.
  "(?|<code>(?:<a href=\"[^\"]*\">)?([^<]*?)(?:</a>)?</code>",
  "|<a href=\"[^\"]*\"><code>([^<]*?)</code></a>)"
)

# The page `html` with tooltips (see inject_tooltips_html()) and, if it has
# any, their contents in a hidden element at the end of the body, which
# quartodown-after-body.html's script shows. Returns `html` and `n`, the
# number of links.
add_tooltips_html <- function(
  html,
  index,
  href_prefix = "reference/",
  pkg = NULL
) {
  result <- inject_tooltips_html(html, index, href_prefix, pkg = pkg)
  html <- result$html
  if (result$n > 0) {
    payload <- tooltip_payload_html(result$tips)
    # The last </body>, as a script may contain the string too.
    end <- "</body>(\\s*</html>\\s*)$"
    html <- if (grepl(end, html, perl = TRUE)) {
      sub(end, paste0(payload, "</body>\\1"), html, perl = TRUE)
    } else {
      paste0(html, payload)
    }
  }
  list(html = html, n = result$n)
}

# Relative path from a rendered page to docs/reference/, e.g. "reference/"
# from docs/index.html or "../reference/" one directory down.
reference_href_prefix <- function(file, docs) {
  rel <- fs::path_rel(
    fs::path(docs, "reference"),
    start = fs::path_dir(file)
  )
  paste0(as.character(rel), "/")
}

# Links every `tooltip_link_re` match found in `index` and tags it with a tip
# id. `tips` has one entry per linked name, for tooltip_payload_html(). `pkg`
# is the package's name, so `pkg::foo()` resolves like `foo()`.
inject_tooltips_html <- function(
  html,
  index,
  href_prefix = "reference/",
  pkg = NULL
) {
  m <- gregexpr(tooltip_link_re, html, perl = TRUE)[[1]]
  if (m[1] == -1) {
    return(list(html = html, n = 0L, tips = list()))
  }

  starts <- as.integer(m)
  lens <- attr(m, "match.length")
  cap_starts <- attr(m, "capture.start")
  cap_lens <- attr(m, "capture.length")
  capture <- function(i, col) {
    substr(html, cap_starts[i, col], cap_starts[i, col] + cap_lens[i, col] - 1L)
  }

  pieces <- vector("list", length(starts) * 2L + 1L)
  pos <- 1L
  pi <- 1L
  n_linked <- 0L
  tip_ids <- character(0)
  tips <- list()

  for (i in seq_along(starts)) {
    s <- starts[i]
    l <- lens[i]
    is_block <- cap_starts[i, 1] != 0L # span vs. inline <code>
    name_html <- capture(i, if (is_block) 2L else 3L)
    name <- html_unescape(name_html)
    # "foo(1, 2)", "?foo", "pkg::foo" and "`foo`" all look up "foo".
    lookup <- sub("\\(.*$", "", name, perl = TRUE)
    lookup <- sub("^\\?", "", lookup)
    if (!is.null(pkg)) {
      lookup <- sub(paste0("^", pkg, ":::?"), "", lookup)
    }
    lookup <- sub("^`(.*)`$", "\\1", lookup)

    entry <- index[[lookup]]

    pieces[[pi]] <- substr(html, pos, s - 1L)
    pi <- pi + 1L
    if (is.null(entry)) {
      pieces[[pi]] <- substr(html, s, s + l - 1L)
    } else {
      id <- tip_ids[lookup]
      if (is.na(id)) {
        id <- paste0("t", length(tips) + 1L)
        tip_ids[lookup] <- id
        entry$name <- lookup
        tips[[id]] <- entry
      }
      link <- sprintf(
        '<a class="quartodown-tip-ref" data-quartodown-tip="%s" href="%s%s.html">%s</a>',
        id,
        href_prefix,
        entry$topic,
        name_html
      )
      pieces[[pi]] <- if (is_block) {
        sprintf('<span class="%s">%s</span>', capture(i, 1L), link)
      } else {
        paste0("<code>", link, "</code>")
      }
      n_linked <- n_linked + 1L
    }
    pi <- pi + 1L
    pos <- s + l
  }
  pieces[[pi]] <- substr(html, pos, nchar(html))

  list(html = paste(unlist(pieces), collapse = ""), n = n_linked, tips = tips)
}

# ---- topic index --------------------------------------------------------------
#
# For each documented alias: its usage, a one-line summary and its topic.
# Each alias gets the \usage{} block that calls it; the topic's \name gets the
# whole \usage{}, and any other alias none.

build_topic_index <- function(
  path = ".",
  config = list(),
  topics = rd_topics(path),
  quiet = FALSE
) {
  index <- list()
  # Only topics with a page (see has_page()) have something to link to.
  for (t in topics[has_page(topics, config)]) {
    rd <- t$rd
    topic <- t$basename
    aliases <- t$alias
    # Relative, so the {.file} link in the warning below resolves.
    rd_name <- fs::path_rel(file.path(path, "man", paste0(topic, ".Rd")))

    tags <- rd_tags(rd)
    usage_node <- rd[tags == "\\usage"]
    usage_blocks <- if (length(usage_node) == 0) {
      character(0)
    } else {
      rd_usage_blocks(usage_node[[1]])
    }
    usage_whole <- if (length(usage_blocks) == 0) {
      NULL
    } else {
      paste(usage_blocks, collapse = "\n\n")
    }

    sentence <- first_sentence(rd_section_text(rd, tags, "\\description"))
    if (!quiet && sentence$clipped) {
      cli::cli_warn(c(
        "The tooltip summary for {.file {rd_name}} is longer than 200 characters and got truncated mid-word.",
        "i" = "Shorten the first sentence of \\description{{}} to fit within 200 characters."
      ))
    }

    for (alias in aliases) {
      match_idx <- match(alias, names(usage_blocks))
      usage <- if (!is.na(match_idx)) {
        usage_blocks[[match_idx]]
      } else if (alias %in% t$name) {
        usage_whole
      }
      index[[alias]] <- list(
        usage = usage,
        brief = sentence$text,
        topic = topic
      )
    }
  }
  index
}

# A \usage{} node split on blank lines into one block per signature, named by
# the alias it documents (see rd_usage_alias()).
rd_usage_blocks <- function(usage_node) {
  blocks <- character(0)
  current <- character(0)
  method <- NULL

  # The signature made of the pieces `current`, or nothing if they're blank.
  usage_block <- function(current, method) {
    text <- trimws(paste(current, collapse = ""))
    if (nchar(text) == 0) {
      return(character(0))
    }
    structure(text, names = rd_usage_alias(text, method))
  }

  for (child in usage_node) {
    tag <- attr(child, "Rd_tag")
    if (!is.null(tag) && tag %in% c("\\method", "\\S3method", "\\S4method")) {
      method <- list(
        s4 = tag == "\\S4method",
        generic = paste(unlist(child[[1]]), collapse = ""),
        signature = gsub("\\s", "", paste(unlist(child[[2]]), collapse = ""))
      )
      current[[length(current) + 1L]] <- if (method$s4) {
        method$generic
      } else {
        paste0(method$generic, ".", method$signature)
      }
    } else {
      piece <- paste(unlist(child), collapse = "")
      if (nchar(trimws(piece)) == 0) {
        # A blank-only run separates one signature from the next.
        blocks <- c(blocks, usage_block(current, method))
        current <- character(0)
        method <- NULL
      } else {
        current[[length(current) + 1L]] <- piece
      }
    }
  }
  c(blocks, usage_block(current, method))
}

# The alias the \usage{} block `block` documents. For a method, `method` is
# its macro's generic and class or signature, and the alias is spelled as
# roxygen does: "generic.class" or "generic,signature-method", with "<-"
# after the generic for a replacement method.
rd_usage_alias <- function(block, method = NULL) {
  replacement <- grepl("(?s)\\)\\s*<-\\s*\\S+$", block, perl = TRUE)
  if (is.null(method)) {
    name <- rd_usage_leading_name(block)
    return(if (replacement) paste0(name, "<-") else name)
  }
  generic <- if (replacement) paste0(method$generic, "<-") else method$generic
  if (method$s4) {
    paste0(generic, ",", method$signature, "-method")
  } else {
    paste0(generic, ".", method$signature)
  }
}

# The identifier before the first "(" of a \usage{} block, or the operator of
# an infix call such as "lhs %op% rhs".
rd_usage_leading_name <- function(block) {
  infix <- regmatches(block, regexec("^\\S+\\s+(%[^%]+%)\\s+\\S+$", block))[[1]]
  if (length(infix) == 2) {
    return(infix[[2]])
  }
  name <- trimws(sub("(?s)\\(.*$", "", block, perl = TRUE))
  sub("^`(.*)`$", "\\1", name)
}

# The text of a top-level Rd section, with code kept in `backticks`, or NULL.
rd_section_text <- function(rd, tags, section) {
  hit <- rd[tags == section]
  if (length(hit) == 0) {
    return(NULL)
  }
  text <- trimws(paste(vapply(hit, rd_flatten, character(1)), collapse = ""))
  if (nchar(text) == 0) NULL else text
}

rd_flatten <- function(x) {
  tag <- attr(x, "Rd_tag")
  # No text from images (such as lifecycle badges), raw output or \Sexpr.
  if (!is.null(tag) && tag %in% c("COMMENT", "\\figure", "\\out", "\\Sexpr")) {
    return("")
  }
  if (identical(tag, "\\href")) {
    return(rd_flatten(x[[2]]))
  }
  # Format-conditional text: the HTML version, as the tooltip is HTML.
  if (!is.null(tag) && tag %in% c("\\if", "\\ifelse")) {
    html <- grepl("html", paste(unlist(x[[1]]), collapse = ""), fixed = TRUE)
    branch <- if (html) {
      x[[2]]
    } else if (tag == "\\ifelse") {
      x[[3]]
    }
    return(if (is.null(branch)) "" else rd_flatten(branch))
  }
  if (is.character(x)) {
    return(paste(x, collapse = ""))
  }
  inner <- paste(vapply(x, rd_flatten, character(1)), collapse = "")
  if (!is.null(tag) && tag %in% c("\\code", "\\verb", "\\samp")) {
    paste0("`", inner, "`")
  } else {
    inner
  }
}

# The first sentence of `text`, cut at `max_chars` (then `clipped` is TRUE).
first_sentence <- function(text, max_chars = 200) {
  if (is.null(text)) {
    return(list(text = NULL, clipped = FALSE))
  }
  text <- gsub("\\s+", " ", trimws(text))
  if (nchar(text) == 0) {
    return(list(text = NULL, clipped = FALSE))
  }

  m <- regexpr("[.!?](\\s|$)", text, perl = TRUE)
  sentence <- trimws(if (m != -1) substr(text, 1, m) else text)

  clipped <- nchar(sentence) > max_chars
  if (clipped) {
    sentence <- paste0(trimws(substr(sentence, 1, max_chars)), "...")
  }
  list(text = sentence, clipped = clipped)
}

# ---- tooltip contents ---------------------------------------------------------

# `tips`: id -> list(usage, brief, name), rendered as a hidden payload that
# quartodown-after-body.html's script reads via
# `data-for`/`data-quartodown-tip`.
tooltip_payload_html <- function(tips) {
  entries <- vapply(
    names(tips),
    function(id) {
      tip <- tips[[id]]
      sig <- if (!is.null(tip$usage)) tip$usage else tip$name
      brief_html <- if (!is.null(tip$brief)) {
        brief <- gsub("`([^`]+)`", "<code>\\1</code>", html_escape(tip$brief))
        paste0("<span class=\"quartodown-tip-brief\">", brief, "</span>")
      } else {
        ""
      }
      sprintf(
        '<div data-for="%s"><code class="quartodown-tip-sig">%s</code>%s</div>',
        id,
        html_escape(sig),
        brief_html
      )
    },
    character(1)
  )

  paste0(
    '<div class="quartodown-tip-payload" hidden>',
    paste(entries, collapse = ""),
    "</div>\n"
  )
}
