# ---- hover tooltips -----------------------------------------------------------
#
# Links to the package's own topics (see link_code_html()) get a tooltip
# showing the signature and the topic's title. The tooltip's style and
# script are in quartodown.scss and quartodown-after-body.html.

# Relative path from a rendered page to docs/reference/, e.g. "reference/"
# from docs/index.html or "../reference/" one directory down.
reference_href_prefix <- function(file, docs) {
  rel <- fs::path_rel(
    fs::path(docs, "reference"),
    start = fs::path_dir(file)
  )
  paste0(as.character(rel), "/")
}

# ---- topic index --------------------------------------------------------------
#
# For each documented alias: its usage, its topic's title and its topic.
# Each alias gets the \usage{} block that calls it; the topic's \name gets the
# whole \usage{}, and any other alias none.

build_topic_index <- function(
  path = ".",
  config = list(),
  topics = rd_topics(path)
) {
  index <- list()
  # Only topics with a page (see has_page()) have something to link to.
  for (t in topics[has_page(topics, config)]) {
    rd <- t$rd
    topic <- t$basename
    aliases <- t$alias

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

    is_data <- "data" %in% trimws(unlist(rd[tags == "\\docType"]))
    title <- rd_section_text(rd, tags, "\\title")
    if (!is.null(title)) {
      title <- squish(title)
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
        brief = title,
        topic = topic,
        data = is_data
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
  unbacktick(name)
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

# ---- tooltip contents ---------------------------------------------------------

# The topic index with each usage's HTML in `usage_html`, once per build since
# the tips repeat across pages.
highlight_usages <- function(index) {
  lapply(index, function(entry) {
    if (!is.null(entry$usage)) {
      entry$usage_html <- usage_html(entry$usage)
    }
    entry
  })
}

# `usage` highlighted, and then clipped so that what's left out can't stop it
# from parsing.
usage_html <- function(usage) {
  clip_usage(highlight_usage(usage))
}

# The HTML of a \usage{} of more than `max_lines` lines, such as `theme()`'s
# ~150 arguments, as its first lines, a line saying how many are left out and
# its last line (usually the closing parenthesis), so the tip fits on the
# screen.
clip_usage <- function(html, max_lines = 12) {
  lines <- strsplit(html, "\n", fixed = TRUE)[[1]]
  n <- length(lines)
  if (n <= max_lines) {
    return(html)
  }
  keep <- max_lines - 2
  paste(
    c(
      lines[seq_len(keep)],
      sprintf('  <span class="co"># ... %d more lines</span>', n - keep - 1),
      lines[[n]]
    ),
    collapse = "\n"
  )
}

# `usage` with the classes Pandoc gives R code, so the tip has the colors of
# the page's code blocks, which keep Quarto's highlighting. Usage that doesn't
# parse is just escaped.
highlight_usage <- function(usage) {
  # The parser counts a tab as up to 8 columns.
  exprs <- safe_parse(gsub("\t", " ", usage, fixed = TRUE))
  data <- if (!is.null(exprs)) utils::getParseData(exprs)
  if (is.null(data)) {
    return(html_escape(usage))
  }
  data <- data[data$terminal, , drop = FALSE]
  data <- data[order(data$line1, data$col1), , drop = FALSE]
  classes <- token_classes(data$token, data$text)
  pos <- token_positions(data, usage)
  pieces <- character()
  at <- 1
  for (i in which(!is.na(classes))) {
    pieces <- c(
      pieces,
      html_escape(substr(usage, at, pos$start[[i]] - 1)),
      sprintf(
        '<span class="%s">%s</span>',
        classes[[i]],
        html_escape(substr(usage, pos$start[[i]], pos$end[[i]]))
      )
    )
    at <- pos$end[[i]] + 1
  }
  paste(
    c(pieces, html_escape(substr(usage, at, nchar(usage)))),
    collapse = ""
  )
}

# Pandoc's class for each of R's parser tokens, as `text`, or NA for those it
# leaves plain, such as variables and parentheses.
token_classes <- function(token, text) {
  classes <- pandoc_token_classes[token]
  numbers <- token == "NUM_CONST"
  classes[numbers] <- ifelse(
    grepl("^[0-9]+L?$", text[numbers]),
    "dv",
    ifelse(grepl("^[0-9.]", text[numbers]), "fl", "cn")
  )
  # An argument's name, but not a `function(x)`'s without a default.
  named <- token == "SYMBOL_FORMALS" & c(token[-1], "") == "EQ_FORMALS"
  classes[named] <- "at"
  unname(classes)
}

pandoc_token_classes <- c(
  SYMBOL_FUNCTION_CALL = "fu",
  STR_CONST = "st",
  NULL_CONST = "cn",
  COMMENT = "co",
  SYMBOL_SUB = "at",
  EQ_SUB = "at",
  EQ_FORMALS = "at",
  FUNCTION = "cf",
  IF = "cf",
  ELSE = "cf",
  FOR = "cf",
  IN = "cf",
  WHILE = "cf",
  REPEAT = "cf",
  BREAK = "cf",
  NEXT = "cf",
  LEFT_ASSIGN = "ot",
  RIGHT_ASSIGN = "ot",
  EQ_ASSIGN = "ot",
  "'-'" = "sc",
  "'+'" = "sc",
  "'*'" = "sc",
  "'/'" = "sc",
  "'^'" = "sc",
  "'~'" = "sc",
  "'!'" = "sc",
  "'?'" = "sc",
  "'$'" = "sc",
  "'@'" = "sc",
  "':'" = "sc",
  LT = "sc",
  GT = "sc",
  LE = "sc",
  GE = "sc",
  EQ = "sc",
  NE = "sc",
  AND = "sc",
  AND2 = "sc",
  OR = "sc",
  OR2 = "sc",
  SPECIAL = "sc",
  PIPE = "sc",
  NS_GET = "sc",
  NS_GET_INT = "sc"
)

# `tips`: id -> list(usage, usage_html, brief, name), rendered as a hidden
# payload that quartodown-after-body.html's script reads via
# `data-for`/`data-quartodown-tip`.
tooltip_payload_html <- function(tips) {
  entries <- vapply(
    names(tips),
    function(id) {
      tip <- tips[[id]]
      sig <- tip$usage_html %||% html_escape(tip$usage %||% tip$name)
      brief_html <- if (!is.null(tip$brief)) {
        brief <- gsub("`([^`]+)`", "<code>\\1</code>", html_escape(tip$brief))
        paste0("<span class=\"quartodown-tip-brief\">", brief, "</span>")
      } else {
        ""
      }
      sprintf(
        '<div data-for="%s"><code class="quartodown-tip-sig">%s</code>%s</div>',
        id,
        sig,
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
