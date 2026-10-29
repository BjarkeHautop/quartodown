# ---- after rendering ----------------------------------------------------------
#
# Each docs/**/*.html page is read and written once, and in between: its ANSI
# codes protected from xml2 (see protect_ansi()), the other site's content
# removed (other_site_classes()), its code linked, with tooltips for the
# package's own topics (link_code_html()), the logo put by its title
# (add_logo_html()) and the ANSI codes made `<span>`s (ansi_to_html()).

postprocess_pages <- function(
  path,
  config = list(),
  topics = rd_topics(path),
  tooltips = TRUE
) {
  docs <- site_dir(path, config)
  tooltips <- isTRUE(tooltips)
  index <- build_topic_index(path, config, topics)
  pkg <- if (length(index) > 0) pkg_name(path)
  drop <- other_site_classes(dev_mode(path, config))
  pages <- fs::dir_ls(docs, regexp = "\\.html$", recurse = TRUE)
  # A release build leaves the development site in dev/ as it was.
  if (docs == docs_dir(path)) {
    pages <- pages[!fs::path_has_parent(pages, file.path(docs, "dev"))]
  }
  logo <- site_logo(path, config)
  copy_logo(path, docs, logo)
  prune_link_cache()
  map_workers(
    as.character(pages),
    postprocess_page,
    docs = docs,
    index = index,
    pkg = pkg,
    names = linkable_names(index, pkg),
    articles = site_articles(path),
    drop = drop,
    logo = logo,
    tooltips = tooltips
  )
  strip_search_ansi(docs)
  invisible()
}

postprocess_page <- function(
  f,
  docs,
  index,
  pkg,
  drop,
  names = character(),
  articles = character(),
  logo = NULL,
  tooltips = TRUE
) {
  html <- paste(read_utf8(f), collapse = "\n")
  # 404.html is served at any URL, so a relative link would break.
  if (fs::path_rel(f, docs) != "404.html") {
    html <- add_logo_html(html, logo, fs::path_rel(docs, start = dirname(f)))
  }
  ctx <- link_context(
    pkg = pkg,
    topics = vapply(index, `[[`, character(1), "topic"),
    names = names,
    articles = articles,
    topic_path = reference_href_prefix(f, docs),
    root = paste0(fs::path_rel(docs, start = dirname(f)), "/"),
    index = index,
    tips = tooltips
  )
  html <- link_code_html(protect_ansi(html), drop = drop, ctx = ctx)
  html <- paste(ansi_to_html(split_lines(html)), collapse = "\n")
  write_utf8(html, f, sep = "")
}

# ---- logo ---------------------------------------------------------------------
#
# The site's logo (site_logo()) floats right of each page's title, its first
# `<h1>`: the title block's, or on pages without one, such as the changelog,
# their first heading's. Pages that show it already, typically the home page,
# don't get it twice.

# Copies the `logo` files into the site, which Quarto copies only when a page
# links them.
copy_logo <- function(path, docs, logo) {
  for (file in logo) {
    fs::dir_create(fs::path_dir(file.path(docs, file)))
    fs::file_copy(
      file.path(path, file),
      file.path(docs, file),
      overwrite = TRUE
    )
  }
  invisible()
}

# The page `html` with the `logo` before its first `<h1>`; `root` is the
# relative path from the page to the site's root.
add_logo_html <- function(html, logo, root = ".") {
  # Nor on a page showing it already, such as a README with it in its title.
  shown <- "<img\\b[^>]*\\bsrc=[\"'][^\"']*man/figures/logo"
  if (length(logo) == 0 || grepl(shown, html, perl = TRUE)) {
    return(html)
  }
  class <- "quartodown-logo"
  if (!is.null(names(logo))) {
    class <- paste(class, paste0(names(logo), "-content"))
  }
  img <- sprintf(
    '<img src="%s" class="%s" alt="">',
    html_escape(paste0(root, "/", logo)),
    class
  )
  html <- sub(
    "(?s)(id=\"quarto-document-content\".*?)(<h1\\b)",
    paste0("\\1", paste(img, collapse = ""), "\\2"),
    html,
    perl = TRUE
  )
  wrap_title_block(html)
}

# On a page with content in the margin, the content's elements are items of a
# grid, each holding its own floats, so the title block would grow to the
# logo's height. The title block and what follows it up to the first section
# become one item instead, unless they're placed in columns of their own.
wrap_title_block <- function(html) {
  if (!grepl("<main\\b[^>]*\\bpage-columns\\b", html, perl = TRUE)) {
    return(html)
  }
  gsub_fn(
    html,
    "(?s)<header id=\"title-block-header\".*?(?=<section\\b|</main>)",
    function(block, groups) {
      if (
        grepl("class=\"[^\"]*\\b(page-columns|column-)", block, perl = TRUE)
      ) {
        return(block)
      }
      paste0("<div class=\"quartodown-title-block\">", block, "</div>")
    }
  )
}

# Content for only one of the released and development versions' sites is
# marked `.quartodown-devel` or `.quartodown-release`: the class whose
# elements this site's pages drop. An unreleased package has no release to
# install, so its site keeps the development content.
other_site_classes <- function(mode) {
  if (mode == "release") "quartodown-devel" else "quartodown-release"
}

drop_classes <- function(doc, classes) {
  if (length(classes) == 0) {
    return(invisible())
  }
  xpath <- sprintf(
    ".//*[contains(concat(' ', normalize-space(@class), ' '), ' %s ')]",
    classes
  )
  xml2::xml_remove(xml2::xml_find_all(doc, paste(xpath, collapse = "|")))
  invisible()
}


# ---- code linking -------------------------------------------------------------
#
# Links a page's R code to the topics, vignettes and packages it refers to
# (see links.R): calls in code blocks, and inline code that is a reference,
# such as `foo()` or `?foo`. Code blocks keep Quarto's markup: R's parser
# finds the references in a block's text, and each is wrapped in a link where
# Quarto put it. A link to one of the package's own topics gets a tooltip
# (see tooltips.R).

# The page `html` (a string) with its code linked, as xml2 writes it, the
# elements with the `drop` classes removed and the tooltips' contents at the
# end of the body.
link_code_html <- function(html, drop = character(), ctx = link_context()) {
  doc <- xml2::read_html(html, encoding = "UTF-8")
  drop_classes(doc, drop)
  for (code in xml2::xml_find_all(
    doc,
    ".//pre[contains(@class, 'sourceCode r')]/code"
  )) {
    link_block(ctx, code)
  }
  link_inline(ctx, doc)
  if (length(ctx$tip_entries) > 0) {
    body <- xml2::xml_find_first(doc, ".//body")
    payload <- xml2::read_html(tooltip_payload_html(ctx$tip_entries))
    xml2::xml_add_child(body, xml2::xml_find_first(payload, ".//body/div"))
  }
  con <- rawConnection(raw(0), "wb")
  on.exit(close(con))
  xml2::write_html(doc, con)
  html <- rawToChar(rawConnectionValue(con))
  Encoding(html) <- "UTF-8"
  html
}

# The names the package's own topics are linked by when used as values
# rather than called: its datasets' and its exported objects' that aren't
# functions, such as ggproto objects. A function's name used as a value is
# more often a variable or an argument named like it.
linkable_names <- function(index, pkg) {
  data <- names(Filter(function(entry) isTRUE(entry$data), index))
  objects <- if (!is.null(pkg) && is_installed(pkg)) {
    exported <- intersect(names(index), getNamespaceExports(pkg))
    ns <- asNamespace(pkg)
    exported[
      !vapply(
        exported,
        function(name) is.function(get0(name, envir = ns)),
        logical(1)
      )
    ]
  }
  union(data, objects)
}

# ---- code blocks ----

# Links the references in the `<code>` of a code block, which may be split
# over several of its text nodes and spans; text in links (the line anchors
# and annotations' markers) isn't code.
link_block <- function(ctx, code) {
  nodes <- xml2::xml_find_all(code, ".//text()[not(ancestor::a)]")
  if (length(nodes) == 0) {
    return(invisible())
  }
  strings <- xml2::xml_text(nodes)
  refs <- code_refs(ctx, paste(strings, collapse = ""))
  if (nrow(refs) == 0) {
    return(invisible())
  }
  ends <- cumsum(nchar(strings))
  starts <- ends - nchar(strings) + 1
  for (i in seq_along(nodes)) {
    # A reference split over nodes stays unlinked.
    in_node <- refs$start >= starts[[i]] & refs$end <= ends[[i]]
    if (any(in_node)) {
      node_refs <- refs[in_node, , drop = FALSE]
      node_refs$start <- node_refs$start - starts[[i]] + 1
      node_refs$end <- node_refs$end - starts[[i]] + 1
      link_text_node(ctx, nodes[[i]], strings[[i]], node_refs)
    }
  }
  invisible()
}

# The references in the R code `text`: where each `start`s and `end`s in it,
# what it's linked to (`url`) and, for one of the package's own topics, its
# alias (`own`). The packages the code attaches are attached first.
code_refs <- function(ctx, text) {
  none <- data.frame(
    start = integer(),
    end = integer(),
    url = character(),
    own = character()
  )
  # The parser counts a tab as up to 8 columns.
  exprs <- safe_parse(gsub("\t", " ", text, fixed = TRUE))
  if (is.null(exprs)) {
    return(none)
  }
  attach_packages(ctx, attached_packages(exprs))
  data <- utils::getParseData(exprs)
  data <- data[data$terminal, , drop = FALSE]
  data <- data[order(data$line1, data$col1), , drop = FALSE]
  token <- data$token
  name <- data$text
  prev <- function(i, k = 1) if (i > k) token[[i - k]] else ""

  refs <- list()
  add <- function(i, ref) {
    if (!is.na(ref$url)) {
      refs[[length(refs) + 1]] <<- data.frame(
        i = i,
        url = ref$url,
        own = ref$own %||% NA_character_
      )
    }
  }
  for (i in seq_along(token)) {
    namespaced <- prev(i) %in%
      c("NS_GET", "NS_GET_INT") &&
      prev(i, 2) == "SYMBOL_PACKAGE"
    if (token[[i]] == "SYMBOL_FUNCTION_CALL") {
      if (namespaced) {
        add(i, topic_ref(ctx, name[[i]], name[[i - 2]]))
      } else if (prev(i) %in% c("'$'", "'@'")) {
        # A method; but an R6 class's `Class$new()` links the class.
        if (name[[i]] == "new" && prev(i) == "'$'" && prev(i, 2) == "SYMBOL") {
          add(i - 2, topic_ref(ctx, name[[i - 2]], is_fun = TRUE))
        }
      } else {
        add(i, topic_ref(ctx, name[[i]], is_fun = TRUE))
        lib <- name[[i]] %in% c("library", "require")
        # `library(pkg)`, or `library(pkg, ...)`.
        if (
          lib &&
            i + 3 <= length(token) &&
            token[[i + 3]] %in% c("')'", "','") &&
            token[[i + 2]] %in% c("SYMBOL", "STR_CONST")
        ) {
          package <- gsub("[\"'`]", "", name[[i + 2]])
          add(i + 2, list(url = href_package(package)))
        }
      }
    } else if (token[[i]] == "SPECIAL") {
      add(i, topic_ref(ctx, name[[i]], is_fun = TRUE))
    } else if (token[[i]] == "SYMBOL" && !prev(i) %in% c("'$'", "'@'")) {
      if (namespaced) {
        add(i, topic_ref(ctx, name[[i]], name[[i - 2]]))
      } else if (name[[i]] %in% ctx$names) {
        add(i, topic_ref(ctx, name[[i]]))
      }
    }
  }
  if (length(refs) == 0) {
    return(none)
  }
  refs <- do.call(rbind, refs)
  refs <- refs[!duplicated(refs$i), , drop = FALSE]
  # From the parser's lines and columns to positions in `text`.
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  line_start <- cumsum(c(1, nchar(lines) + 1))
  rows <- data[refs$i, , drop = FALSE]
  data.frame(
    start = line_start[rows$line1] + rows$col1 - 1,
    end = line_start[rows$line2] + rows$col2 - 1,
    url = refs$url,
    own = refs$own
  )
}

# The packages `exprs` attach with `library()` or `require()`.
attached_packages <- function(exprs) {
  out <- character()
  walk <- function(x) {
    if (!is.call(x)) {
      return()
    }
    fun <- x[[1]]
    if (is.symbol(fun) && as.character(fun) %in% c("library", "require")) {
      call <- tryCatch(
        match.call(get(as.character(fun), baseenv()), x),
        error = function(e) NULL
      )
      if (
        !is.null(call) && !isTRUE(call$character.only) && !is.null(call$package)
      ) {
        out <<- c(out, as.character(call$package))
      }
      return()
    }
    for (arg in as.list(x)[-1]) {
      if (!missing(arg)) walk(arg)
    }
  }
  for (x in exprs) {
    walk(x)
  }
  unique(out)
}

safe_parse <- function(text) {
  tryCatch(
    parse(text = text, keep.source = TRUE, encoding = "UTF-8"),
    error = function(e) NULL
  )
}

# Replaces the text node `node` (its text, `text`) with the same text, its
# `refs` (positions in `text`) made links.
link_text_node <- function(ctx, node, text, refs) {
  refs <- refs[order(refs$start), , drop = FALSE]
  pieces <- character()
  at <- 1
  for (r in seq_len(nrow(refs))) {
    pieces <- c(
      pieces,
      html_escape(substr(text, at, refs$start[[r]] - 1)),
      link_html(
        ctx,
        html_escape(substr(text, refs$start[[r]], refs$end[[r]])),
        refs$url[[r]],
        refs$own[[r]]
      )
    )
    at <- refs$end[[r]] + 1
  }
  pieces <- c(pieces, html_escape(substr(text, at, nchar(text))))
  replace_with_html(node, paste(pieces, collapse = ""))
}

# Replaces `node` with the nodes of the HTML fragment `html`. They go before
# a placeholder element, as libxml2 merges text added next to a text node
# into it, so `node` itself can't be what they're added next to.
replace_with_html <- function(node, html) {
  # Without xml2's default "NOBLANKS", which drops a space between links.
  fragment <- xml2::read_xml(paste0("<x>", html, "</x>"), options = character())
  placeholder <- xml2::xml_add_sibling(node, "x", .where = "before")
  xml2::xml_remove(node)
  for (child in xml2::xml_contents(fragment)) {
    xml2::xml_add_sibling(placeholder, child, .where = "before")
  }
  xml2::xml_remove(placeholder)
}

# ---- inline code ----

# Links inline code that is a reference. Not in headings or `<summary>`, nor
# in another link, except a link around nothing but the code, or one around
# the name before a "()" (roxygen's `\code{\link{foo}()}`): those are
# relinked if it's to the package's own topic, for its tooltip.
link_inline <- function(ctx, doc) {
  bad <- c("h1", "h2", "h3", "h4", "h5", "h6", "summary", "pre")
  xpath <- sprintf(
    ".//code[not(%s)]",
    paste0("ancestor::", bad, collapse = " or ")
  )
  for (code in xml2::xml_find_all(doc, xpath)) {
    text <- xml2::xml_text(code)
    children <- xml2::xml_children(code)
    parent <- xml2::xml_parent(code)
    wrapped <- xml2::xml_name(parent) == "a"
    if (wrapped) {
      # Code in a link: only one around nothing else.
      if (
        length(xml2::xml_children(parent)) != 1 ||
          trimws(xml2::xml_text(parent)) != trimws(text) ||
          length(children) > 0
      ) {
        next
      }
    } else if (length(xml2::xml_find_all(code, "ancestor::a")) > 0) {
      next
    }
    if (
      length(children) > 1 ||
        (length(children) == 1 && xml2::xml_name(children[[1]]) != "a")
    ) {
      next
    }
    ref <- inline_ref(ctx, text)
    if (is.null(ref) || is.na(ref$url)) {
      next
    }
    linked <- wrapped || length(children) == 1
    if (linked && is.na(ref$own %||% NA)) {
      next
    }
    a <- link_html(ctx, html_escape(text), ref$url, ref$own %||% NA)
    if (wrapped) {
      replace_with_html(parent, paste0("<code>", a, "</code>"))
    } else {
      xml2::xml_remove(xml2::xml_contents(code))
      replace_with_html(
        xml2::xml_add_child(code, "x"),
        a
      )
    }
  }
  invisible()
}

# The reference inline code `text` is, as from topic_ref(), or NULL: a call
# like `foo()`, `pkg::foo(x)` or `?foo`, a vignette, a package's `{name}` or
# `library(name)`, or a name of the package's own (see linkable_names()).
inline_ref <- function(ctx, text) {
  if (grepl("^\\{[a-zA-Z][a-zA-Z0-9.]+\\}$", text)) {
    return(list(url = href_package(gsub("[{}]", "", text))))
  }
  exprs <- safe_parse(gsub("\t", " ", text, fixed = TRUE))
  if (length(exprs) != 1) {
    # The package's own `!!` or `%op%`, which don't parse on their own.
    if (text %in% names(ctx$topics)) {
      return(topic_ref(ctx, text))
    }
    return(NULL)
  }
  expr <- exprs[[1]]
  if (is.symbol(expr)) {
    name <- as.character(expr)
    return(if (name %in% ctx$names) topic_ref(ctx, name))
  }
  if (!is.call(expr)) {
    return(NULL)
  }
  fun <- expr[[1]]
  package <- NULL
  if (
    is.call(fun) &&
      length(fun) == 3 &&
      as.character(fun[[1]]) %in% c("::", ":::")
  ) {
    package <- as.character(fun[[2]])
    fun <- fun[[3]]
  }
  if (!is.symbol(fun)) {
    return(NULL)
  }
  name <- as.character(fun)
  args <- as.list(expr)[-1]
  literal <- function(x) is.symbol(x) || is_string(x)
  # Only a call written as one, `name(...)`: not `x = 1` or `~ x`. Or
  # `break` and `next`, which are calls without parentheses.
  call_text <- sub(
    "^[A-Za-z.][A-Za-z0-9._]*:::?",
    "",
    gsub("`", "", trimws(text))
  )
  written_as_call <- startsWith(call_text, paste0(name, "(")) ||
    (call_text %in% c("break", "next") && call_text == name)

  if (name %in% c("::", ":::") && length(args) == 2) {
    return(topic_ref(ctx, as.character(args[[2]]), as.character(args[[1]])))
  }
  if (is.null(package) && name == "?") {
    if (
      length(args) == 1 &&
        is.call(args[[1]]) &&
        as.character(args[[1]][[1]]) %in% c("::", ":::")
    ) {
      return(topic_ref(
        ctx,
        as.character(args[[1]][[3]]),
        as.character(args[[1]][[2]])
      ))
    }
    if (length(args) == 1 && literal(args[[1]])) {
      return(topic_ref(ctx, as.character(args[[1]])))
    }
    if (length(args) == 2) {
      # `package?foo`
      return(topic_ref(ctx, paste0(args[[2]], "-", args[[1]])))
    }
    return(NULL)
  }
  if (
    is.null(package) &&
      name %in% c("library", "require", "requireNamespace") &&
      length(args) == 1 &&
      is.null(names(expr)) &&
      literal(args[[1]])
  ) {
    url <- href_package(as.character(args[[1]]))
    return(
      if (!is.na(url)) list(url = url) else topic_ref(ctx, name, is_fun = TRUE)
    )
  }
  if (is.null(package) && name == "vignette" && length(args) >= 1) {
    call <- tryCatch(match.call(utils::vignette, expr), error = function(e) {
      NULL
    })
    if (
      is.null(call) ||
        !is_string(call$topic) ||
        !(is.null(call$package) || is_string(call$package))
    ) {
      return(NULL)
    }
    return(list(url = href_article(ctx, call$topic, call$package)))
  }
  if (is.null(package) && name == "help" && length(args) >= 1) {
    call <- tryCatch(match.call(utils::help, expr), error = function(e) NULL)
    if (is.null(call)) {
      return(NULL)
    }
    if (literal(call$topic)) {
      pkg <- if (literal(call$package)) as.character(call$package)
      return(topic_ref(ctx, as.character(call$topic), pkg))
    }
    if (is.null(call$topic) && literal(call$package)) {
      return(list(url = package_reference_url(as.character(call$package))))
    }
    return(NULL)
  }
  if (
    !written_as_call || name %in% c("function", "if", "for", "while", "repeat")
  ) {
    return(NULL)
  }
  topic_ref(ctx, name, package, is_fun = TRUE)
}

# ---- links ----

# `name` in `package` (see href_topic()) as `url` and, if it's one of the
# package's own topics, `own`: its alias.
topic_ref <- function(ctx, name, package = NULL, is_fun = FALSE) {
  own <- (is.null(package) || identical(package, ctx$pkg)) &&
    name %in% names(ctx$topics) &&
    ctx$topics[[name]] != "reexports"
  list(
    url = href_topic(ctx, name, package, is_fun = is_fun),
    own = if (own) name
  )
}

# A link to `url` around `html`; to one of the package's own topics (its
# alias `own`), with a tooltip.
link_html <- function(ctx, html, url, own = NA) {
  tip <- if (!is.na(own)) tip_id(ctx, own)
  attrs <- if (!is.null(tip)) {
    sprintf(' class="quartodown-tip-ref" data-quartodown-tip="%s"', tip)
  } else {
    ""
  }
  sprintf('<a%s href="%s">%s</a>', attrs, html_escape(url), html)
}

# The id of the tooltip for the package's own topic `alias` on the page
# `ctx` links, its contents kept for tooltip_payload_html(). NULL without
# tooltips.
tip_id <- function(ctx, alias) {
  entry <- ctx$index[[alias]]
  if (!isTRUE(ctx$tips) || is.null(entry)) {
    return(NULL)
  }
  if (!alias %in% names(ctx$tip_ids)) {
    id <- paste0("t", length(ctx$tip_ids) + 1L)
    ctx$tip_ids[[alias]] <- id
    entry$name <- alias
    ctx$tip_entries[[id]] <- entry
  }
  ctx$tip_ids[[alias]]
}
