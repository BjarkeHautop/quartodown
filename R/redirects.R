# ---- redirects ----------------------------------------------------------------
#
# Pages at old addresses that send visitors on to new ones, listed under
# _quartodown.yml's `redirects:` as pairs of paths.
# Quarto's `aliases:` does the same, but needs the target page's front matter,
# which generated pages such as the reference pages don't take.

write_redirects <- function(path, config = list()) {
  docs <- site_dir(path, config)
  redirects <- config_redirects(config)
  for (from in names(redirects)) {
    if (file.exists(file.path(docs, from))) {
      config_warn(
        "redirects",
        "redirects {.file {from}}, which is a page of the site. Not redirecting it."
      )
      redirects <- redirects[names(redirects) != from]
    } else if (!is_url(redirects[[from]])) {
      target <- sub("#.*", "", redirects[[from]])
      if (!file.exists(file.path(docs, target))) {
        config_warn(
          "redirects",
          "redirects {.file {from}} to {.file {target}}, which isn't a page of the site."
        )
      }
    }
  }

  for (from in names(redirects)) {
    file <- file.path(docs, from)
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    write_utf8(redirect_page(redirect_href(from, redirects[[from]])), file)
  }
  invisible(redirects)
}

# `redirects:` as a named character vector: targets by the path they're
# redirected from, both relative to the site's root.
config_redirects <- function(config = list()) {
  pairs <- config$redirects
  ok <- is.list(pairs) &&
    all(vapply(
      pairs,
      function(pair) is.character(pair) && length(pair) == 2,
      logical(1)
    ))
  if (!ok) {
    if (!is.null(pairs)) {
      config_abort(
        "redirects",
        c(
          "must be a list of pairs of paths.",
          "i" = 'For example, {.code - ["reference/old.html", "reference/new.html"]}.'
        )
      )
    }
    return(character())
  }
  from <- sub("^/+", "", vapply(pairs, `[[`, character(1), 1))
  to <- vapply(pairs, `[[`, character(1), 2)
  to[!is_url(to)] <- sub("^/+", "", to[!is_url(to)])
  stats::setNames(to, from)
}

# `to`, as a link from the page at `from`.
redirect_href <- function(from, to) {
  if (is_url(to)) {
    return(to)
  }
  depth <- length(strsplit(from, "/", fixed = TRUE)[[1]]) - 1
  paste0(strrep("../", depth), to)
}

is_url <- function(x) grepl("^[a-z][a-z0-9+.-]*:", x, ignore.case = TRUE)

# Keeps the address's `#anchor`, unless the target has its own.
redirect_page <- function(href) {
  js <- js_string(href)
  href <- html_escape(href)
  c(
    "<!DOCTYPE html>",
    "<html lang=\"en\">",
    "<head>",
    "<meta charset=\"utf-8\">",
    "<title>Redirect</title>",
    "<meta name=\"robots\" content=\"noindex\">",
    sprintf("<meta http-equiv=\"refresh\" content=\"0; url=%s\">", href),
    "<script>",
    sprintf("var target = %s;", js),
    "if (target.indexOf(\"#\") < 0) target += window.location.hash;",
    "window.location.replace(target);",
    "</script>",
    "</head>",
    "<body>",
    sprintf("<p>This page has moved to <a href=\"%s\">%s</a>.</p>", href, href),
    "</body>",
    "</html>"
  )
}

# `x` as a JavaScript string literal, safe inside a <script> element.
js_string <- function(x) {
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
  x <- gsub("\"", "\\\"", x, fixed = TRUE)
  x <- gsub("<", "\\u003c", x, fixed = TRUE)
  paste0("\"", x, "\"")
}
