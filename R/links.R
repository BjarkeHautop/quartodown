# ---- links to topics, articles and packages ----------------------------------
#
# Where a reference in the site's code to a function, help topic, vignette or
# package links to: the package's own pages, another package's pkgdown site
# (found through the pkgdown.yml at its root, see package_site()), or
# rdrr.io. Only installed packages' topics and vignettes are linked, as their
# help pages' names are only known then; a package that isn't installed links
# to its home page, see cran_package_urls().

# What a page's links are resolved against: the package `pkg`, its `topics`
# (alias -> Rd name), the `names` they can be linked by as values (see
# linkable_names()) and its `articles` (name -> path from the site's root,
# see site_articles()), the paths from the page to its reference pages
# (`topic_path`) and to the site's root (`root`), and the packages the page
# attaches, which grow as its code is linked (see attach_packages()). With
# `tips`, links to the package's own topics get a tooltip from `index` (see
# build_topic_index()); the page's are kept in `tip_entries`.
link_context <- function(
  pkg = NULL,
  topics = character(),
  names = character(),
  articles = character(),
  topic_path = "reference/",
  root = "",
  index = list(),
  tips = FALSE
) {
  ctx <- new.env(parent = emptyenv())
  ctx$pkg <- pkg
  ctx$topics <- topics
  ctx$names <- names
  ctx$articles <- articles
  ctx$topic_path <- topic_path
  ctx$root <- root
  ctx$index <- index
  ctx$tips <- tips
  ctx$attached <- character()
  ctx$tip_ids <- character()
  ctx$tip_entries <- list()
  ctx
}

# The package's vignettes, as linked from its site: name -> path from the
# site's root. As in write_pkgdown_yml().
site_articles <- function(path) {
  vignettes <- strip_ext(find_vignettes(path))
  structure(
    paste0("vignettes/", vignettes, ".html"),
    names = basename(vignettes)
  )
}

# The packages R attaches before any `library()` call, searched last.
default_packages <- c(
  "datasets",
  "utils",
  "grDevices",
  "graphics",
  "stats",
  "base"
)

base_packages <- c(
  "base",
  "compiler",
  "datasets",
  "graphics",
  "grDevices",
  "grid",
  "methods",
  "parallel",
  "splines",
  "stats",
  "stats4",
  "tcltk",
  "tools",
  "utils"
)

# Adds `packages`, which a page attaches with `library()`, to the packages
# its later calls are looked up in, with the packages they attach in turn:
# their `Depends`, and the core of the tidyverse.
attach_packages <- function(ctx, packages) {
  packages <- packages[nzchar(packages)]
  if ("tidyverse" %in% packages && is_installed("tidyverse")) {
    packages <- union(packages, getNamespace("tidyverse")$core)
  }
  depends <- unlist(lapply(packages, package_depends))
  ctx$attached <- union(union(packages, depends), ctx$attached)
  invisible(ctx)
}

package_depends <- function(package) {
  meta <- system.file("Meta", "package.rds", package = package)
  if (meta == "") {
    return(character())
  }
  setdiff(names(readRDS(meta)$Depends), "R")
}

# ---- topics -----------------------------------------------------------------

# The URL of help topic `topic` in `package`, or NA. Without a `package`, the
# package's own topics come first, then those of the attached packages; with
# `is_fun`, a function's only (it must be exported).
href_topic <- function(ctx, topic, package = NULL, is_fun = FALSE) {
  if (!is_string(topic) || is.na(topic)) {
    return(NA_character_)
  }
  own <- is.null(package) || identical(package, ctx$pkg)
  if (own && topic %in% names(ctx$topics)) {
    rdname <- ctx$topics[[topic]]
    if (rdname == "reexports" && is_installed(ctx$pkg %||% "")) {
      url <- href_reexport(topic, ctx$pkg)
      if (!is.na(url)) {
        return(url)
      }
    }
    return(paste0(ctx$topic_path, rdname, ".html"))
  }
  if (!is.null(package)) {
    return(if (own) NA_character_ else href_topic_in(topic, package))
  }
  for (package in c(ctx$attached, default_packages)) {
    if (!is_installed(package) || (is_fun && !is_exported(topic, package))) {
      next
    }
    if (!is.null(find_rdname(package, topic))) {
      return(href_topic_in(topic, package))
    }
  }
  NA_character_
}

# The URL of `topic` in the installed package `package`, or NA.
href_topic_in <- function(topic, package) {
  if (!is_installed(package)) {
    return(NA_character_)
  }
  rdname <- find_rdname(package, topic)
  if (is.null(rdname)) {
    return(NA_character_)
  }
  if (is_reexported(topic, package)) {
    return(href_reexport(topic, package))
  }
  paste0(package_reference_url(package), "/", rdname, ".html")
}

# The Rd file documenting `topic` in the installed package `package`, or
# NULL.
find_rdname <- function(package, topic) {
  aliases <- cached(paste0("aliases:", package), function() {
    path <- system.file("help", "aliases.rds", package = package)
    if (path == "") character() else readRDS(path)
  })
  if (topic %in% names(aliases)) aliases[[topic]] else NULL
}

is_exported <- function(name, package) {
  exports <- cached(paste0("exports:", package), function() {
    tryCatch(getNamespaceExports(package), error = function(e) character())
  })
  name %in% exports
}

is_reexported <- function(name, package) {
  if (package == "base") {
    return(FALSE)
  }
  imports <- parent.env(asNamespace(package))
  exists(name, envir = imports, inherits = FALSE) && is_exported(name, package)
}

# A topic `package` re-exports links to the package it comes from.
href_reexport <- function(topic, package) {
  ns <- asNamespace(package)
  obj <- get0(topic, envir = ns)
  if (is.null(obj)) {
    return(NA_character_)
  }
  from <- if (is.primitive(obj)) {
    "base"
  } else if (is.function(obj) && isNamespace(environment(obj))) {
    getNamespaceName(environment(obj))
  } else {
    imports <- getNamespaceImports(ns)
    imports <- imports[names(imports) != ""]
    has <- vapply(imports, function(x) topic %in% x, logical(1))
    utils::tail(names(imports)[has], 1)
  }
  if (length(from) == 0 || identical(unname(from), package)) {
    return(NA_character_)
  }
  href_topic_in(topic, unname(from))
}

# ---- articles ---------------------------------------------------------------

# The URL of the vignette `article` of `package`, or NA. Without a
# `package`, the package's own, then those of the attached packages.
href_article <- function(ctx, article, package = NULL) {
  if (!is_string(article)) {
    return(NA_character_)
  }
  own <- is.null(package) || identical(package, ctx$pkg)
  if (own && article %in% names(ctx$articles)) {
    return(paste0(ctx$root, ctx$articles[[article]]))
  }
  if (is.null(package)) {
    package <- find_vignette_package(ctx, article)
  } else if (own) {
    return(NA_character_)
  }
  if (is.null(package) || !is_installed(package)) {
    return(NA_character_)
  }
  path <- package_articles(package)[article]
  if (is.na(path)) {
    return(NA_character_)
  }
  site <- package_site(package)
  if (!is.null(site$article)) {
    paste0(site$article, "/", path)
  } else if (is_bioc_package(package)) {
    paste0(
      "https://bioconductor.org/packages/release/bioc/vignettes/",
      package,
      "/inst/doc/",
      path
    )
  } else {
    paste0(
      "https://cran.rstudio.com/web/packages/",
      package,
      "/vignettes/",
      path
    )
  }
}

find_vignette_package <- function(ctx, article) {
  for (package in ctx$attached) {
    if (article %in% names(package_articles(package))) {
      return(package)
    }
  }
  NULL
}

# The articles of the installed package `package`: name -> path from its
# site's articles, or else its vignettes' file names.
package_articles <- function(package) {
  articles <- package_site(package)$articles
  if (!is.null(articles)) {
    return(unlist(articles))
  }
  meta <- system.file("Meta", "vignette.rds", package = package)
  if (meta == "") {
    return(character())
  }
  meta <- readRDS(meta)
  structure(meta$PDF, names = tools::file_path_sans_ext(meta$File))
}

is_bioc_package <- function(package) {
  views <- utils::packageDescription(package, fields = "biocViews")
  !is.na(views) && nzchar(views)
}

# ---- packages ---------------------------------------------------------------

# The URL of the package `package`'s home page, or NA.
href_package <- function(package) {
  urls <- if (is_installed(package)) {
    package_urls(package)
  } else {
    cran_package_urls(package)
  }
  if (length(urls) == 0) NA_character_ else urls[[1]]
}

# The base URL of the installed package `package`'s reference pages: its
# site's, or else rdrr.io's.
package_reference_url <- function(package) {
  if (package %in% base_packages) {
    return(paste0("https://rdrr.io/r/", package))
  }
  package_site(package)$reference %||%
    paste0("https://rdrr.io/pkg/", package, "/man")
}

# The `URL`s in the installed package `package`'s DESCRIPTION.
package_urls <- function(package) {
  parse_urls(utils::packageDescription(package, fields = "URL"))
}

# The `URL`s in the DESCRIPTION of `package`, which isn't installed, or else
# its CRAN page; none if it isn't on CRAN. Only this package's DESCRIPTION is
# downloaded, not CRAN's whole package database, from CRAN's CDN, as
# cran.r-project.org can take over 10 seconds per file.
cran_package_urls <- function(package) {
  if (!grepl("^[A-Za-z][A-Za-z0-9.]*$", package)) {
    return(character())
  }
  description <- cached_download(
    "cran",
    paste0(
      "https://cloud.r-project.org/web/packages/",
      package,
      "/DESCRIPTION"
    ),
    fetch_cran_description
  )
  if (is.null(description)) {
    return(character())
  }
  urls <- parse_urls(unname(description["URL"]))
  if (length(urls) > 0) {
    urls
  } else {
    paste0("https://cran.r-project.org/package=", package)
  }
}

parse_urls <- function(url) {
  if (is.na(url)) {
    return(character())
  }
  urls <- strsplit(trimws(url), "[,[:space:]]+")[[1]]
  urls <- urls[grepl("^http", urls)]
  # rOpenSci's packages all have their site there.
  sub(
    "^https?://github.com/ropensci/(\\w+).*$",
    "https://docs.ropensci.org/\\1",
    urls
  )
}

# The pkgdown site of the installed package `package`, from the
# pkgdown.yml it's installed with, or else at the root of one of its URLs:
# `reference` and `article`, its pages' base URLs, and `articles`. NULL
# without one.
package_site <- function(package) {
  cached(paste0("site:", package), function() {
    local <- system.file("pkgdown.yml", package = package)
    if (local != "") {
      return(site_from_yml(yaml::read_yaml(local), NULL))
    }
    for (url in package_urls(package)) {
      url <- sub("/+$", "", url)
      yml <- pkgdown_yml(url)
      if (is.list(yml)) {
        return(site_from_yml(yml, url))
      }
    }
    NULL
  })
}

site_from_yml <- function(yml, url) {
  urls <- yml$urls
  list(
    reference = urls$reference %||%
      if (!is.null(url)) paste0(url, "/reference"),
    article = urls$article %||% if (!is.null(url)) paste0(url, "/articles"),
    articles = if (!is.null(yml$articles)) unlist(yml$articles)
  )
}

# The pkgdown.yml at the site `url`, or NULL.
pkgdown_yml <- function(url) {
  cached_download("sites", url, fetch_pkgdown_yml)
}

# What `fetch(url)` downloads, or NULL. Kept for a week in the cache's `kind`
# folder, shared by the build's workers and later builds, so it's fetched once.
# A failed download (rather than a missing file) isn't cached, so the next
# build tries again; meanwhile a week-old copy is still used. Without a
# network (see network_down()), nothing is downloaded.
cached_download <- function(kind, url, fetch) {
  dir <- file.path(link_cache_dir(), kind)
  file <- file.path(dir, paste0(gsub("[^A-Za-z0-9]+", "_", url), ".rds"))
  entry <- if (file.exists(file)) readRDS(file)
  if (!is.null(entry) && difftime(Sys.time(), entry$time, units = "days") < 7) {
    return(entry$value)
  }
  if (network_down()) {
    return(entry$value)
  }
  result <- fetch(url)
  if (result$failed) {
    note_failed_download(url)
    return(entry$value)
  }
  link_memo$failures <- 0
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  # Written whole, then moved, as another worker may read it.
  tmp <- tempfile(tmpdir = dir)
  saveRDS(list(time = Sys.time(), value = result$value), tmp)
  file.rename(tmp, file)
  result$value
}

# Whether downloads failed three times in a row, in this R session or, in
# the last 10 minutes, another (such as another of the build's workers): the
# network is then taken to be down, so a build without one doesn't wait for
# every site to time out. A single site that's down isn't enough.
network_down <- function() {
  marker <- file.path(link_cache_dir(), "offline")
  isTRUE(link_memo$offline) ||
    (file.exists(marker) &&
      difftime(Sys.time(), file.mtime(marker), units = "mins") < 10)
}

note_failed_download <- function(url) {
  link_memo$failures <- (link_memo$failures %||% 0) + 1
  if (link_memo$failures < 3) {
    return(invisible())
  }
  # Another worker may have warned already.
  warned <- network_down()
  link_memo$offline <- TRUE
  dir <- link_cache_dir()
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(dir, "offline"))
  if (warned) {
    return(invisible())
  }
  cli::cli_warn(c(
    "Couldn't download other packages' details, last from {.url {url}}.",
    "i" = "Their functions not looked up before link to {.url https://rdrr.io} instead of their sites, and packages that aren't installed aren't linked, until a build with a network connection."
  ))
}

link_cache_dir <- function() {
  getOption("quartodown.cache_dir", tools::R_user_dir("quartodown", "cache"))
}

# Removes from the cache what was downloaded more than a month ago (what's
# still in use is downloaded again every week), and an expired offline marker
# (see network_down()), so the cache doesn't keep growing.
prune_link_cache <- function() {
  dir <- link_cache_dir()
  age <- function(files, units) {
    difftime(Sys.time(), file.mtime(files), units = units)
  }
  files <- list.files(file.path(dir, c("sites", "cran")), full.names = TRUE)
  unlink(files[age(files, "days") > 30])
  marker <- file.path(dir, "offline")
  if (file.exists(marker) && age(marker, "mins") >= 10) {
    unlink(marker)
  }
  invisible()
}

fetch_pkgdown_yml <- function(url) {
  fetch_file(paste0(url, "/pkgdown.yml"), function(path) {
    yml <- yaml::read_yaml(path)
    if (is.list(yml)) yml
  })
}

# The fields of the DESCRIPTION at `url`, as a named character vector.
fetch_cran_description <- function(url) {
  fetch_file(url, function(path) read.dcf(path)[1, ])
}

# `value`, what `parse` makes of the downloaded file at `url` (NULL without
# one, or if it can't be parsed), and whether the download `failed` for another
# reason than the file not existing.
fetch_file <- function(url, parse) {
  status <- tryCatch(
    attr(curlGetHeaders(url, timeout = 10), "status"),
    error = function(e) NA
  )
  if (!identical(status, 200L)) {
    missing <- !is.na(status) && status >= 400 && status < 500
    return(list(value = NULL, failed = !missing))
  }
  path <- tempfile()
  on.exit(unlink(path))
  old <- options(timeout = 10)
  on.exit(options(old), add = TRUE)
  ok <- tryCatch(
    utils::download.file(url, path, mode = "wb", quiet = TRUE) == 0,
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
  if (!ok) {
    return(list(value = NULL, failed = TRUE))
  }
  value <- tryCatch(suppressWarnings(parse(path)), error = function(e) NULL)
  list(value = value, failed = FALSE)
}

# ---- helpers ----------------------------------------------------------------

link_memo <- new.env(parent = emptyenv())

# `fn()`, computed once per R session for each `key`.
cached <- function(key, fn) {
  if (!exists(key, envir = link_memo, inherits = FALSE)) {
    assign(key, fn(), envir = link_memo)
  }
  get(key, envir = link_memo, inherits = FALSE)
}

is_installed <- function(package) nzchar(system.file(package = package))
