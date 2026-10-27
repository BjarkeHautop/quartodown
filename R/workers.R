# Building with `workers`: the reference pages are written and the rendered
# pages post-processed by mirai daemons (map_workers()), and Quarto renders the
# pages in up to `workers` processes, each on a copy of the project (a shard).

workers_profile <- "quartodown"

# Starts `workers` mirai daemons for map_workers(), in a compute profile of
# their own, so daemons the user set aren't touched, until the calling
# function exits. They run this quartodown: the same installed one, or, while
# developing it, the same source loaded with pkgload.
local_workers <- function(workers, envir = parent.frame()) {
  if (workers <= 1) {
    return(invisible())
  }
  if (
    !requireNamespace("mirai", quietly = TRUE) ||
      utils::packageVersion("mirai") < "2.4.1"
  ) {
    cli::cli_abort(c(
      "Building with {.code workers > 1} needs the {.pkg mirai} package (2.4.1 or later).",
      "i" = "Install it with {.code install.packages(\"mirai\")}."
    ))
  }
  mirai::daemons(workers, .compute = workers_profile)
  defer <- function() mirai::daemons(0, .compute = workers_profile)
  do.call(on.exit, list(substitute(defer()), add = TRUE), envir = envir)

  ns_path <- getNamespaceInfo("quartodown", "path")
  dev <- isNamespaceLoaded("pkgload") && pkgload::is_dev_package("quartodown")
  setup <- mirai::everywhere(
    if (dev) {
      pkgload::load_all(ns_path, export_all = FALSE, quiet = TRUE)
    } else {
      loadNamespace("quartodown", lib.loc = dirname(ns_path))
    },
    dev = dev,
    ns_path = ns_path,
    .compute = workers_profile
  )
  setup[mirai::.stop]
  invisible()
}

# lapply(x, fn, ...), by the daemons of local_workers() if they run. `fn`
# must be a function of the package, so that it is sent by name rather than
# with the environment it was made in. Warnings and messages, which mirai
# drops, are signalled again here, in order.
map_workers <- function(x, fn, ...) {
  if (
    length(x) <= 1 ||
      !isNamespaceLoaded("mirai") ||
      !mirai::daemons_set(.compute = workers_profile)
  ) {
    return(lapply(x, fn, ...))
  }
  results <- mirai::mirai_map(
    x,
    worker_call,
    .args = list(fn = fn, args = list(...), libpaths = .libPaths()),
    .compute = workers_profile
  )[mirai::.stop]
  for (result in results) {
    for (cond in result$conditions) {
      if (inherits(cond, "warning")) warning(cond) else message(cond)
    }
  }
  lapply(results, `[[`, "value")
}

# `fn(x, ...)` in a daemon, with this session's library path (which
# with_libpaths() may have changed), and the warnings and messages it signals.
worker_call <- function(x, fn, args, libpaths) {
  old <- .libPaths()
  on.exit(.libPaths(old))
  .libPaths(libpaths)
  conditions <- list()
  keep <- function(cond) {
    conditions[[length(conditions) + 1L]] <<- cond
    restart <- if (inherits(cond, "warning")) {
      "muffleWarning"
    } else {
      "muffleMessage"
    }
    invokeRestart(restart)
  }
  value <- withCallingHandlers(
    do.call(fn, c(list(x), args)),
    warning = keep,
    message = keep
  )
  list(value = value, conditions = conditions)
}

# ---- Quarto shards ------------------------------------------------------------

# The project's pages, split into up to `workers` shards: those with code
# cells first, dealt out in turn, so the slow pages are spread out. NULL, for
# one Quarto process, unless `pages` are all files.
shard_pages <- function(dir, pages, workers) {
  if (
    workers <= 1 ||
      length(pages) <= 1 ||
      !all(file.exists(file.path(dir, pages)))
  ) {
    return(NULL)
  }
  has_code <- vapply(
    file.path(dir, pages),
    function(file) any(grepl("^\\s*```+\\s*\\{", read_utf8(file))),
    logical(1)
  )
  pages <- c(pages[has_code], pages[!has_code])
  unname(split(pages, rep_len(seq_len(workers), length(pages))))
}

# Why Quarto must render `pages` in one process, or NULL. A shard renders
# only its own pages, and Quarto reads the other pages for a listing page and
# for a navbar or sidebar item's text (from the page it links when it has
# none, or for every page of an `auto` sidebar), so those would come out
# without them.
one_process_reason <- function(dir, pages, settings) {
  metadata <- list.files(
    dir,
    pattern = "^_metadata\\.ya?ml$",
    recursive = TRUE,
    full.names = TRUE
  )
  for (file in c(file.path(dir, pages), metadata)) {
    lines <- read_utf8(file)
    if (!grepl("^_metadata", basename(file))) {
      lines <- split_front_matter(lines)$front_matter
    }
    if (any(grepl("^listing\\s*:", lines))) {
      name <- sub(paste0(dir, "/"), "", file, fixed = TRUE)
      return(sprintf("{.file %s} has a listing", cli_escape(name)))
    }
  }
  website <- settings$website
  if (has_untitled_link(list(website$navbar, website$sidebar), pages)) {
    return("a navbar or sidebar item has no {.field text}")
  }
  NULL
}

# Does the navbar or sidebar settings `x` have an item linking one of `pages`
# without its own text: an item whose `href` (or `file`) has no `text` or
# `section`, a bare path in a list of items, or `auto`?
has_untitled_link <- function(x, pages) {
  is_page <- function(href) {
    is_string(href) && sub("^\\.?/", "", href) %in% pages
  }
  if (!is.list(x)) {
    return(FALSE)
  }
  if (!is.null(x[["auto"]]) || identical(x[["contents"]], "auto")) {
    return(TRUE)
  }
  href <- if (is.null(x[["href"]])) x[["file"]] else x[["href"]]
  if (is_page(href) && is.null(x[["text"]]) && is.null(x[["section"]])) {
    return(TRUE)
  }
  # Lists of items: unnamed, or (read from YAML) a vector of paths.
  items <- if (is.null(names(x))) {
    x
  } else {
    x[names(x) %in% c("left", "right", "menu", "contents")]
  }
  paths <- unlist(Filter(is.character, items))
  if (any(vapply(paths, is_page, logical(1)))) {
    return(TRUE)
  }
  any(vapply(Filter(is.list, x), has_untitled_link, logical(1), pages = pages))
}

shards_dir <- "_shards"

# A copy of the project in its shards_dir for each shard, rendering only its
# pages into a site of its own in `scratch`. The copies stay inside the
# package, for code that finds it from the working directory (such as
# testthat's snapshots). Returns the copies' folders, with the sites in their
# "site" attribute.
write_shard_projects <- function(path, settings, shards, scratch) {
  dir <- project_dir(path)
  files <- list.files(dir, all.files = TRUE, no.. = TRUE)
  files <- files[!files %in% c(".quarto", shards_dir)]
  projects <- file.path(dir, shards_dir, seq_along(shards))
  sites <- file.path(scratch, paste0("site-", seq_along(shards)))
  for (k in seq_along(shards)) {
    dir.create(projects[[k]], recursive = TRUE)
    file.copy(file.path(dir, files), projects[[k]], recursive = TRUE)
    settings$project$render <- as.list(shards[[k]])
    settings$project$`output-dir` <- normalizePath(
      sites[[k]],
      winslash = "/",
      mustWork = FALSE
    )
    write_quarto_yml(settings, projects[[k]])
  }
  structure(projects, site = sites)
}

# Copies the shards' sites into `dest`, and their freeze results back into
# the project. A shard's site has its pages and the files every page uses
# (site_libs/, images), but its index.html redirects to its first page unless
# it rendered the home page, and other shards' sources are copied into it as
# resources; neither is kept. Its search index and sitemap list only its
# pages, so they're joined.
merge_shard_sites <- function(path, projects, shards, dest) {
  sites <- attr(projects, "site")
  pages <- unlist(shards)
  for (k in seq_along(sites)) {
    files <- list.files(sites[[k]], recursive = TRUE, all.files = TRUE)
    others <- setdiff(pages, shards[[k]])
    skip <- c(pages, html_path(others), "search.json", "sitemap.xml")
    copy_dir(sites[[k]], dest, files[!files %in% skip])
    for (page in shards[[k]]) {
      file <- file.path(dest, html_path(page))
      if (file.exists(file)) {
        html <- paste(read_utf8(file), collapse = "\n")
        write_utf8(link_shard_pages(html, page, others), file)
      }
    }
    freeze <- file.path(projects[[k]], "_freeze")
    if (dir.exists(freeze)) {
      copy_dir(freeze, file.path(project_dir(path), "_freeze"))
    }
  }
  join_search_json(file.path(sites, "search.json"), dest)
  join_sitemaps(file.path(sites, "sitemap.xml"), dest)
  invisible()
}

# The output path of the page `page`.
html_path <- function(page) sub("\\.[^./]+$", ".html", page)

# Quarto links a page's source (`../NEWS.md`) to the page only when it
# renders both, so a shard leaves links to `others`, the other shards' pages,
# to their sources. `page` is the page `html` is from.
link_shard_pages <- function(html, page, others) {
  gsub_fn(html, 'href="([^":?#]+)([?#][^"]*)?"', function(match, groups) {
    href <- utils::URLdecode(groups[1])
    # A site-absolute link (as on the 404 page) is from the site's root.
    from <- if (startsWith(href, "/")) "." else dirname(page)
    target <- fs::path_norm(file.path(from, sub("^/+", "", href)))
    if (!target %in% others) {
      return(match)
    }
    rest <- if (is.na(groups[2])) "" else groups[2]
    sprintf('href="%s%s"', html_path(groups[1]), rest)
  })
}

# Quarto writes search.json as a JSON array of an entry per section.
join_search_json <- function(files, dest) {
  files <- files[file.exists(files)]
  if (length(files) == 0) {
    return(invisible())
  }
  entries <- vapply(
    files,
    function(file) {
      json <- trimws(paste(read_utf8(file), collapse = "\n"))
      trimws(substr(json, 2, nchar(json) - 1))
    },
    character(1)
  )
  entries <- entries[nzchar(entries)]
  write_utf8(
    c("[", paste(entries, collapse = ",\n"), "]"),
    file.path(dest, "search.json")
  )
}

join_sitemaps <- function(files, dest) {
  files <- files[file.exists(files)]
  if (length(files) == 0) {
    return(invisible())
  }
  sitemap <- xml2::read_xml(files[[1]])
  for (file in files[-1]) {
    for (url in xml2::xml_children(xml2::read_xml(file))) {
      xml2::xml_add_child(sitemap, url)
    }
  }
  xml2::write_xml(sitemap, file.path(dest, "sitemap.xml"))
}
