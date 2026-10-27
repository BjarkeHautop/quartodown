# ---- running the reference pages' examples ---------------------------------------
#
# Quarto starts a new R process for every page with code, which is most of the
# time a site with many reference pages takes to render. Their examples run
# here instead, all in one R process (inst/scripts/run-examples.R), which
# replaces each page with the Markdown Quarto's knitr engine would have made
# of it, so Quarto only has Markdown left to render. A page that can't be done
# this way (an error, HTML dependencies) stays as it is for Quarto to run, as
# do all of them with settings the script doesn't handle (see
# example_knit_options()). Vignettes are always run by Quarto.
#
# With `freeze = TRUE`, each page's result and figures are kept in
# _quarto/_freeze/quartodown/reference/<page>/ and reused while the page and
# the options are the same, as Quarto's freeze does for the other pages.

run_examples <- function(path, envvars, settings, freeze = FALSE) {
  opts <- example_knit_options(settings)
  pages <- example_pages(path)
  if (is.null(opts) || length(pages) == 0) {
    return(invisible())
  }
  keys <- vapply(pages, example_cache_key, character(1), opts = opts)
  if (freeze) {
    cached <- vapply(
      pages,
      function(page) restore_example_cache(path, page, keys[[page]]),
      logical(1)
    )
    pages <- pages[!cached]
  }
  n <- length(pages)
  if (n == 0) {
    return(invisible())
  }

  cli::cli_progress_step(
    "Running the examples of {n} reference page{?s}",
    msg_done = "Ran the examples of {n} reference page{?s}"
  )
  status <- run_examples_script(pages, opts, envvars)
  if (freeze) {
    done <- pages[status[basename(pages)] %in% "done"]
    for (page in done) {
      write_example_cache(path, page, keys[[page]])
    }
  }
  cli::cli_progress_done()
  invisible(status)
}

# The reference pages with code, as absolute paths, since the script changes
# into each page's folder.
example_pages <- function(path) {
  files <- list.files(
    file.path(project_dir(path), "reference"),
    pattern = "\\.qmd$",
    full.names = TRUE
  )
  has_code <- vapply(
    files,
    function(file) {
      lines <- read_utf8(file)
      any(startsWith(lines, "```{r"))
    },
    logical(1),
    USE.NAMES = FALSE
  )
  normalizePath(files[has_code], winslash = "/")
}

# The options Quarto's knitr engine would run the pages with (see
# knitr_options() in Quarto's share/rmd/execute.R), from `settings`, the
# generated _quarto.yml. NULL if it has settings that change how Quarto
# runs code beyond these, leaving the pages to Quarto.
example_knit_options <- function(settings) {
  html <- settings$format$html
  handled <- c(
    "freeze",
    "fig-width",
    "fig-height",
    "fig-asp",
    "fig-format",
    "fig-dpi",
    "warning",
    "error"
  )
  unhandled <- c(
    "eval",
    "echo",
    "output",
    "include",
    "cache",
    "enabled",
    "keep-hidden",
    "df-print",
    "r-options"
  )
  if (
    length(setdiff(names(settings$execute), handled)) > 0 ||
      any(unhandled %in% names(html)) ||
      !is.null(settings$`r-options`) ||
      !is.null(settings$engine) ||
      length(setdiff(names(settings$knitr), "opts_chunk")) > 0 ||
      length(setdiff(names(settings$knitr$opts_chunk), example_chunk_options)) >
        0
  ) {
    return(NULL)
  }

  # A format's setting overrides the project's.
  value <- function(name, default = NULL) {
    if (!is.null(html[[name]])) {
      html[[name]]
    } else if (!is.null(settings$execute[[name]])) {
      settings$execute[[name]]
    } else {
      default
    }
  }
  format <- value("fig-format", "retina")
  if (!format %in% c("retina", "png", "svg", "jpeg")) {
    return(NULL)
  }
  warning <- isTRUE(value("warning", TRUE))
  opts_chunk <- list(
    fig.width = value("fig-width", 7),
    fig.height = value("fig-height", 5),
    fig.asp = value("fig-asp"),
    dev = if (format == "retina") "png" else format,
    fig.retina = if (format == "retina") 2,
    dpi = value("fig-dpi", 96),
    eval = TRUE,
    error = isTRUE(value("error", FALSE)),
    echo = TRUE,
    warning = warning,
    message = warning,
    include = TRUE,
    comment = NA,
    ft.shadow = FALSE
  )
  opts_chunk <- utils::modifyList(
    Filter(Negate(is.null), opts_chunk),
    as.list(settings$knitr$opts_chunk)
  )
  comment <- opts_chunk$comment
  if (isTRUE(opts_chunk$collapse) && (is.null(comment) || is.na(comment))) {
    opts_chunk$comment <- "##"
  }
  list(
    opts_chunk = opts_chunk,
    opts_knit = list(
      quarto.version = 1,
      rmarkdown.pandoc.to = "html",
      rmarkdown.version = 3,
      rmarkdown.runtime = "static"
    )
  )
}

# The `knitr: opts_chunk:` settings run-examples.R handles as Quarto's hooks
# would.
example_chunk_options <- c(
  "collapse",
  "comment",
  "fig.width",
  "fig.height",
  "fig.asp",
  "fig.align",
  "fig.alt",
  "fig.retina",
  "out.width",
  "out.height",
  "out.extra",
  "dpi",
  "dev",
  "dev.args",
  "warning",
  "message",
  "error",
  "results",
  "tidy",
  "tidy.opts",
  "highlight",
  "strip.white",
  "R.options"
)

# Runs run-examples.R on `pages` with the package's `envvars` (from
# install_envvars() or load_all_envvars()), as Quarto would run them. Returns
# each page's status, named by its file name: "done", or why it was left for
# Quarto.
run_examples_script <- function(pages, opts, envvars) {
  job <- tempfile("quartodown-examples-", fileext = ".rds")
  status <- tempfile("quartodown-examples-", fileext = ".tsv")
  on.exit(unlink(c(job, status)))
  saveRDS(c(list(pages = pages, status = status), opts), job)

  # As in run_quarto_render().
  local_envvars(c(envvars, R_CLI_NUM_COLORS = "256"))
  rscript <- if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  processx::run(
    file.path(R.home("bin"), rscript),
    c(system.file("scripts", "run-examples.R", package = "quartodown"), job),
    wd = dirname(pages[[1]]),
    error_on_status = FALSE
  )

  lines <- if (file.exists(status)) {
    read_utf8(status)
  } else {
    character()
  }
  parts <- strsplit(lines, "\t", fixed = TRUE)
  structure(
    vapply(parts, function(x) x[2], character(1)),
    names = vapply(parts, function(x) x[1], character(1))
  )
}

# ---- freeze -----------------------------------------------------------------------

example_cache_dir <- function(path, page) {
  file.path(
    project_dir(path),
    "_freeze",
    "quartodown",
    "reference",
    strip_ext(basename(page))
  )
}

# What a page's result depends on: its source and the knitr options.
example_cache_key <- function(page, opts) {
  file <- tempfile()
  on.exit(unlink(file))
  write_utf8(c(read_utf8(page), deparse(opts)), file)
  unname(tools::md5sum(file))
}

# Puts the cached result of `page` in its place if it was made from the same
# `key`. Returns whether it did.
restore_example_cache <- function(path, page, key) {
  dir <- example_cache_dir(path, page)
  key_file <- file.path(dir, "key")
  if (!file.exists(key_file) || !identical(readLines(key_file), key)) {
    return(FALSE)
  }
  file.copy(file.path(dir, "page.qmd"), page, overwrite = TRUE)
  figures <- paste0(strip_ext(basename(page)), "_files")
  if (dir.exists(file.path(dir, figures))) {
    copy_dir(file.path(dir, figures), file.path(dirname(page), figures))
  }
  TRUE
}

write_example_cache <- function(path, page, key) {
  dir <- example_cache_dir(path, page)
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE)
  writeLines(key, file.path(dir, "key"))
  file.copy(page, file.path(dir, "page.qmd"))
  figures <- paste0(strip_ext(basename(page)), "_files")
  if (dir.exists(file.path(dirname(page), figures))) {
    copy_dir(file.path(dirname(page), figures), file.path(dir, figures))
  }
  invisible()
}
