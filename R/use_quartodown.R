#' Set up a package to use quartodown
#'
#' Writes `_quartodown/_quartodown.yml`, the configuration file
#' [build_site()] reads. It starts with a `reference:` field listing every
#' function currently documented in `<path>/man/*.Rd` in one "All functions"
#' section. Also adds the `.gitignore`/`.Rbuildignore` entries the build
#' needs (see Details) if they're not already there.
#'
#' In a package with a pkgdown configuration (`_pkgdown.yml`, also in
#' `pkgdown/`), it's translated instead, with any files it needs, such as
#' `_quartodown/extra.scss`, and the settings that couldn't be translated are
#' listed. See "Coming from pkgdown" in
#' `vignette("customizing", package = "quartodown")`.
#'
#' An existing `_quartodown.yml`, or file in `_quartodown/`, is never
#' overwritten.
#'
#' @details
#' `use_quartodown()` adds the following lines to `.gitignore` and
#' `.Rbuildignore` (only the ones not already present in each file):
#'
#' ```
#' # .gitignore
#' _quarto/*
#' !_quarto/_freeze/
#'
#' # .Rbuildignore
#' ^docs$
#' ^_quarto$
#' ^_quartodown$
#' ```
#'
#' `_quarto/` is the Quarto project [build_site()] generates and renders
#' into `docs/`. Its `_freeze/` directory holds the results of code run with
#' `build_site(freeze = TRUE)`, which can be committed so later builds, e.g.
#' on CI, reuse them. `_quartodown/` holds the site's configuration and any
#' files it uses, such as `extra.scss`.
#'
#' @param path Path to the target package root. Defaults to the current
#'   directory.
#' @return Invisibly, the path to `_quartodown/_quartodown.yml`.
#' @export
#'
#' @examples
#' pkg <- tempfile("examplepkg")
#' dir.create(pkg)
#' write(
#'   "Package: examplepkg\nVersion: 0.0.0.9000",
#'   file.path(pkg, "DESCRIPTION")
#' )
#' use_quartodown(pkg)
#' readLines(file.path(pkg, "_quartodown", "_quartodown.yml"))
#' unlink(pkg, recursive = TRUE)
use_quartodown <- function(path = ".") {
  check_pkg_root(path)

  config_dest <- file.path(path, config_file)
  pkgdown_file <- find_pkgdown_config(path)
  if (file.exists(config_dest)) {
    cli::cli_alert_info("Skipping {.path {config_dest}}: already exists.")
  } else if (!is.null(pkgdown_file)) {
    write_pkgdown_translation(path, pkgdown_file, config_dest)
  } else {
    dir.create(dirname(config_dest), showWarnings = FALSE)
    writeLines(starter_config_yml(path), config_dest)
    cli::cli_alert_success(
      "Wrote {.path {config_dest}} (edit this to group your functions)."
    )
  }

  add_ignore_lines(
    path,
    ".gitignore",
    c("_quarto/*", "!_quarto/_freeze/")
  )
  add_ignore_lines(
    path,
    ".Rbuildignore",
    c("^docs$", "^_quarto$", "^_quartodown$")
  )

  cli_next_steps(c(
    if (is.null(pkgdown_file)) {
      "Edit {.file {config_file}} to group your functions the way you want."
    } else {
      "Check {.file {config_file}}, translated from {.file {pkgdown_file}}."
    },
    "Build and preview the site with {.run quartodown::build_site()}."
  ))

  invisible(config_dest)
}

# Writes the translation of the package's pkgdown configuration (see
# translate_pkgdown()) to `config_dest`, with the files it needs, and lists
# what it couldn't translate.
write_pkgdown_translation <- function(path, pkgdown_file, config_dest) {
  translation <- translate_pkgdown(path, pkgdown_file)
  dir.create(dirname(config_dest), showWarnings = FALSE)
  for (name in names(translation$files)) {
    dest <- file.path(dirname(config_dest), name)
    if (file.exists(dest)) {
      cli::cli_alert_info("Skipping {.path {dest}}: already exists.")
    } else {
      write_utf8(translation$files[[name]], dest)
      cli::cli_alert_success("Wrote {.path {dest}}.")
    }
  }
  write_utf8(pkgdown_config_yml(translation$config, pkgdown_file), config_dest)
  cli::cli_alert_success(
    "Wrote {.path {config_dest}}, translated from {.file {pkgdown_file}}."
  )
  if (length(translation$skipped) > 0) {
    cli::cli_alert_info("Not translated, with no equivalent in quartodown:")
    cli::cli_bullets(cli_lines(translation$skipped, "*"))
  }
}

check_pkg_root <- function(path, call = parent.frame()) {
  if (!file.exists(file.path(path, "DESCRIPTION"))) {
    cli::cli_abort(
      "No {.file DESCRIPTION} found at {.path {path}}. Is this a package root?",
      call = call
    )
  }
}

# A numbered list of what to do next, after a blank line.
cli_next_steps <- function(steps, .envir = parent.frame()) {
  cli::cli_text("")
  cli::cli_text("Next steps:")
  cli::cli_ol(steps, .envir = .envir)
}

# Appends any of `lines` not already present (as a whole line) in
# `<path>/<file_name>`, creating the file first if needed.
add_ignore_lines <- function(path, file_name, lines) {
  dest <- file.path(path, file_name)
  existing <- if (file.exists(dest)) {
    readLines(dest, warn = FALSE)
  } else {
    character()
  }

  missing <- lines[!lines %in% existing]
  if (length(missing) == 0) {
    return(invisible())
  }

  writeLines(c(existing, missing), dest)
  cli::cli_alert_success("Added {.code {missing}} to {.path {dest}}.")
  invisible()
}

starter_config_yml <- function(path) {
  c(
    "# Configuration for quartodown::build_site(). See",
    "# vignette(\"customizing\", package = \"quartodown\") for all fields,",
    "# including `navbar:` (navbar items and menus) and `quarto:` (other Quarto",
    "# settings).",
    "",
    "# Builds the development version (e.g. 1.2.0.9000) into dev/, next to the",
    "# released version's site; `release` keeps a single site.",
    "development:",
    "  mode: auto",
    "",
    "# Groups the function reference index into sections, each with a `title:`",
    "# and the `contents:` it lists (topic names or selectors such as",
    "# starts_with(\"read_\")).",
    starter_reference_yml(path)
  )
}

# Every public topic, sorted, in a single "All functions" section.
starter_reference_yml <- function(path) {
  topics <- Filter(function(t) !t$internal, rd_topics(path))
  names <- sort(vapply(topics, function(t) t$name, character(1)))

  c(
    "reference:",
    "  - title: All functions",
    "    contents:",
    if (length(names) == 0) "      []" else paste0("      - ", names)
  )
}
