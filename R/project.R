# ---- the Quarto project ---------------------------------------------------------
#
# build_site() assembles a Quarto website project in _quarto/ from the package
# sources, then renders it into docs/ (`output-dir: ../docs/`). _quarto/ is
# emptied first on every build, except Quarto's _freeze/ when `freeze = TRUE`.

project_dir <- function(path) file.path(path, "_quarto")

# The built site's folder, which may also hold the development site in dev/.
docs_dir <- function(path) file.path(path, "docs")

# Where this build goes: docs/, or docs/dev/ for the development site.
site_dir <- function(path, config = list()) {
  docs <- docs_dir(path)
  if (dev_mode(path, config) == "devel") file.path(docs, "dev") else docs
}

dev_modes <- c("release", "devel", "unreleased", "auto")

# Whether the site is set to have a development version's site, which is
# looked for and linked even from a release build (see dev_mode_meta()).
has_dev_mode <- function(config = list()) {
  !is.null(config$development$mode) ||
    nzchar(Sys.getenv("QUARTODOWN_DEV_MODE"))
}

# _quartodown.yml's `development: mode:` (or the QUARTODOWN_DEV_MODE
# environment variable, which takes precedence): "release" (the
# default), "devel" (a site in dev/ for the development version, alongside
# the released version's), or "unreleased" (in the root, for a package not
# yet released). "auto" picks from the version: a fourth component makes it
# devel, and a 0.0.0.* version unreleased.
dev_mode <- function(path, config = list()) {
  mode <- Sys.getenv("QUARTODOWN_DEV_MODE")
  if (!nzchar(mode)) {
    mode <- config$development$mode
    if (is.null(mode)) {
      mode <- "release"
    }
    if (!is.character(mode) || length(mode) != 1 || !mode %in% dev_modes) {
      config_abort(
        "development: mode",
        "must be one of {.or {.val {dev_modes}}}."
      )
    }
  } else if (!mode %in% dev_modes) {
    cli::cli_abort(
      "{.envvar QUARTODOWN_DEV_MODE} must be one of {.or {.val {dev_modes}}}, not {.val {mode}}."
    )
  }
  if (mode != "auto") {
    return(mode)
  }
  version <- desc::desc_get_field("Version", default = "0.0.0", file = path)
  version <- unclass(package_version(version))[[1]]
  if (length(version) < 4) {
    "release"
  } else if (all(version[1:3] == 0)) {
    "unreleased"
  } else {
    "devel"
  }
}

pkg_name <- function(path) desc::desc_get_field("Package", file = path)

reset_project <- function(path, freeze = FALSE) {
  dir <- project_dir(path)
  files <- list.files(dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  if (freeze) {
    files <- files[basename(files) != "_freeze"]
  }
  unlink(files, recursive = TRUE)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  invisible()
}

# Copies everything in `from` to the same paths under `to`.
copy_dir <- function(from, to) {
  files <- list.files(from, recursive = TRUE, all.files = TRUE)
  for (dir in unique(file.path(to, dirname(files)))) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  file.copy(file.path(from, files), file.path(to, files), overwrite = TRUE)
  invisible()
}

# ---- package files ----------------------------------------------------------------

# Vignettes, including vignettes/articles/, are rendered by Quarto itself,
# alongside any images or data files they use. Each gets a source_note() under
# its title, ruled off from the text below like a heading (see
# quartodown.scss), as a reference page's is by its first `##` heading.
copy_vignettes <- function(path, config = list()) {
  out <- file.path(project_dir(path), "vignettes")
  copy_dir(file.path(path, "vignettes"), out)
  source <- pkg_repo(path, config)$source
  for (vignette in find_vignettes(path)) {
    file <- file.path(out, vignette)
    lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
    # Quarto rejects `#|` chunk options that aren't YAML, such as knitr's
    # `#| label, echo = FALSE`, which R Markdown vignettes may use.
    if (grepl("\\.[Rr]md$", vignette)) {
      parts <- split_front_matter(lines)
      front_matter <- parts$front_matter
      if (length(front_matter) == 0) {
        front_matter <- c("---", "---")
      }
      lines <- c(
        append(front_matter, "validate-yaml: false", length(front_matter) - 1),
        parts$body
      )
    }
    note <- source_note(
      file.path("vignettes", vignette),
      source,
      class = "source-note"
    )
    writeLines(
      enc2utf8(insert_after_front_matter(lines, c(note, ""))),
      file,
      useBytes = TRUE
    )
  }
}

# `lines` with `insert` after the YAML front matter (`---` to `---` or `...`),
# or at the top if there's none.
insert_after_front_matter <- function(lines, insert) {
  parts <- split_front_matter(lines)
  front_matter <- parts$front_matter
  c(front_matter, if (length(front_matter) > 0) "", insert, parts$body)
}

# man/figures/ holds the images README.md and `\figure{}` use. README.Rmd's
# knitr figures can land in README_files/ instead.
copy_figures <- function(path) {
  for (dir in c("man/figures", "README_files")) {
    copy_dir(file.path(path, dir), file.path(project_dir(path), dir))
  }
}

# _quartodown/, with the files _quartodown.yml's `quarto:` settings refer to.
copy_site_files <- function(path) {
  copy_dir(
    file.path(path, config_dir),
    file.path(project_dir(path), config_dir)
  )
}

# Markdown files rendered as a page of the same name, from the package root or
# .github/.
community_files <- c("CODE_OF_CONDUCT.md", "CONTRIBUTING.md", "SUPPORT.md")

# The path, relative to `path`, of the community file `name`, or NULL.
community_file_source <- function(path, name) {
  candidates <- c(name, file.path(".github", name))
  found <- candidates[file.exists(file.path(path, candidates))]
  if (length(found) == 0) NULL else found[[1]]
}

# NEWS.md and the community files keep their name; LICENSE.md is the full
# license, and a plain LICENSE file (e.g. the "YEAR:/COPYRIGHT HOLDER:" stub
# of "MIT + file LICENSE") becomes LICENSE-text.md.
import_markdown_files <- function(path, config = list()) {
  out <- project_dir(path)
  read <- function(file) readLines(file, warn = FALSE, encoding = "UTF-8")
  read_md <- function(file) github_markdown_to_quarto(read(file))
  write <- function(lines, name) {
    writeLines(enc2utf8(lines), file.path(out, name), useBytes = TRUE)
  }

  news <- file.path(path, "NEWS.md")
  if (file.exists(news)) {
    write(link_news(read_md(news), pkg_repo(path, config)), "NEWS.md")
  }
  for (name in community_files) {
    src <- community_file_source(path, name)
    if (!is.null(src)) write(read_md(file.path(path, src)), name)
  }
  if (file.exists(file.path(path, "LICENSE.md"))) {
    write(read_md(file.path(path, "LICENSE.md")), "LICENSE.md")
  }
  if (file.exists(file.path(path, "LICENSE"))) {
    write(
      c(
        "---",
        "title: License",
        "---",
        "",
        "```",
        read(file.path(path, "LICENSE")),
        "```"
      ),
      "LICENSE-text.md"
    )
  }
  invisible()
}

# Rewrites GitHub-flavored Markdown that Quarto doesn't render: alerts
# (`> [!NOTE]` and the lines quoted after it) become callouts, and with
# `mermaid = TRUE`, ```` ```mermaid ```` blocks become ```` ```{mermaid} ````
# diagrams. Quarto runs those as code, so only in a .qmd file, not a .md. Code
# blocks are left alone.
github_markdown_to_quarto <- function(lines, mermaid = FALSE) {
  alert_pattern <- "^\\s*>\\s*\\[!(NOTE|TIP|IMPORTANT|WARNING|CAUTION)\\]\\s*$"
  kinds <- code_block_lines(lines)
  out <- character()
  i <- 1L
  while (i <= length(lines)) {
    line <- lines[[i]]
    kind <- kinds[[i]]
    i <- i + 1L
    if (kind == "open" && mermaid) {
      line <- sub("^(\\s*(```+|~~~+))\\s*mermaid\\s*$", "\\1{mermaid}", line)
      out <- c(out, line)
    } else if (nzchar(kind)) {
      out <- c(out, line)
    } else if (grepl(alert_pattern, line, ignore.case = TRUE)) {
      type <- tolower(sub(alert_pattern, "\\1", line, ignore.case = TRUE))
      body <- character()
      while (i <= length(lines) && grepl("^\\s*>", lines[[i]])) {
        body <- c(body, sub("^\\s*> ?", "", lines[[i]]))
        i <- i + 1L
      }
      out <- c(out, "", paste0("::: {.callout-", type, "}"), body, ":::", "")
    } else {
      out <- c(out, line)
    }
  }
  out
}

# Links issues and pull requests (`#123`) and users (`@user`) to `repo`'s
# (see pkg_repo()). Code blocks are left alone.
link_news <- function(lines, repo) {
  text <- !nzchar(code_block_lines(lines))
  if (!is.na(repo$issue)) {
    lines[text] <- gsub(
      "(?<![\\w\\[/&#])#(\\d+)\\b",
      sprintf("[#\\1](%s\\1)", repo$issue),
      lines[text],
      perl = TRUE
    )
  }
  if (!is.na(repo$user)) {
    lines[text] <- gsub(
      "(^|\\s|\\()@([-\\w]+)",
      sprintf("\\1[@\\2](%s\\2)", repo$user),
      lines[text],
      perl = TRUE
    )
  }
  lines
}

# ---- rendering --------------------------------------------------------------------

check_quarto <- function() {
  if (is.null(quarto::quarto_path())) {
    cli::cli_abort(c(
      "Quarto (the command-line tool) isn't installed.",
      "i" = "Install it from {.url https://quarto.org/docs/get-started/}."
    ))
  }
}

# `envvars` makes the package available to the pages' R processes, see
# install_envvars() and load_all_envvars().
render_project <- function(path, envvars, config = list(), quiet = TRUE) {
  clear_site(path, config)

  n <- length(render_inputs(project_dir(path)))
  # Quarto's own output goes to the console unless `quiet`, and a progress
  # step's line would be redrawn over it.
  current <- ""
  step <- if (quiet) {
    cli::cli_progress_step(
      "Rendering {n} page{?s} with Quarto{current}",
      msg_done = "Rendered {n} page{?s} with Quarto"
    )
  } else {
    cli::cli_inform("Rendering {n} page{?s} with Quarto.")
  }

  # Each page's time runs from Quarto's line starting it to the line starting
  # the next, and the last page's to "Output created", after which Quarto only
  # finishes the site (search index, sitemap). The first page's includes
  # compiling the theme. The step names the page being rendered only where its
  # line can be redrawn, else each update would print a line.
  show_page <- quiet && cli::is_dynamic_tty()
  pages <- character()
  starts <- numeric()
  end <- NA_real_
  on_line <- function(line, proc) {
    file <- rendering_file(line)
    if (!is.na(file)) {
      pages <<- c(pages, file)
      starts <<- c(starts, as.numeric(Sys.time()))
      if (show_page) {
        current <<- paste0(": ", gsub("([{}])", "\\1\\1", file))
        cli::cli_progress_update(id = step, force = TRUE)
      }
    } else if (is.na(end) && grepl("^\\s*Output created", line)) {
      end <<- as.numeric(Sys.time())
    }
  }

  result <- run_quarto_render(path, envvars, quiet, on_line)
  if (result$status != 0) {
    cli::cli_abort(c(
      "Quarto failed to render the site.",
      if (quiet) render_error_log(result$stderr)
    ))
  }
  # Without it, GitHub Pages runs the site through Jekyll, which drops `_`
  # folders such as _quartodown/.
  file.create(file.path(docs_dir(path), ".nojekyll"))

  if (quiet) {
    cli::cli_progress_done(id = step)
  }
  if (is.na(end)) {
    end <- as.numeric(Sys.time())
  }
  times <- stats::setNames(diff(c(starts, end)), pages)
  cli::cli_bullets(render_time_bullets(times))
  invisible()
}

# The file a line of Quarto's output starts rendering ("[2/31]
# reference/foo.qmd"), with `/` separators, or NA.
rendering_file <- function(line) {
  line <- trimws(gsub("\r", "", line, fixed = TRUE))
  if (!grepl("^\\[\\s*\\d+/\\d+\\]\\s", line)) {
    return(NA_character_)
  }
  gsub("\\", "/", sub("^\\[[^]]*\\]\\s+", "", line), fixed = TRUE)
}

# cli bullets with the seconds Quarto took for the pages in `times` (named by
# file): the reference pages together, each vignette, slowest first (the ten
# slowest, the rest together), the home page, and the other pages together.
render_time_bullets <- function(times, max_vignettes = 10) {
  file <- as.character(names(times))
  is_ref <- startsWith(file, "reference/") & file != "reference/index.qmd"
  is_vignette <- startsWith(file, "vignettes/")
  is_home <- file == "index.qmd"
  is_other <- !(is_ref | is_vignette | is_home)

  group <- function(x, label) {
    if (length(x) > 0) {
      plural <- if (length(x) == 1) "" else "s"
      sprintf("%d %s%s: %s", length(x), label, plural, format_secs(sum(x)))
    }
  }
  vignettes <- sort(times[is_vignette], decreasing = TRUE)
  shown <- utils::head(vignettes, max_vignettes)
  lines <- c(
    group(times[is_ref], "reference page"),
    sprintf("%s: %s", names(shown), format_secs(shown)),
    group(vignettes[-seq_along(shown)], "more vignette"),
    if (any(is_home)) {
      sprintf("Home page: %s", format_secs(sum(times[is_home])))
    },
    group(times[is_other], "other page")
  )
  # Braces escaped so cli doesn't interpolate them.
  lines <- gsub("([{}])", "\\1\\1", lines)
  structure(lines, names = rep("*", length(lines)))
}

format_secs <- function(x) {
  sprintf("%.1fs", x)
}

# The environment variables are set only while Quarto runs, so the progress
# step's messages aren't colored as for the pages. `on_line` is called with
# each line Quarto writes to stderr, where its progress goes.
run_quarto_render <- function(path, envvars, quiet, on_line = NULL) {
  # Colored output, as in a terminal; see ansi_to_html(). QUARTO_R, unless set
  # already, makes Quarto run the pages' code with this R.
  local_envvars(c(
    envvars,
    R_CLI_NUM_COLORS = "256",
    if (!nzchar(Sys.getenv("QUARTO_R"))) c(QUARTO_R = R.home("bin"))
  ))
  # Not quarto::quarto_render(): its `quiet` passes `--quiet`, which drops
  # Quarto's error messages too. Without it, Quarto writes its progress and
  # errors to stderr, kept here to show the error.
  processx::run(
    quarto::quarto_path(),
    c("render", project_dir(path)),
    echo = !quiet,
    stderr_line_callback = on_line,
    error_on_status = FALSE
  )
}

# The files Quarto renders in the project `dir`: those it can render, outside
# `_`/`.` folders and not themselves `_`/`.`-prefixed.
render_inputs <- function(dir) {
  files <- list.files(
    dir,
    pattern = "\\.(qmd|md|Rmd|ipynb)$",
    recursive = TRUE,
    ignore.case = TRUE
  )
  files[!grepl("(^|/)[_.]", files)]
}

# The end of Quarto's output, from the line naming the file that failed
# ("[2/31] reference/foo.qmd"), as cli bullets.
render_error_log <- function(stderr) {
  lines <- gsub("\r", "", split_lines(stderr), fixed = TRUE)
  lines <- lines[nzchar(trimws(lines))]
  start <- utils::tail(grep("^\\[\\s*\\d+/\\d+\\]", lines), 1)
  if (length(start) == 1) {
    lines <- lines[start:length(lines)]
  }
  # Braces escaped so cli doesn't interpolate them.
  lines <- gsub("([{}])", "\\1\\1", utils::tail(lines, 30))
  structure(lines, names = rep(" ", length(lines)))
}

# Empties the folder this build goes in (see site_dir()), as a stale page would
# otherwise outlive its source. CNAME (GitHub Pages' custom domain) and hidden
# files such as .nojekyll stay, and so does the development site in dev/,
# which a release build doesn't replace.
clear_site <- function(path, config = list()) {
  dir <- site_dir(path, config)
  files <- list.files(dir, full.names = TRUE)
  keep <- if (dir == docs_dir(path)) c("CNAME", "dev") else "CNAME"
  unlink(files[!basename(files) %in% keep], recursive = TRUE)
  invisible()
}

# ---- the package for the pages' code ------------------------------------------
#
# Quarto runs each page's code in a new R process, as run_examples() does the
# reference pages' examples, all in one. The package is either installed into
# a temporary library first on its library path, or, with `devel = TRUE`,
# loaded with pkgload::load_all() by an R profile.

install_envvars <- function(path, scratch) {
  lib <- file.path(scratch, "library")
  dir.create(lib)
  pkg <- pkg_name(path)
  cli::cli_progress_step(
    "Installing {.pkg {pkg}} into a temporary library",
    msg_done = "Installed {.pkg {pkg}} into a temporary library"
  )
  output <- suppressWarnings(system2(
    file.path(R.home("bin"), "R"),
    c(
      "CMD",
      "INSTALL",
      "--no-multiarch",
      "--with-keep.source",
      "-l",
      shQuote(lib),
      shQuote(normalizePath(path))
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(output, "status")
  if (!is.null(status) && status != 0) {
    # The log's lines, with braces escaped so cli doesn't interpolate them.
    log <- gsub("([{}])", "\\1\\1", utils::tail(output, 20))
    cli::cli_abort(c(
      "Installing {.pkg {pkg}} failed.",
      "i" = "Fix the installation, or build with {.code devel = TRUE}.",
      structure(log, names = rep(" ", length(log)))
    ))
  }
  c(R_LIBS = paste(c(lib, .libPaths()), collapse = .Platform$path.sep))
}

load_all_envvars <- function(path, scratch) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    cli::cli_abort(c(
      "Building with {.code devel = TRUE} needs the {.pkg pkgload} package.",
      "i" = "Install it with {.code install.packages(\"pkgload\")}."
    ))
  }
  profile <- file.path(scratch, "Rprofile.R")
  writeLines(
    c(
      # R_PROFILE_USER replaces the user's own profile, so it's run first.
      "if (file.exists('.Rprofile')) {",
      "  source('.Rprofile')",
      "} else if (file.exists('~/.Rprofile')) {",
      "  source('~/.Rprofile')",
      "}",
      sprintf(
        "suppressMessages(pkgload::load_all(%s, export_all = FALSE, helpers = FALSE, quiet = TRUE))",
        deparse(normalizePath(path))
      )
    ),
    profile
  )
  c(R_PROFILE_USER = profile)
}

# Evaluates `code` with the temporary library of `envvars` (from
# install_envvars()), if any, first on this R session's library path.
with_libpaths <- function(envvars, code) {
  if (is.na(envvars["R_LIBS"])) {
    return(code)
  }
  old <- .libPaths()
  on.exit(.libPaths(old))
  lib <- strsplit(envvars[["R_LIBS"]], .Platform$path.sep, fixed = TRUE)[[1]]
  .libPaths(c(lib[[1]], old))
  code
}

# Sets `envvars` until the calling function exits.
local_envvars <- function(envvars, envir = parent.frame()) {
  old <- Sys.getenv(names(envvars), unset = NA, names = TRUE)
  do.call(Sys.setenv, as.list(envvars))
  defer <- function() {
    unset <- is.na(old)
    Sys.unsetenv(names(old)[unset])
    if (!all(unset)) do.call(Sys.setenv, as.list(old[!unset]))
  }
  do.call(on.exit, list(substitute(defer()), add = TRUE), envir = envir)
  invisible()
}

# The site's own URL: _quartodown.yml's `url:`, else the
# first DESCRIPTION URL that isn't a source repository. NA if there's none.
site_url <- function(path, config = list()) {
  if (!is.null(config$url)) {
    return(sub("/+$", "", config$url))
  }
  urls <- desc::desc_get_urls(path)
  urls <- urls[is.na(repo_host(urls))]
  if (length(urls) == 0) NA_character_ else sub("/+$", "", urls[[1]])
}

# downlit finds a package's reference pages and vignettes through the
# pkgdown.yml at its site's root.
write_pkgdown_yml <- function(path, url = site_url(path), config = list()) {
  if (is.null(url) || is.na(url)) {
    return(invisible())
  }
  vignettes <- find_vignettes(path)
  articles <- as.list(sprintf("%s.html", strip_ext(vignettes)))
  names(articles) <- basename(strip_ext(vignettes))
  yaml::write_yaml(
    list(
      urls = list(
        reference = paste0(url, "/reference"),
        article = paste0(url, "/vignettes")
      ),
      articles = articles
    ),
    file.path(site_dir(path, config), "pkgdown.yml")
  )
}

#' Preview the built site
#'
#' Serves `docs/` (or `docs/dev/`, for a development site; see the
#' Configuration section of [build_site()]), as built by [build_site()], in a
#' web browser, and reloads
#' the page when the site is rebuilt. The server keeps running for the rest of
#' the R session and is reused by later calls, including [build_site()]'s
#' (which, with a server already running, leaves the open page to reload
#' instead of opening another).
#'
#' @inheritParams build_site
#' @return The server, invisibly; see [servr::httw()].
#' @export
#'
#' @examples
#' \dontrun{
#' build_site()
#' preview_site()
#' }
preview_site <- function(path = ".") {
  serve_site(path, read_config(path), reopen = TRUE)
}

# Serves the site (see site_dir()), starting a server unless one already
# serves it. `reopen` opens the page again in the browser when it does.
serve_site <- function(path, config = list(), reopen) {
  docs <- site_dir(path, config)
  if (!file.exists(file.path(docs, "index.html"))) {
    cli::cli_abort(c(
      "No built site found in {.path {docs}}.",
      "i" = "Build it with {.run quartodown::build_site()}."
    ))
  }
  if (!requireNamespace("servr", quietly = TRUE)) {
    cli::cli_abort(c(
      "Previewing the site needs the {.pkg servr} package.",
      "i" = "Install it with {.code install.packages(\"servr\")}."
    ))
  }

  root <- normalizePath(docs)
  if (!identical(the$server_root, root)) {
    if (!is.null(the$server)) {
      the$server$stop_server()
    }
    the$server <- servr::httw(docs)
    the$server_root <- root
  } else if (reopen) {
    the$server$browse(reopen = TRUE)
  }
  invisible(the$server)
}

# The preview server, kept across calls so rebuilds reuse it, and the
# packages' CRAN pages (see cran_url_if_available()).
the <- new.env(parent = emptyenv())
the$cran_urls <- list()
