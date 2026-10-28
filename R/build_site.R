#' Build the package website
#'
#' Builds the package website into `docs/` with [Quarto](https://quarto.org/).
#'
#' `build_site()` assembles a Quarto website project in `_quarto/` from the
#' package sources and `_quartodown/_quartodown.yml`, then renders it. It uses
#' `README.md` (or, without one, `DESCRIPTION`), `man/`, the vignettes (including those in subfolders),
#' `NEWS.md`, `LICENSE`/`LICENSE.md`, and `CODE_OF_CONDUCT.md`,
#' `CONTRIBUTING.md` and `SUPPORT.md` (at the top level or in `.github/`). See
#' `vignette("quartodown")` for what the site contains.
#'
#' While it builds, the environment variable `IN_QUARTODOWN` is `"true"`, so
#' the package's examples and vignettes can check
#' `identical(Sys.getenv("IN_QUARTODOWN"), "true")`. `IN_QUARTODOWN_PKG` is
#' the name of the package whose site it is, which code shared between
#' packages can check.
#'
#' @section Configuration:
#' `_quartodown/_quartodown.yml` is optional; [use_quartodown()] creates
#' one. It can contain:
#'
#' * `url`: the site's address. It defaults to the first `URL` in `DESCRIPTION`
#'   that isn't a source repository.
#' * `reference`: the sections of the function reference index and the topics
#'   each lists, by name or with selectors such as `starts_with()`. The format
#'   is the same as pkgdown's.
#' * `navbar`: the items on the navbar's `left` and `right`: keywords such as
#'   `reference` and `articles`, vignettes, and Quarto navbar items and menus;
#'   and the icons in its `tools`, by default `repo`.
#'   With `style: sidebar`, the `left` items go in a sidebar instead, with a
#'   link to every reference page.
#' * `home`: `file`, the `.md` or `.qmd` file the home page shows instead of
#'   `README.md`; `sidebar`, the sidebar's `structure` (its sections, in
#'   order) and `components` (sections of your own), or `false` to show the
#'   page as written, without it; and `links`, added to the sidebar's links.
#' * `authors`: by a person's name, an `href` to link it and `html` to
#'   replace it; and under `sidebar` and `footer`, the `roles` shown, with
#'   `before`/`after` text in the sidebar and `text` in the footer.
#' * `repo`: the repository's `url`s (`home`, and the prefixes of links to a
#'   file, `source`; an issue, `issue`; and a user, `user`) and `branch`. They
#'   default to those of DESCRIPTION's GitHub, GitLab or Codeberg `URL`.
#' * `theme`: the `light` and `dark` themes: names of Quarto's built-in
#'   themes, such as `flatly` and `darkly`, or `.scss` files in
#'   `_quartodown/`. By default, `default` and `darkly`.
#' * `redirects`: pairs of paths, from an old page to the one replacing it,
#'   such as `["reference/old.html", "reference/new.html"]`.
#' * `development`: `mode`, `release` (the default) to build the site into
#'   `docs/`, or `devel` to build a site for the development version into
#'   `docs/dev/`, next to the released version's; `unreleased` marks the
#'   version as unreleased, and `auto` picks from the version number and
#'   whether the package is on CRAN. The environment variable
#'   `QUARTODOWN_DEV_MODE` overrides it. The format is the same as pkgdown's.
#'   Content in `.quartodown-devel` or `.quartodown-release` shows on only
#'   that site.
#' * `vignettes`: `files`, files and folders outside `vignettes/` that the
#'   vignettes use by a relative path, such as `man/rmd/` for a child document
#'   `../man/rmd/shared.Rmd`, copied with them.
#' * `logo`: `false` to leave the logo, `man/figures/logo.svg` or `.png`
#'   (and in light mode `logo-light`, if it exists), off the pages' titles.
#' * `internal-pages`: `true` to give every `\keyword{internal}` topic a
#'   reference page, rather than only those the reference index lists.
#' * `quarto`: settings merged over the generated Quarto configuration.
#'
#' `_quartodown/extra.scss` and `_quartodown/extra.css`, if they exist, style
#' every page, on top of quartodown's theme. The whole `_quartodown/` folder is
#' copied into the Quarto project, so `quarto` settings can refer to other
#' files in it by the same path.
#'
#' See `vignette("customizing", package = "quartodown")` for the details.
#'
#' @param path Path to the package root.
#' @param tooltips Logical. Give references to the package's own functions
#'   (in code blocks and inline code) a hover tooltip showing the function's
#'   usage and the first sentence of its description?
#' @param freeze Logical. Reuse the results of code that ran in an earlier
#'   build, for pages whose source hasn't changed since? Uses Quarto's
#'   [freeze](https://quarto.org/docs/projects/code-execution.html#freeze),
#'   which keeps them in `_quarto/_freeze/`. Changes to the package's code
#'   or its dependencies aren't detected; delete `_quarto/_freeze/` to rerun
#'   everything.
#' @param quiet Logical. Hide Quarto's rendering output? If a page fails to
#'   render, its error is shown either way.
#' @param devel Logical. By default, the examples and vignettes run against
#'   the package installed from `path` into a temporary library. With
#'   `devel = TRUE`, they run against it loaded with [pkgload::load_all()]
#'   instead, which is faster, but code that starts other R processes (e.g.
#'   with callr or parallel workers) won't find the package there.
#' @param workers Number of processes to build the site with. More than 1
#'   needs the [mirai](https://mirai.r-lib.org/) package.
#' @param preview Logical. Open the built site in a web browser with
#'   [preview_site()]? By default, only in interactive sessions.
#' @return Invisibly, the path to the built site.
#' @export
#'
#' @examples
#' \dontrun{
#' use_quartodown()
#' build_site()
#' }
build_site <- function(
  path = ".",
  tooltips = TRUE,
  freeze = FALSE,
  quiet = TRUE,
  devel = FALSE,
  workers = 1,
  preview = interactive()
) {
  if (
    !is.numeric(workers) ||
      length(workers) != 1 ||
      is.na(workers) ||
      workers < 1 ||
      workers != round(workers)
  ) {
    cli::cli_abort("{.arg workers} must be a whole number of at least 1.")
  }
  config <- read_config(path)
  check_quarto()
  topics <- rd_topics(path)
  check_config(path, config, topics)

  # Lets code (`\Sexpr`, `@examplesIf`, examples, vignettes) tell it is
  # building the site, and whose. Quarto's R processes inherit them.
  local_envvars(c(
    IN_QUARTODOWN = "true",
    IN_QUARTODOWN_PKG = pkg_name(path)
  ))

  # The package is installed first: build-stage `\Sexpr` macros in the Rd
  # files, such as Rdpack's `\insertRef{}`, need it, as in `R CMD build`.
  scratch <- tempfile("quartodown-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  envvars <- if (devel) {
    load_all_envvars(path, scratch)
  } else {
    install_envvars(path, scratch)
  }
  local_workers(workers)

  cli::cli_progress_step(
    "Writing the Quarto project to {.path _quarto/}",
    msg_done = "Wrote the Quarto project to {.path _quarto/}"
  )
  reset_project(path, freeze = freeze)
  copy_vignettes(path, config)
  copy_package_files(path, config)
  with_libpaths(envvars, write_man_pages(path, config, topics))
  import_markdown_files(path, config)
  copy_theme_files(path, config)
  copy_site_files(path)
  write_home(path, config)
  build_reference_qmd(path, config, topics)
  build_authors_qmd(path, config)
  settings <- write_quarto_config(path, config, freeze, topics)

  cli::cli_progress_done()

  run_examples(path, envvars, settings, freeze = freeze, workers = workers)
  render_project(
    path,
    envvars,
    settings,
    config,
    quiet = quiet,
    workers = workers,
    scratch = scratch
  )

  done <- if (isTRUE(tooltips)) {
    "Linked code and added tooltips"
  } else {
    "Linked code"
  }
  cli::cli_progress_step("Linking code", msg_done = done)
  write_pkgdown_yml(path, settings$website$`site-url`, config)
  postprocess_pages(path, config, topics, tooltips = tooltips)
  write_redirects(path, config)
  cli::cli_progress_done()
  cli::cli_alert_success("Built the site in {.path {site_dir(path, config)}}")

  if (isTRUE(preview) && !identical(Sys.getenv("TESTTHAT"), "true")) {
    if (requireNamespace("servr", quietly = TRUE)) {
      serve_site(path, config, reopen = FALSE)
    } else {
      cli::cli_inform(c(
        "i" = "Install {.pkg servr} to preview the site in your browser."
      ))
    }
  }

  invisible(site_dir(path, config))
}

theme_files <- c(
  "quartodown.scss",
  "quartodown-fonts.scss",
  "quartodown-header.html",
  "quartodown-after-body.html",
  "orcid.svg",
  "ror.svg"
)

# The package's site files: _quartodown.yml, plus anything _quartodown.yml
# refers to and extra.scss/extra.css, which style every page. The folder is
# copied into _quarto/ as is, so paths in it are the same from the package root
# and from the project. Quarto doesn't render `_` folders, so nothing in it
# becomes a page.
config_dir <- "_quartodown"
config_file <- file.path(config_dir, "_quartodown.yml")

read_config <- function(path) {
  if (file.exists(file.path(path, "_quartodown.yml"))) {
    cli::cli_warn(c(
      "{.file _quartodown.yml} at the package root is ignored.",
      "i" = "Move it to {.file {config_file}}."
    ))
  }
  config_path <- file.path(path, config_file)
  config <- if (file.exists(config_path)) yaml::read_yaml(config_path)
  if (is.null(config)) {
    return(list())
  }
  warn_unknown_fields(
    names(config),
    c(
      "url",
      "reference",
      "navbar",
      "home",
      "authors",
      "repo",
      "theme",
      "redirects",
      "development",
      "vignettes",
      "logo",
      "internal-pages",
      "quarto"
    )
  )
  if (is.list(config$vignettes)) {
    warn_unknown_fields(names(config$vignettes), "files", "vignettes")
  }
  if (is.list(config$home)) {
    warn_unknown_fields(
      names(config$home),
      c("file", "sidebar", "links"),
      "home"
    )
    check_home_config(config$home)
  }
  if (is.list(config$theme)) {
    warn_unknown_fields(names(config$theme), c("light", "dark"), "theme")
  }
  if (is.list(config$development)) {
    warn_unknown_fields(names(config$development), "mode", "development")
  }
  if (is.list(config$repo)) {
    warn_unknown_fields(names(config$repo), c("url", "branch"), "repo")
    warn_unknown_fields(
      names(config$repo$url),
      c("home", "source", "issue", "user"),
      "repo: url"
    )
  }
  # Any other field of `authors:` is a person, by name.
  for (where in intersect(names(config$authors), c("sidebar", "footer"))) {
    fields <- if (where == "sidebar") c("before", "after") else "text"
    warn_unknown_fields(
      names(config$authors[[where]]),
      c("roles", fields),
      paste("authors:", where)
    )
  }
  if (!is.null(config$logo) && !isTRUE(config$logo) && !isFALSE(config$logo)) {
    config_abort("logo", "must be {.code true} or {.code false}.")
  }
  pages <- config[["internal-pages"]]
  if (!is.null(pages) && !isTRUE(pages) && !isFALSE(pages)) {
    config_abort("internal-pages", "must be {.code true} or {.code false}.")
  }
  config_redirects(config) # Checks it before the slow render.
  config
}

# Runs the checks of the settings read_config() can't check on its own, which
# need the package's files and topics, so a mistake fails before the slow
# install and render rather than during them.
check_config <- function(path, config, topics) {
  dev_mode(path, config)
  home_file(path, config)
  quarto_theme(path, config)
  navbar_style(config)
  navbar_layout(path, config)
  reference_block_topics(reference_blocks(config, topics), topics)
  check_internal_links(config, topics, pkg_name(path))
  check_vignette_paths(path, config)
  invisible()
}

# Errors about _quartodown.yml's `field:` with `message`, whose first element
# is said of the field, e.g. `config_abort("theme", "must be a list.")` for
# "_quartodown.yml `theme:` must be a list.". `field` is interpolated too.
config_abort <- function(field, message, ..., .envir = parent.frame()) {
  cli::cli_abort(
    config_message(field, message),
    ...,
    call = .envir,
    .envir = .envir
  )
}

config_warn <- function(field, message, ..., .envir = parent.frame()) {
  cli::cli_warn(config_message(field, message), ..., .envir = .envir)
}

config_message <- function(field, message) {
  message[[1]] <- paste0("_quartodown.yml `", field, ":` ", message[[1]])
  message
}

# Warns about fields of _quartodown.yml (or of its `parent:`) that quartodown
# doesn't know, such as typos or pkgdown settings, which are ignored.
warn_unknown_fields <- function(fields, known, parent = NULL) {
  where <- if (is.null(parent)) "" else paste0(" under `", parent, ":`")
  for (field in setdiff(fields, known)) {
    distance <- utils::adist(field, known)[1, ]
    hint <- if (min(distance) <= 2) {
      c("i" = "Did you mean {.field {known[which.min(distance)]}}?")
    } else {
      c("i" = "Known fields: {.field {known}}.")
    }
    cli::cli_warn(c(
      "_quartodown.yml has an unknown field {.field {field}}{where}, which is ignored.",
      hint
    ))
  }
}

copy_theme_files <- function(path, config = list()) {
  src <- system.file(
    "quarto",
    theme_files,
    package = "quartodown",
    mustWork = TRUE
  )
  file.copy(src, file.path(project_dir(path), theme_files), overwrite = TRUE)
  write(
    ansi_scss_rules(),
    file.path(project_dir(path), "quartodown.scss"),
    append = TRUE
  )
  # Without `development:`, there's only ever the one site.
  if (has_dev_mode(config)) {
    write(
      dev_mode_meta(dev_mode(path, config)),
      file.path(project_dir(path), "quartodown-header.html"),
      append = TRUE
    )
  }
  invisible()
}

# The site's dev_mode(), for the after-body script, which labels the navbar's
# version with it and links the other site. A development site isn't indexed
# by search engines, which should send people to the released version's.
dev_mode_meta <- function(mode) {
  c(
    sprintf('<meta name="quartodown-dev-mode" content="%s">', mode),
    if (mode == "devel") '<meta name="robots" content="noindex">'
  )
}

# ---- Quarto configuration -----------------------------------------------------

quarto_config <- function(
  path,
  config = list(),
  freeze = FALSE,
  topics = rd_topics(path)
) {
  pkg <- pkg_name(path)
  version <- desc::desc_get_field("Version", default = "", file = path)
  mode <- dev_mode(path, config)

  navbar <- list(search = TRUE)
  items <- navbar_items(path, config)
  sidebar <- NULL
  if (navbar_style(config) == "sidebar") {
    sidebar <- list(
      style = "docked",
      contents = sidebar_contents(path, config, items$left, topics)
    )
    items$left <- NULL
  }
  for (side in names(items)) {
    if (length(items[[side]]) > 0) navbar[[side]] <- items[[side]]
  }

  settings <- list(
    project = list(
      type = "website",
      `output-dir` = if (mode == "devel") "../docs/dev/" else "../docs/"
    ),
    language = list(`search-text-placeholder` = " Search"),
    website = list(
      # after-body script splits the version into its own <small>.
      title = trimws(paste(pkg, version)),
      search = list(location = "navbar", type = "textbox"),
      navbar = navbar,
      `page-footer` = list(
        left = data_developed_by(path, config),
        right = paste(
          "Site built with [quartodown](https://github.com/BjarkeHautop/quartodown)",
          "and [Quarto](https://quarto.org/)."
        )
      )
    ),
    # As in the reference examples, output goes in the code's block, each
    # line prefixed with `#>`.
    knitr = list(opts_chunk = list(collapse = TRUE, comment = "#>")),
    format = list(
      html = list(
        theme = quarto_theme(path, config),
        toc = TRUE,
        `include-in-header` = "quartodown-header.html",
        `include-after-body` = "quartodown-after-body.html"
      )
    )
  )

  extra_css <- file.path(config_dir, "extra.css")
  if (file.exists(file.path(path, extra_css))) {
    settings$format$html$css <- extra_css
  }

  settings$website$sidebar <- sidebar

  # For the sitemap, among others.
  url <- site_url(path, config)
  if (!is.na(url)) {
    settings$website$`site-url` <- if (mode == "devel") {
      paste0(url, "/dev")
    } else {
      url
    }
  }
  settings$website <- c(settings$website, social_cards(path))
  logo <- logo_file(path)
  if (!is.null(logo)) {
    settings$website$favicon <- logo
  }

  if (isTRUE(freeze)) {
    # Reruns a page's code only when its source changed.
    settings$execute <- list(freeze = "auto")
  }

  if (!is.null(config$quarto)) {
    settings <- merge_quarto_config(settings, config$quarto)
  }
  settings
}

# _quartodown.yml's `theme:` (Quarto's `default` and `darkly` unless set), with
# quartodown's SCSS on top, then the package's _quartodown/extra.scss so it
# overrides quartodown's.
quarto_theme <- function(path, config = list()) {
  if (!is.null(config$theme) && !is.list(config$theme)) {
    config_abort(
      "theme",
      c(
        "must list a {.field light} and/or {.field dark} theme.",
        "i" = "For example, {.code theme: {{light: flatly, dark: darkly}}}."
      )
    )
  }
  theme <- list(light = "default", dark = "darkly")
  for (mode in names(theme)) {
    value <- config$theme[[mode]]
    if (is.null(value)) {
      next
    }
    if (!is_string(value)) {
      config_abort(
        "theme: {mode}",
        "must be a single theme name."
      )
    }
    theme[[mode]] <- value
  }
  extra_scss <- file.path(config_dir, "extra.scss")
  layers <- c(
    if (theme$light == "default") "quartodown-fonts.scss",
    "quartodown.scss",
    if (file.exists(file.path(path, extra_scss))) extra_scss
  )
  list(light = c(theme$light, layers), dark = c(theme$dark, layers))
}

# The package's logo, `man/figures/logo` with the first of the `exts` that
# exists (or the `-light` variant, `light = TRUE`), or NULL. The favicon
# prefers PNG, which every browser shows as one.
logo_file <- function(path, light = FALSE, exts = c("png", "svg")) {
  logos <- paste0("man/figures/logo", if (light) "-light", ".", exts)
  logos <- logos[file.exists(file.path(path, logos))]
  if (length(logos) > 0) logos[[1]]
}

# The logo shown by the title of every page not showing it already, unless
# `logo: false`: the `logo_file()`, preferring an SVG, which stays sharp at
# any size, or with a `-light` variant, both, named by the theme they show
# in. NULL without a logo.
site_logo <- function(path, config) {
  if (isFALSE(config$logo)) {
    return(NULL)
  }
  logo <- logo_file(path, exts = c("svg", "png"))
  light <- logo_file(path, light = TRUE, exts = c("svg", "png"))
  if (is.null(logo) || is.null(light)) {
    return(logo)
  }
  c(light = light, dark = logo)
}

# The preview of a link to the site on social media and chat apps: the
# package's title and, as a small square card, its logo. They don't show SVGs.
social_cards <- function(path) {
  title <- desc::desc_get_field("Title", default = NA, file = path)
  logo <- "man/figures/logo.png"
  c(
    list(`open-graph` = TRUE, `twitter-card` = list(`card-style` = "summary")),
    if (!is.na(title)) list(description = squish(title)),
    # From the site's root, not each page's folder.
    if (file.exists(file.path(path, logo))) list(image = paste0("/", logo))
  )
}

# Like utils::modifyList(), but unnamed lists (e.g. the navbar's `left:`
# items) are appended to. Other values are replaced, and `~` (NULL) removes.
merge_quarto_config <- function(x, val) {
  for (name in names(val)) {
    if (is.list(x[[name]]) && is.list(val[[name]])) {
      x[[name]] <- if (is.null(names(val[[name]]))) {
        c(x[[name]], val[[name]])
      } else {
        merge_quarto_config(x[[name]], val[[name]])
      }
    } else {
      x[[name]] <- val[[name]] # NULL removes it
    }
  }
  x
}

# Also writes the 404 page, as the render list (see render_list()), set unless
# the `quarto:` settings have one, needs every page written. Returns,
# invisibly, the settings written.
write_quarto_config <- function(
  path,
  config = list(),
  freeze = FALSE,
  topics = rd_topics(path)
) {
  settings <- quarto_config(path, config, freeze = freeze, topics = topics)
  write_404(path, settings$website$`site-url`)
  if (is.null(settings$project$render)) {
    settings$project$render <- render_list(
      project_dir(path),
      vignette_files(path, config)
    )
  }
  write_quarto_yml(settings, project_dir(path))
  invisible(settings)
}

# Quarto reads YAML 1.2, whose booleans are only `true`/`false`.
write_quarto_yml <- function(settings, dir) {
  yaml::write_yaml(
    settings,
    file.path(dir, "_quarto.yml"),
    handlers = list(logical = yaml::verbatim_logical)
  )
}

# ---- home page ----------------------------------------------------------------

# The home page is `home: file:` from _quartodown.yml, else README.md, else
# made from DESCRIPTION; with a sidebar unless `home: sidebar: false`.
write_home <- function(path, config = list()) {
  file <- home_file(path, config)
  lines <- if (is.null(file)) {
    description_home(path)
  } else {
    read_utf8(file.path(path, file))
  }
  write_utf8(
    build_website_readme(
      lines,
      path,
      config,
      qmd = !is.null(file) && grepl("\\.qmd$", file, ignore.case = TRUE)
    ),
    file.path(project_dir(path), "index.qmd")
  )
}

# The home page's source, relative to `path`: `home: file:`, else README.md
# if there is one, else NULL.
home_file <- function(path, config = list()) {
  file <- config$home$file
  if (is.null(file)) {
    return(if (file.exists(file.path(path, "README.md"))) "README.md")
  }
  if (!is_string(file)) {
    config_abort(
      "home: file",
      "must be a single file path."
    )
  }
  if (!file.exists(file.path(path, file))) {
    config_abort(
      "home: file",
      "is {.path {file}}, which doesn't exist."
    )
  }
  file
}

# The page GitHub Pages (and Netlify) show for a URL that doesn't exist, with
# the navbar and search to find the way back. It can be shown at any depth, so
# needs absolute links, which Quarto makes from the `site_url`; without one,
# the host's own page is better than an unstyled one.
write_404 <- function(path, site_url) {
  if (is.null(site_url)) {
    return(invisible())
  }
  writeLines(
    c(
      "---",
      "title: Page not found",
      "toc: false",
      "search: false",
      "---",
      "",
      "Sorry, there is no page at this address. It may have moved, or the link",
      "may be mistyped. Try the search, or go to the [home page](index.qmd)."
    ),
    file.path(project_dir(path), "404.qmd")
  )
}

# Checks `home: sidebar:` and `home: links:` before the slow render.
check_home_config <- function(home) {
  sidebar <- home$sidebar
  if (is.list(sidebar)) {
    warn_unknown_fields(
      names(sidebar),
      c("structure", "components"),
      "home: sidebar"
    )
    known <- c(sidebar_sections, names(sidebar$components))
    for (name in setdiff(unlist(sidebar$structure), known)) {
      config_abort(
        "home: sidebar: structure",
        c(
          "has {.val {name}}, which isn't a section.",
          "i" = "Sections: {.val {sidebar_sections}}, or one of your own under `components:`."
        )
      )
    }
    for (name in names(sidebar$components)) {
      component <- sidebar$components[[name]]
      if (!is.character(component$title) || !is.character(component$text)) {
        config_abort(
          "home: sidebar: components: {name}",
          "needs a {.field title} and a {.field text}."
        )
      }
    }
  } else if (!is.null(sidebar) && !isTRUE(sidebar) && !isFALSE(sidebar)) {
    config_abort(
      "home: sidebar",
      "must be true, false, or a list with {.field structure} and {.field components}."
    )
  }
  for (link in home$links) {
    if (!is.character(link$text) || !is.character(link$href)) {
      config_abort(
        "home: links",
        "entries each need a {.field text} and an {.field href}."
      )
    }
  }
  invisible()
}

# A home page for a package without a README: its name and description.
description_home <- function(path) {
  pkg <- pkg_name(path)
  description <- desc::desc_get_field("Description", default = "", file = path)
  c(paste("#", pkg), "", squish(description))
}

# With _quartodown.yml's `home: sidebar: false`, the page is `lines` as
# written, only made to render like on GitHub. Its code runs only if it's a
# .qmd (`qmd = TRUE`).
build_website_readme <- function(
  lines,
  path = ".",
  config = list(),
  qmd = FALSE
) {
  # A .qmd's front matter stays at the top.
  parts <- split_front_matter(lines)
  lines <- github_markdown_to_quarto(
    parts$body,
    mermaid = TRUE,
    executable = qmd
  )
  if (!qmd) {
    lines <- escape_knitr_blocks(lines)
  }
  lines <- link_community_files(lines, path)
  lines <- theme_pictures_to_images(lines)
  if (!isFALSE(config$home$sidebar)) {
    lines <- home_page_with_sidebar(lines, path, config)
  }
  c(parts$front_matter, lines)
}

home_page_with_sidebar <- function(lines, path, config = list()) {
  parts <- split_readme_badges(lines)
  badges <- parts$badges
  lines <- parts$lines

  # Not a `# comment` in a code block.
  in_code <- nzchar(code_block_lines(lines))
  title_idx <- which(grepl("^# ", lines) & !in_code)[1]
  if (is.na(title_idx)) {
    title <- character()
  } else {
    title <- lines[title_idx]
    lines <- lines[-seq_len(title_idx)]
  }
  first_nonblank <- which(nzchar(trimws(lines)))[1]
  body <- if (is.na(first_nonblank)) {
    character()
  } else {
    lines[seq(first_nonblank, length(lines))]
  }

  sidebar <- build_sidebar_markdown(path, badges = badges, config = config)
  if (length(sidebar) == 0) {
    c(title, "", body)
  } else {
    # One grid item for title and body, so text can wrap around a logo
    # floated right in the title.
    c("::::: {.quartodown-home}", title, "", body, ":::::", "", sidebar)
  }
}

# The README's `badges` (the non-blank lines between `<!-- badges: start -->`
# and `<!-- badges: end -->`) and its other `lines`, without the block.
split_readme_badges <- function(lines) {
  start <- which(lines == "<!-- badges: start -->")
  end <- which(lines == "<!-- badges: end -->")
  if (length(start) != 1 || length(end) != 1 || end <= start) {
    return(list(badges = character(), lines = lines))
  }
  badges <- lines[seq_len(end - start - 1) + start]
  list(badges = badges[nzchar(trimws(badges))], lines = lines[-seq(start, end)])
}

# ---- light/dark images --------------------------------------------------------
#
# A README `<picture>` with a `prefers-color-scheme` `<source>` would follow
# the OS rather than the site's toggle, and Quarto doesn't copy `srcset`
# images. Each becomes one `.light-content`/`.dark-content` `<img>` instead.

theme_pictures_to_images <- function(lines) {
  if (!any(grepl("<picture", lines, fixed = TRUE))) {
    return(lines)
  }
  text <- gsub_fn(
    paste(lines, collapse = "\n"),
    "(?s)<picture\\b.*?</picture>",
    function(picture, groups) theme_picture_to_images(picture)
  )
  split_lines(text)
}

theme_picture_to_images <- function(picture) {
  img <- regmatches(picture, regexpr("<img\\b[^>]*>", picture, perl = TRUE))
  if (length(img) == 0) {
    return(picture)
  }
  sources <- regmatches(
    picture,
    gregexpr("<source\\b[^>]*>", picture, perl = TRUE)
  )[[1]]
  media <- vapply(
    sources,
    html_attr,
    character(1),
    name = "media",
    USE.NAMES = FALSE
  )
  scheme_pattern <- "^.*prefers-color-scheme\\s*:\\s*(light|dark).*$"
  is_scheme <- !is.na(media) & grepl(scheme_pattern, media, perl = TRUE)
  if (!any(is_scheme)) {
    return(picture)
  }
  schemes <- sub(scheme_pattern, "\\1", media[is_scheme], perl = TRUE)
  sources <- sources[is_scheme][!duplicated(schemes)]
  schemes <- unique(schemes)

  images <- vapply(
    seq_along(sources),
    function(i) {
      srcset <- html_attr(sources[i], "srcset")
      # The first candidate's URL, without a width or density descriptor.
      src <- sub(
        "\\s.*$",
        "",
        trimws(strsplit(srcset, ",", fixed = TRUE)[[1]][1])
      )
      with_theme_class(set_html_attr(img, "src", src), schemes[i])
    },
    character(1)
  )
  # The <img> is the fallback, so it covers the scheme no <source> names.
  fallback <- setdiff(c("light", "dark"), schemes)
  if (length(fallback) == 1) {
    images <- c(with_theme_class(img, fallback), images)
  }
  paste(images, collapse = "")
}

with_theme_class <- function(tag, scheme) {
  class <- html_attr(tag, "class")
  class <- if (is.na(class)) "" else class
  set_html_attr(tag, "class", trimws(paste(class, paste0(scheme, "-content"))))
}

html_attr_pattern <- function(name) {
  sprintf("(?i)\\s%s\\s*=\\s*(\"[^\"]*\"|'[^']*')", name)
}

# The value of attribute `name` in the HTML start tag `tag`, or NA.
html_attr <- function(tag, name) {
  m <- regmatches(tag, regexec(html_attr_pattern(name), tag, perl = TRUE))[[1]]
  if (length(m) == 0) {
    return(NA_character_)
  }
  substr(m[2], 2, nchar(m[2]) - 1)
}

set_html_attr <- function(tag, name, value) {
  new <- sprintf(" %s=\"%s\"", name, value)
  m <- regexpr(html_attr_pattern(name), tag, perl = TRUE)
  if (m == -1) {
    m <- regexpr("\\s*/?>$", tag, perl = TRUE)
    regmatches(tag, m) <- paste0(new, regmatches(tag, m))
  } else {
    regmatches(tag, m) <- new
  }
  tag
}
