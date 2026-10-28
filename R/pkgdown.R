# ---- _pkgdown.yml -> _quartodown.yml ------------------------------------------
#
# use_quartodown() translates an existing pkgdown configuration: fields of the
# same format are copied, the navbar and `articles:` become quartodown's
# navbar, template settings become `theme:`, `quarto:` settings and files in
# _quartodown/, and redirects keep pkgdown's addresses of the vignettes and
# changelog working. What has no equivalent is listed in `skipped`, for
# use_quartodown() to report.

pkgdown_config_files <- c(
  "_pkgdown.yml",
  "_pkgdown.yaml",
  "pkgdown/_pkgdown.yml",
  "pkgdown/_pkgdown.yaml",
  "inst/_pkgdown.yml",
  "inst/_pkgdown.yaml"
)

# The package's pkgdown configuration file, relative to `path`, or NULL.
find_pkgdown_config <- function(path) {
  found <- pkgdown_config_files[file.exists(file.path(
    path,
    pkgdown_config_files
  ))]
  if (length(found) == 0) NULL else found[[1]]
}

# The translation of `<path>/<file>`: `config`, the _quartodown.yml fields;
# `files`, the lines of each file to write into _quartodown/, by name; and
# `skipped`, a description of each setting with no equivalent.
translate_pkgdown <- function(path, file) {
  pkgdown <- yaml::read_yaml(file.path(path, file))
  if (!is.list(pkgdown)) {
    pkgdown <- list()
  }
  skipped <- character()
  skip <- function(field, note = NULL) {
    skipped <<- c(
      skipped,
      paste0("`", field, "`", if (!is.null(note)) paste0(": ", note))
    )
  }
  config <- list()
  quarto <- list()
  files <- list()
  vignettes <- find_vignettes(path)

  for (field in c("url", "authors", "repo", "reference")) {
    config[[field]] <- pkgdown[[field]]
  }

  development <- pkgdown$development
  if (is.list(development)) {
    if (!is.null(development$mode)) {
      config$development <- list(mode = development$mode)
    }
    for (field in setdiff(names(development), "mode")) {
      skip(paste0("development: ", field))
    }
  }

  navbar <- pkgdown_navbar(path, pkgdown, vignettes)
  config$navbar <- navbar$navbar
  skipped <- c(skipped, navbar$skipped)

  home <- pkgdown$home
  if (is.list(home)) {
    config$home <- home[intersect(names(home), c("sidebar", "links"))]
    if (length(config$home) == 0) {
      config$home <- NULL
    }
    if (is_string(home$description)) {
      quarto$website$description <- home$description
    }
    for (field in setdiff(names(home), c("sidebar", "links", "description"))) {
      skip(
        paste0("home: ", field),
        if (field == "title") "the home page's title is the package's"
      )
    }
  }
  # The site's name, on the navbar.
  if (is_string(pkgdown$title)) {
    quarto$website$navbar$title <- pkgdown$title
  }

  template <- pkgdown$template
  if (is.list(template)) {
    theme <- template$bootswatch %||%
      template$bslib$preset %||%
      template$bslib$bootswatch
    if (is_string(theme)) {
      config$theme <- list(light = theme)
    }
    scss <- pkgdown_bslib_scss(template$bslib)
    includes <- pkgdown_includes(template$includes)
    files <- c(files, includes$files)
    quarto$format$html <- c(quarto$format$html, includes$quarto)
    skipped <- c(skipped, includes$skipped)
    quarto$website <- c(quarto$website, pkgdown_opengraph(template$opengraph))
    math <- template$`math-rendering`
    if (is_string(math)) {
      quarto$format$html$`html-math-method` <- math
    }
    ganalytics <- template$params$ganalytics
    if (is_string(ganalytics)) {
      quarto$website$`google-analytics` <- ganalytics
    }
    for (param in setdiff(names(template$params), "ganalytics")) {
      skip(paste0("template: params: ", param))
    }
    handled <- c(
      "bootswatch",
      "bslib",
      "includes",
      "opengraph",
      "math-rendering",
      "params",
      "bootstrap",
      "light-switch"
    )
    for (field in setdiff(names(template), handled)) {
      skip(
        paste0("template: ", field),
        if (field == "package") "quartodown has its own theme"
      )
    }
  } else {
    scss <- NULL
  }

  # pkgdown/extra.scss holds rules; quartodown's needs Quarto's sections.
  extra_scss <- file.path(path, "pkgdown", "extra.scss")
  if (file.exists(extra_scss)) {
    scss <- c(scss, "/*-- scss:rules --*/", read_utf8(extra_scss))
  }
  if (length(scss) > 0) {
    files[["extra.scss"]] <- scss
  }
  extra_css <- file.path(path, "pkgdown", "extra.css")
  if (file.exists(extra_css)) {
    files[["extra.css"]] <- read_utf8(extra_css)
  }
  if (file.exists(file.path(path, "pkgdown", "extra.js"))) {
    skip(
      "pkgdown/extra.js",
      "add it with `quarto: format: html: include-after-body:`"
    )
  }

  if (is_string(pkgdown$lang)) {
    quarto$lang <- pkgdown$lang
  }
  if (!is.null(pkgdown$toc$depth)) {
    quarto$format$html$`toc-depth` <- pkgdown$toc$depth
  }
  figures <- pkgdown$figures
  figure_fields <- c(
    fig.width = "fig-width",
    fig.height = "fig-height",
    dpi = "fig-dpi"
  )
  for (field in names(figures)) {
    if (field %in% names(figure_fields)) {
      quarto$format$html[[figure_fields[[field]]]] <- figures[[field]]
    } else {
      skip(paste0("figures: ", field))
    }
  }
  if (!is.null(pkgdown$footer)) {
    skip(
      "footer",
      "set the footer's roles with `authors: footer:`, or `quarto: website: page-footer:`"
    )
  }
  if (!is.null(pkgdown$news)) {
    skip("news", "the changelog is NEWS.md as one page")
  }

  handled <- c(
    "url",
    "authors",
    "repo",
    "reference",
    "redirects",
    "development",
    "navbar",
    "articles",
    "home",
    "title",
    "template",
    "lang",
    "toc",
    "figures",
    "footer",
    "news"
  )
  for (field in setdiff(names(pkgdown), handled)) {
    skip(field, if (field == "destination") "the site is built into docs/")
  }

  config$redirects <- pkgdown_redirects(path, pkgdown$redirects, vignettes)
  config$quarto <- drop_empty(quarto)

  # pkgdown builds every topic's page, which links may rely on.
  links <- internal_links(config, rd_topics(path), pkg_name(path))
  if (length(links$rd) > 0 || length(links$navbar) > 0) {
    config$`internal-pages` <- TRUE
  }

  list(config = config, files = files, skipped = skipped)
}

# `x` without its empty lists, at any depth; NULL if nothing is left.
drop_empty <- function(x) {
  if (!is.list(x)) {
    return(x)
  }
  x <- Filter(Negate(is.null), lapply(x, drop_empty))
  if (length(x) == 0) NULL else x
}

# ---- navbar ---------------------------------------------------------------------

# pkgdown's defaults; `search` and `lightswitch` are always there in
# quartodown.
pkgdown_navbar_structure <- list(
  left = c("intro", "reference", "articles", "tutorials", "news"),
  right = c("search", "github", "lightswitch")
)

# `navbar:` from pkgdown's `navbar:` and `articles:`: `navbar`, and `skipped`.
pkgdown_navbar <- function(path, pkgdown, vignettes) {
  navbar <- pkgdown$navbar
  skipped <- character()
  structure <- pkgdown_navbar_structure
  components <- list()
  if (is.list(navbar)) {
    for (side in c("left", "right")) {
      if (!is.null(navbar$structure[[side]])) {
        structure[[side]] <- unlist(navbar$structure[[side]])
      } else if (is.list(navbar[[side]])) {
        # pkgdown's older format: the items themselves.
        components[paste0(side, seq_along(navbar[[side]]))] <- navbar[[side]]
        structure[[side]] <- paste0(side, seq_along(navbar[[side]]))
      }
    }
    components[names(navbar$components)] <- navbar$components
    for (field in intersect(names(navbar), c("type", "bg"))) {
      skipped <- c(
        skipped,
        paste0("`navbar: ", field, "`: use `theme:` or _quartodown/extra.scss")
      )
    }
  }

  out <- list(left = list(), right = list(), tools = list())
  add <- function(side, item) {
    out[[side]][[length(out[[side]]) + 1]] <<- item
  }
  for (side in c("left", "right")) {
    for (name in structure[[side]]) {
      component <- components[[name]]
      if (!is.null(component)) {
        item <- pkgdown_nav_item(component, path, vignettes)
        skipped <- c(skipped, item$skipped)
        item <- item$item
        if (side == "right" && !is.null(item$icon)) {
          add("tools", item)
        } else {
          add(side, item)
        }
      } else if (name %in% c("intro", "reference", "news")) {
        add(side, name)
      } else if (name == "articles") {
        add(side, pkgdown_articles_menu(path, pkgdown, vignettes, structure))
      } else if (name %in% c("github", "gitlab")) {
        add("tools", "repo")
      } else if (name == "tutorials") {
        # In pkgdown's default navbar; only worth a mention with tutorials.
        if (dir.exists(file.path(path, "inst", "tutorials"))) {
          skipped <- c(skipped, "`navbar: tutorials`")
        }
      } else if (!name %in% c("search", "lightswitch", "home")) {
        skipped <- c(skipped, paste0("`navbar: ", name, "`: no such component"))
      }
    }
  }
  list(navbar = Filter(length, out), skipped = skipped)
}

# A pkgdown navbar item (or menu entry) as quartodown's: `item`, and
# `skipped`.
pkgdown_nav_item <- function(item, path, vignettes) {
  skipped <- character()
  out <- item[intersect(names(item), c("text", "icon", "aria-label"))]
  if (is_string(out$text) && grepl("^-{3,}$", out$text)) {
    out$text <- "---"
  }
  if (!is.null(out$icon)) {
    out$icon <- pkgdown_icon(out$icon)
  }
  if (is_string(item$href)) {
    href <- pkgdown_href(item$href, path, vignettes)
    if (is.null(href)) {
      skipped <- c(
        skipped,
        paste0("navbar link to `", item$href, "`: no such page")
      )
      out$href <- item$href
    } else if (!is.null(href$vignette)) {
      title <- vignette_title(file.path(path, "vignettes", href$vignette))
      if (identical(out$text, title) && is.null(out$icon)) {
        # Linked with its title, the vignette's path is enough.
        return(list(item = strip_ext(href$vignette), skipped = skipped))
      }
      out$href <- file.path("vignettes", href$vignette)
    } else {
      out$href <- href$href
    }
  }
  if (!is.null(item$menu)) {
    entries <- lapply(
      item$menu,
      pkgdown_nav_item,
      path = path,
      vignettes = vignettes
    )
    out$menu <- lapply(entries, `[[`, "item")
    skipped <- c(skipped, unlist(lapply(entries, `[[`, "skipped")))
  }
  list(item = out, skipped = skipped)
}

# A Font Awesome icon such as "fab fa-twitter fa-lg" as the name of the
# Bootstrap Icons icon Quarto takes, e.g. "twitter".
pkgdown_icon <- function(icon) {
  classes <- strsplit(icon, "\\s+")[[1]]
  names <- sub("^fa-", "", classes[grepl("^fa-", classes)])
  sizes <- "^(lg|xs|sm|xl|fw|[0-9]+x|solid|regular|brands)$"
  name <- utils::head(names[!grepl(sizes, names)], 1)
  if (length(name) == 0) {
    return(icon)
  }
  renamed <- c(
    "x-twitter" = "twitter-x",
    "home" = "house",
    "house" = "house",
    "envelope" = "envelope",
    "comments" = "chat",
    "comment" = "chat"
  )
  if (name %in% names(renamed)) renamed[[name]] else name
}

# A link in pkgdown's site as one in quartodown's: `list(href = )`, or
# `list(vignette = )` (its path in vignettes/) for an article. NULL for a
# page quartodown doesn't make.
pkgdown_href <- function(href, path, vignettes) {
  if (is_url(href) || startsWith(href, "#")) {
    return(list(href = href))
  }
  anchor <- if (grepl("#", href, fixed = TRUE)) sub("^[^#]*", "", href) else ""
  page <- sub("^(\\./|/)+", "", sub("#.*", "", href))
  page <- sub("(^|/)$", "\\1index.html", page)
  with_anchor <- function(x) list(href = paste0(x, anchor))

  if (page == "index.html") {
    return(with_anchor("index.qmd"))
  }
  if (page == "news/index.html") {
    return(with_anchor("NEWS.md"))
  }
  if (grepl("^reference/[^/]+\\.html$", page)) {
    return(with_anchor(sub("\\.html$", ".qmd", page)))
  }
  if (grepl("^articles/.+\\.html$", page) && page != "articles/index.html") {
    vignette <- pkgdown_article_vignette(
      sub("^articles/(.*)\\.html$", "\\1", page),
      vignettes
    )
    if (!is.null(vignette)) {
      return(
        if (nzchar(anchor)) {
          with_anchor(file.path("vignettes", vignette))
        } else {
          list(vignette = vignette)
        }
      )
    }
    return(NULL)
  }
  if (grepl("^[^/]+\\.html$", page)) {
    md <- sub("\\.html$", ".md", page)
    if (page == "authors.html") {
      return(with_anchor("authors.qmd"))
    }
    if (
      page == "LICENSE-text.html" && file.exists(file.path(path, "LICENSE"))
    ) {
      return(with_anchor(md))
    }
    if (
      file.exists(file.path(path, md)) ||
        file.exists(file.path(path, ".github", md))
    ) {
      return(with_anchor(md))
    }
  }
  NULL
}

# The vignette pkgdown publishes as articles/<name>.html: vignettes/<name>,
# or vignettes/articles/<name>.
pkgdown_article_vignette <- function(name, vignettes) {
  names <- strip_ext(vignettes)
  match <- vignettes[names == name | names == paste0("articles/", name)]
  if (length(match) == 0) NULL else match[[1]]
}

# The "Articles" menu, from `articles:`'s sections: a header and the vignettes
# of each, the sections apart. Without `articles:`, the `articles` keyword.
pkgdown_articles_menu <- function(path, pkgdown, vignettes, structure) {
  sections <- pkgdown$articles
  if (!is.list(sections) || length(sections) == 0) {
    return("articles")
  }
  names <- strip_ext(vignettes)
  # "Get started" links this one.
  if ("intro" %in% unlist(structure)) {
    intro <- find_getting_started_vignette(path, vignettes)
    names <- setdiff(names, strip_ext(intro))
  }
  menu <- list()
  listed <- character()
  for (section in sections) {
    picked <- setdiff(pkgdown_select_articles(section$contents, names), listed)
    if (length(picked) == 0) {
      next
    }
    if (length(menu) > 0) {
      menu <- c(menu, list(list(text = "---")))
    }
    # pkgdown's `navbar:` heading, none if empty, else the section's title.
    header <- if ("navbar" %in% names(section)) {
      section$navbar
    } else {
      section$title
    }
    if (is_string(header) && !nzchar(header)) {
      header <- NULL
    }
    if (is_string(header)) {
      menu <- c(menu, list(list(text = header)))
    }
    menu <- c(menu, as.list(picked))
    listed <- c(listed, picked)
  }
  list(text = "Articles", menu = menu)
}

# The vignettes (paths without extension) an `articles:` section's
# `contents:` select: names, or pkgdown's selectors.
pkgdown_select_articles <- function(contents, names) {
  env <- new.env(parent = emptyenv())
  env$starts_with <- function(x) names[startsWith(names, x)]
  env$ends_with <- function(x) names[endsWith(names, x)]
  env$contains <- function(x) names[grepl(x, names, fixed = TRUE)]
  env$matches <- function(x) names[grepl(x, names)]
  unique(unlist(lapply(unlist(contents), function(entry) {
    if (entry %in% names) {
      return(entry)
    }
    tryCatch(
      intersect(eval(str2lang(entry), env), names),
      error = function(e) character()
    )
  })))
}

# ---- template ---------------------------------------------------------------------

# `template: bslib:` as the lines of a scss:defaults section: Bootstrap
# variables, and fonts, with an import for those on Google Fonts.
pkgdown_bslib_scss <- function(bslib) {
  if (!is.list(bslib)) {
    return(NULL)
  }
  vars <- c(
    bg = "body-bg",
    fg = "body-color",
    base_font = "font-family-sans-serif",
    heading_font = "headings-font-family",
    code_font = "font-family-monospace"
  )
  imports <- character()
  lines <- character()
  for (name in setdiff(names(bslib), c("version", "preset", "bootswatch"))) {
    value <- bslib[[name]]
    if (is.list(value) && !is.null(value$google)) {
      family <- if (is.list(value$google)) value$google$family else value$google
      imports <- c(imports, gsub(" ", "+", family))
      fallback <- if (name == "code_font") "monospace" else "sans-serif"
      value <- sprintf('"%s", %s', family, fallback)
    }
    if (!is.atomic(value) || length(value) != 1) {
      next
    }
    var <- if (name %in% names(vars)) vars[[name]] else gsub("_", "-", name)
    lines <- c(lines, sprintf("$%s: %s;", var, value))
  }
  if (length(lines) == 0) {
    return(NULL)
  }
  c(
    "/*-- scss:defaults --*/",
    if (length(imports) > 0) {
      sprintf(
        '@import url("https://fonts.googleapis.com/css2?%s&display=swap");',
        paste0("family=", imports, collapse = "&")
      )
    },
    lines
  )
}

# `template: includes:` as files in _quartodown/ and the Quarto settings that
# include them: `files`, `quarto` and `skipped`.
pkgdown_includes <- function(includes) {
  where <- c(
    in_header = "include-in-header",
    before_body = "include-before-body",
    after_body = "include-after-body"
  )
  files <- list()
  quarto <- list()
  skipped <- character()
  for (name in names(includes)) {
    if (!name %in% names(where)) {
      skipped <- c(skipped, paste0("`template: includes: ", name, "`"))
      next
    }
    file <- paste0(gsub("_", "-", name), ".html")
    files[[file]] <- split_lines(includes[[name]])
    quarto[[where[[name]]]] <- file.path(config_dir, file)
  }
  if (length(files) > 0) {
    skipped <- c(
      skipped,
      paste0(
        "`template: includes`: copied to ",
        paste0("_quartodown/", names(files), collapse = ", "),
        "; check what they load, such as analytics"
      )
    )
  }
  list(files = files, quarto = quarto, skipped = skipped)
}

# `template: opengraph:` as Quarto's `website:` settings.
pkgdown_opengraph <- function(opengraph) {
  if (!is.list(opengraph)) {
    return(list())
  }
  out <- list()
  image <- opengraph$image$src
  if (is_string(image)) {
    out$`open-graph` <- list(image = image)
    if (is_string(opengraph$image$alt)) {
      out$`open-graph`$`image-alt` <- opengraph$image$alt
    }
  } else if (!is.null(opengraph)) {
    out$`open-graph` <- TRUE
  }
  twitter <- opengraph$twitter
  if (is.list(twitter)) {
    out$`twitter-card` <- Filter(
      Negate(is.null),
      list(
        creator = twitter$creator,
        site = twitter$site,
        `card-style` = twitter$card
      )
    )
    if (is_string(image)) {
      out$`twitter-card`$image <- image
    }
  }
  out
}

# ---- redirects ----------------------------------------------------------------------

# pkgdown's `redirects:`, with targets moved to quartodown's addresses, and
# pkgdown's addresses of the vignettes and changelog redirected to them, as
# a list of pairs.
pkgdown_redirects <- function(path, redirects, vignettes) {
  moved <- character()
  for (vignette in strip_ext(vignettes)) {
    old <- paste0("articles/", sub("^articles/", "", vignette), ".html")
    if (!old %in% names(moved)) {
      moved[[old]] <- paste0("vignettes/", vignette, ".html")
    }
  }
  if (file.exists(file.path(path, "NEWS.md"))) {
    moved[["news/index.html"]] <- "NEWS.html"
  }

  pairs <- lapply(redirects, function(pair) {
    to <- unlist(pair)[[2]]
    target <- sub("^/+", "", sub("#.*", "", to))
    if (target %in% names(moved)) {
      to <- paste0(moved[[target]], sub("^[^#]*", "", to))
    }
    c(unlist(pair)[[1]], to)
  })
  from <- vapply(pairs, `[[`, character(1), 1)
  for (old in setdiff(names(moved), sub("^/+", "", from))) {
    pairs[[length(pairs) + 1]] <- c(old, moved[[old]])
  }
  if (length(pairs) == 0) NULL else pairs
}

# ---- writing ----------------------------------------------------------------------

# The lines of _quartodown.yml for the translated `config`.
pkgdown_config_yml <- function(config, file) {
  comments <- list(
    `internal-pages` = "# Pages for internal topics too, which some links go to, as pkgdown builds.",
    redirects = "# pkgdown's addresses of the vignettes and changelog, now under vignettes/\n# and NEWS.html."
  )
  order <- c(
    "url",
    "internal-pages",
    "development",
    "navbar",
    "home",
    "authors",
    "repo",
    "theme",
    "reference",
    "redirects",
    "quarto"
  )
  fields <- lapply(intersect(order, names(config)), function(field) {
    yml <- if (field == "redirects") {
      c(
        "redirects:",
        vapply(
          config$redirects,
          function(pair) {
            sprintf(
              "  - [%s, %s]",
              yaml_string(pair[[1]]),
              yaml_string(pair[[2]])
            )
          },
          character(1)
        )
      )
    } else {
      split_lines(sub(
        "\n$",
        "",
        yaml::as.yaml(
          config[field],
          indent.mapping.sequence = TRUE,
          # Else `yes`/`no`, YAML 1.1's spelling.
          handlers = list(logical = function(x) {
            structure(ifelse(x, "true", "false"), class = "verbatim")
          })
        )
      ))
    }
    c(if (!is.null(comments[[field]])) split_lines(comments[[field]]), yml, "")
  })
  c(
    sprintf(
      "# Configuration for quartodown::build_site(), translated from %s by",
      file
    ),
    "# use_quartodown(). See vignette(\"customizing\", package = \"quartodown\").",
    "",
    utils::head(unlist(fields), -1)
  )
}

yaml_string <- function(x) {
  sub("\n$", "", yaml::as.yaml(x))
}
