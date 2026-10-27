# ---- navbar -------------------------------------------------------------------
#
# `navbar:` in quartodown.yml lists the items on the navbar's `left` and
# `right`. An item is the name of a built-in one below, or a Quarto navbar
# item. A built-in item the package has nothing for (e.g. "news" without
# NEWS.md) is left out.

builtin_navbar_items <- c("intro", "reference", "articles", "news", "repo")

default_navbar <- list(
  left = list("intro", "reference", "articles", "news"),
  right = list("repo")
)

# A list of `left` and `right` Quarto navbar items.
navbar_items <- function(path, config = list()) {
  vignettes <- find_vignettes(path)
  layout <- navbar_layout(path, config, vignettes)
  items <- unlist(layout, recursive = FALSE)
  used <- unlist(Filter(is.character, items))
  listed <- nav_vignettes(Filter(Negate(is.character), items))

  builtins <- list()
  if ("intro" %in% used) {
    intro <- find_getting_started_vignette(path, setdiff(vignettes, listed))
    if (!is.null(intro)) {
      builtins$intro <- nav_vignette(path, intro, text = "Get started")
      listed <- c(listed, intro)
    }
  }
  if ("reference" %in% used) {
    builtins$reference <- list(text = "Reference", file = "reference/index.qmd")
  }
  unlisted <- setdiff(vignettes, listed)
  if ("articles" %in% used) {
    if (length(unlisted) > 0) {
      builtins$articles <- list(
        text = "Articles",
        menu = lapply(unlisted, nav_vignette, path = path)
      )
    }
  } else if (length(unlisted) > 0) {
    cli::cli_warn(c(
      "{.val {strip_ext(unlisted)}} {?is/are} not in the navbar.",
      "i" = "List vignettes under `navbar:` in _quartodown.yml, or add {.val articles} for those no other item links."
    ))
  }
  if ("news" %in% used && file.exists(file.path(path, "NEWS.md"))) {
    builtins$news <- list(text = "Changelog", file = "NEWS.md")
  }
  if ("repo" %in% used) {
    home <- pkg_repo(path, config)$home
    if (!is.na(home)) {
      host <- repo_host(home)
      labels <- c(github = "GitHub", gitlab = "GitLab", codeberg = "Codeberg")
      builtins$repo <- list(
        # Bootstrap Icons has none for Codeberg or other hosts.
        icon = if (host %in% c("github", "gitlab")) host else "git",
        href = home,
        `aria-label` = if (is.na(host)) "Source code" else labels[[host]]
      )
    }
  }

  lapply(layout, function(side) {
    side <- lapply(side, function(item) {
      if (is.character(item)) builtins[[item]] else item
    })
    Filter(Negate(is.null), side)
  })
}

# The `left` and `right` items of `navbar:`, with every item but the
# built-in ones (still their name) made a Quarto navbar item. These go first:
# "Get started" and "Articles" skip the vignettes they link.
navbar_layout <- function(
  path,
  config = list(),
  vignettes = find_vignettes(path)
) {
  layout <- default_navbar
  for (side in setdiff(names(config$navbar), "style")) {
    if (!side %in% names(default_navbar)) {
      config_abort(
        "navbar",
        "has {.field {side}}, but only {.field left}, {.field right} and {.field style}."
      )
    }
    layout[[side]] <- as.list(config$navbar[[side]])
  }
  lapply(layout, lapply, function(item) {
    if (is_builtin_item(item)) item else nav_item(item, path, vignettes)
  })
}

is_builtin_item <- function(item) {
  is_string(item) && item %in% builtin_navbar_items
}

# A `navbar:` item as a Quarto navbar item. A string is a vignette (its path
# relative to vignettes/, without extension); in a `menu:`, a string ending in
# "/" is every vignette in that folder. Other items are Quarto's own.
nav_item <- function(item, path, vignettes) {
  if (is_string(item)) {
    return(nav_vignette(path, match_vignette(item, vignettes)))
  }
  if (!is.null(item$menu)) {
    item$menu <- unlist(
      lapply(as.list(item$menu), function(entry) {
        if (is.character(entry) && endsWith(entry, "/")) {
          lapply(match_folder(entry, vignettes), nav_vignette, path = path)
        } else {
          list(nav_item(entry, path, vignettes))
        }
      }),
      recursive = FALSE
    )
  }
  item
}

nav_vignette <- function(path, vignette, text = NULL) {
  if (is.null(text)) {
    text <- vignette_title(file.path(path, "vignettes", vignette))
  }
  list(text = text, file = file.path("vignettes", vignette))
}

# The vignettes the navbar items link to.
nav_vignettes <- function(items) {
  files <- as.character(unlist(lapply(items, function(item) {
    c(item$file, unlist(lapply(item$menu, function(m) m$file)))
  })))
  sub("^vignettes/", "", files[startsWith(files, "vignettes/")])
}

match_vignette <- function(name, vignettes) {
  hit <- vignettes[strip_ext(vignettes) == strip_ext(name)]
  if (length(hit) == 0) {
    config_abort(
      "navbar",
      c(
        "lists {.val {name}}, which is not a vignette.",
        "i" = "Known vignettes: {.val {strip_ext(vignettes)}}.",
        "i" = "Built-in items: {.val {builtin_navbar_items}}."
      )
    )
  }
  hit[[1]]
}

match_folder <- function(folder, vignettes) {
  hit <- vignettes[startsWith(vignettes, folder)]
  if (length(hit) == 0) {
    config_abort(
      "navbar",
      "lists {.val {folder}}, but {.path vignettes/{folder}} has no vignettes."
    )
  }
  hit
}

# ---- sidebar ------------------------------------------------------------------
#
# With `navbar: style: sidebar`, the navbar keeps the title, search and its
# `right` items, and its `left` items go in a docked sidebar on every page,
# menus as sections. The reference index's entry lists every reference page,
# grouped as on the index.

navbar_style <- function(config = list()) {
  style <- config$navbar$style
  if (is.null(style)) {
    return("top")
  }
  if (!identical(style, "top") && !identical(style, "sidebar")) {
    config_abort(
      "navbar: style",
      "must be {.val top} or {.val sidebar}."
    )
  }
  style
}

# `items` are the navbar's `left` items, from navbar_items().
sidebar_contents <- function(path, config, items, topics = rd_topics(path)) {
  lapply(items, function(item) {
    if (identical(item$file, "reference/index.qmd")) {
      contents <- sidebar_reference(config, topics)
      if (length(contents) > 0) {
        return(list(section = item$text, href = item$file, contents = contents))
      }
    }
    sidebar_item(item)
  })
}

# A Quarto navbar item as a sidebar item: a menu becomes a section.
sidebar_item <- function(item) {
  if (!is.null(item$menu)) {
    return(list(
      section = item$text,
      contents = lapply(item$menu, sidebar_item)
    ))
  }
  if (!is.null(item$file)) {
    item$href <- item$file
    item$file <- NULL
  }
  item
}

# Every topic the reference index lists, by its first name (`f()`), under a
# section per titled group of the index. With a single group, they're listed
# without one.
sidebar_reference <- function(config, topics) {
  blocks <- reference_blocks(config, topics)
  groups <- Map(
    function(block, idx) {
      entries <- lapply(unname(topics[idx]), function(topic) {
        list(
          text = topic_label(topic),
          href = paste0("reference/", topic$basename, ".qmd")
        )
      })
      label <- if (is.null(block$subtitle)) block$title else block$subtitle
      if (length(entries) == 0 || is.null(label) || length(blocks) == 1) {
        entries
      } else {
        list(list(section = label, contents = entries))
      }
    },
    blocks,
    reference_block_topics(blocks, topics)
  )
  unlist(groups, recursive = FALSE)
}

# A topic's first name from its usage, as plain text: inline code would get
# a tooltip link inside the sidebar's link (see add_tooltips_html()), so
# quartodown.scss sets it in the code font instead.
topic_label <- function(topic) {
  html_unescape(gsub("<[^>]+>", "", topic$names[[1]]))
}
