# ---- shared helpers --------------------------------------------------------

role_lookup <- function(abbr) {
  roles <- c(
    aut = "author",
    com = "compiler",
    ctr = "contractor",
    ctb = "contributor",
    cph = "copyright holder",
    cre = "maintainer",
    dtc = "data contributor",
    fnd = "funder",
    rev = "reviewer",
    ths = "thesis advisor",
    trl = "translator"
  )
  # Any other code as is, rather than "NA".
  ifelse(abbr %in% names(roles), roles[abbr], abbr)
}

# Default roles shown in the sidebar/footer.
default_roles <- function() c("aut", "cre", "fnd")

pkg_authors <- function(path, roles = NULL) {
  authors <- unclass(desc::desc_get_authors(path))
  if (is.null(roles)) {
    return(authors)
  }
  Filter(function(a) any(a$role %in% roles), authors)
}

author_name <- function(x) {
  given <- trimws(paste(x$given, collapse = " "))
  if (is.null(x$family)) given else paste(given, x$family)
}

# A person's name as shown on the site. _quartodown.yml's `authors:` can, by
# the name, link it (`href`) or replace it with HTML (`html`), such as a logo.
author_display <- function(x, config = list()) {
  name <- author_name(x)
  meta <- config$authors[[name]]
  text <- if (is.null(meta$html)) name else meta$html
  if (is.null(meta$href)) {
    text
  } else {
    sprintf("<a href='%s'>%s</a>", meta$href, text)
  }
}

# The roles shown in the sidebar or footer (`where`): _quartodown.yml's
# `authors: <where>: roles:`, else the default ones.
author_roles <- function(config, where) {
  roles <- config$authors[[where]]$roles
  if (is.null(roles)) default_roles() else unlist(roles)
}

author_roles_text <- function(x) {
  roles <- paste0(role_lookup(x$role), collapse = ", ")
  substr(roles, 1, 1) <- toupper(substr(roles, 1, 1))
  roles
}

# ---- sidebar section builders ------------------------------------------------

# A heading and bullet list, or nothing without bullets.
sidebar_section <- function(heading, bullets) {
  if (length(bullets) == 0) {
    return(character())
  }
  c(
    paste0("#### ", heading),
    "",
    paste(bullets, collapse = "\n\n")
  )
}

# Whether the package is on CRAN: TRUE, FALSE, or NA when CRAN can't be
# reached (or httr2 isn't installed). Looked up once per session, so
# rebuilds don't wait on CRAN.
on_cran <- function(pkg) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    return(NA)
  }
  if (!is.null(the$on_cran[[pkg]])) {
    return(the$on_cran[[pkg]])
  }
  resp <- tryCatch(
    httr2::req_perform(httr2::req_error(
      httr2::req_timeout(httr2::request(cran_url(pkg)), 5),
      function(resp) FALSE
    )),
    error = function(e) NULL
  )
  # Not when offline: the next build looks again.
  if (is.null(resp)) {
    return(NA)
  }
  the$on_cran[[pkg]] <- !httr2::resp_is_error(resp)
  the$on_cran[[pkg]]
}

cran_url <- function(pkg) paste0("https://cloud.r-project.org/package=", pkg)

# The host a repository URL is on: "github", "gitlab" (gitlab.com or a
# self-hosted GitLab, whose host name says so) or "codeberg". NA otherwise.
repo_host <- function(urls) {
  host <- tolower(sub("^https?://(www\\.)?([^/]+).*$", "\\2", urls))
  out <- rep(NA_character_, length(urls))
  out[which(host == "github.com")] <- "github"
  out[grep("(^|\\.)gitlab\\.", host)] <- "gitlab"
  out[which(host == "codeberg.org")] <- "codeberg"
  out
}

# Where the package's source lives, from the first GitHub, GitLab or Codeberg
# URL in DESCRIPTION (else its `BugReports`, without the `/issues` part),
# with _quartodown.yml's `repo:` over it: `home`, the repository's page, and
# the prefixes of the links to a file (`source`), an issue (`issue`) and a
# user (`user`). A URL into a folder, `<repo>/tree/<branch>/<folder>`
# (Codeberg's `<repo>/src/branch/...`), is a package in that folder of that
# branch. Each is NA if unknown.
pkg_repo <- function(path, config = list()) {
  urls <- desc::desc_get_urls(path)
  url <- urls[!is.na(repo_host(urls))][1]
  if (is.na(url)) {
    bug_reports <- desc::desc_get_field("BugReports", default = NA, file = path)
    if (!is.na(repo_host(bug_reports))) {
      url <- sub("(/-)?/issues(/.*)?$", "", trimws(bug_reports))
    }
  }
  repo <- list(
    home = NA_character_,
    source = NA_character_,
    issue = NA_character_,
    user = NA_character_
  )
  branch <- "HEAD"
  folder <- ""
  if (!is.na(url)) {
    url <- sub("/+$", "", url)
    repo$home <- url
    host <- repo_host(url)
    tree <- regmatches(
      url,
      regexec("^(.*?)(/-)?/(tree|src/branch)/([^/]+)(/.*)?$", url)
    )[[1]]
    if (length(tree) > 0) {
      url <- tree[[2]]
      branch <- tree[[5]]
      folder <- sub("^/", "", paste0(tree[[6]], "/"))
    }
    # GitLab's pages are under `/-/`.
    sep <- if (host == "gitlab") "/-/" else "/"
    repo$issue <- paste0(url, sep, "issues/")
    repo$user <- regmatches(url, regexpr("^https?://[^/]+/", url))
  }
  if (!is.null(config$repo$branch)) {
    branch <- config$repo$branch
  }
  if (!is.na(url)) {
    blob <- if (host == "codeberg") "src/branch/" else "blob/"
    repo$source <- paste0(url, sep, blob, branch, "/", folder)
  }
  for (field in names(repo)) {
    value <- config$repo$url[[field]]
    if (!is.null(value)) {
      repo[[field]] <- if (field == "home") sub("/+$", "", value) else value
    }
  }
  repo
}

# A "Source: <files>" line for a page made from `files` (relative to the
# package root). The files aren't part of the site, so each links to the
# repository, under `source` (see pkg_repo()), if known. `class` is the
# `<p>`'s.
source_note <- function(files, source = NA_character_, class = NULL) {
  links <- sprintf("<code>%s</code>", files)
  if (!is.na(source)) {
    links <- sprintf('<a href="%s%s">%s</a>', source, files, links)
  }
  sprintf(
    '<p%s><small class="dont-index">Source: %s</small></p>',
    if (is.null(class)) "" else sprintf(' class="%s"', class),
    paste(links, collapse = ", ")
  )
}

# `links` are _quartodown.yml's `home: links:`, each with `text` and `href`,
# listed after the package's own.
data_sidebar_links <- function(path, links = list(), config = list()) {
  home <- pkg_repo(path, config)$home
  bug_reports <- desc::desc_get_field("BugReports", default = NA, file = path)

  extra <- vapply(
    links,
    function(link) sprintf("[%s](%s)", link$text, link$href),
    character(1)
  )
  links <- character()
  if (isTRUE(on_cran(pkg_name(path)))) {
    links <- c(links, sprintf("[View on CRAN](%s)", cran_url(pkg_name(path))))
  }
  if (!is.na(home)) {
    links <- c(links, sprintf("[Browse source code](%s)", home))
  }
  if (!is.na(bug_reports)) {
    links <- c(links, sprintf("[Report a bug](%s)", bug_reports))
  }

  sidebar_section("Links", c(links, extra))
}

data_sidebar_devstatus <- function(badges) {
  bullets <- if (length(badges) == 0) {
    character()
  } else {
    paste(badges, collapse = "\n")
  }
  sidebar_section("Dev status", bullets)
}

# R's license database.
autolink_license <- function(license_field, has_license_file) {
  db_path <- file.path(R.home("share"), "licenses", "license.db")
  db <- as.data.frame(read.dcf(db_path), stringsAsFactors = FALSE)

  needs_sss <- !is.na(db$Abbrev) & !is.na(db$Version) & is.na(db$SSS)
  db$SSS[needs_sss] <- paste0(db$Abbrev[needs_sss], "-", db$Version[needs_sss])

  abbr <- ifelse(is.na(db$SSS), db$Abbrev, db$SSS)
  url <- db$URL
  if (has_license_file) {
    abbr <- c(abbr, "LICENSE")
    url <- c(url, "LICENSE-text.html")
  }
  keep <- !is.na(abbr)
  abbr <- abbr[keep]
  url <- url[keep]

  x <- license_field
  for (i in seq_along(abbr)) {
    pattern <- paste0("\\b", abbr[i], "\\b")
    replacement <- sprintf("[%s](%s)", abbr[i], url[i])
    x <- gsub(pattern, replacement, x, perl = TRUE)
  }
  x
}

data_sidebar_license <- function(path) {
  license_field <- desc::desc_get_field("License", default = NA, file = path)
  if (is.na(license_field)) {
    return(character())
  }

  has_license_md <- file.exists(file.path(path, "LICENSE.md"))
  has_license_file <- file.exists(file.path(path, "LICENSE"))

  link <- autolink_license(license_field, has_license_file)
  if (has_license_md) {
    link <- c(
      "[Full license](LICENSE.html)",
      sprintf("<small>%s</small>", link)
    )
  }

  sidebar_section("License", link)
}

data_sidebar_community <- function(path) {
  # Each is rendered into the site, see import_markdown_files().
  community_link <- function(text, file) {
    if (is.null(community_file_source(path, file))) {
      return(character())
    }
    sprintf("[%s](%s)", text, sub("\\.md$", ".html", file))
  }

  links <- c(
    community_link("Contributing guide", "CONTRIBUTING.md"),
    community_link("Code of conduct", "CODE_OF_CONDUCT.md"),
    community_link("Getting help", "SUPPORT.md")
  )

  sidebar_section("Community", links)
}

data_sidebar_citation <- function(path) {
  sidebar_section(
    "Citation",
    sprintf("[Citing %s](authors.html#citation)", pkg_name(path))
  )
}

# With _quartodown.yml's `authors: sidebar: before:` and `after:`, Markdown
# shown above and below the list.
data_sidebar_authors <- function(path, config = list()) {
  authors <- pkg_authors(path, author_roles(config, "sidebar"))
  if (length(authors) == 0) {
    return(character())
  }

  bullets <- vapply(
    authors,
    function(x) {
      sprintf(
        "%s\\\n<small>%s</small>%s",
        author_display(x, config),
        author_roles_text(x),
        author_id_links(x)
      )
    },
    character(1)
  )

  # Also when authors.html shows something the sidebar doesn't, such as a
  # comment.
  all_authors <- pkg_authors(path)
  has_comment <- any(lengths(lapply(all_authors, author_comment)) > 0)
  if (length(all_authors) != length(authors) || has_comment) {
    bullets <- c(bullets, "[More about authors...](authors.html)")
  }

  sidebar <- config$authors$sidebar
  sidebar_section(
    "Developers",
    c(trimws(sidebar$before), bullets, trimws(sidebar$after))
  )
}

sidebar_sections <- c(
  "links",
  "license",
  "community",
  "citation",
  "authors",
  "dev"
)

# _quartodown.yml's `home: sidebar: structure:` orders the sections, which
# are those above or `sidebar: components:` of its own, each with a `title`
# and Markdown `text`.
build_sidebar_markdown <- function(
  path,
  badges = character(),
  config = list()
) {
  home <- config$home
  sidebar <- if (is.list(home$sidebar)) home$sidebar else list()
  components <- sidebar$components
  structure <- sidebar$structure
  if (is.null(structure)) {
    structure <- c(sidebar_sections, names(components))
  }
  sections <- lapply(unlist(structure), function(name) {
    component <- components[[name]]
    if (!is.null(component)) {
      return(sidebar_section(component$title, trimws(component$text)))
    }
    switch(
      name,
      links = data_sidebar_links(path, home$links, config),
      license = data_sidebar_license(path),
      community = data_sidebar_community(path),
      citation = data_sidebar_citation(path),
      authors = data_sidebar_authors(path, config),
      # Any other name was rejected by check_home_config().
      dev = data_sidebar_devstatus(badges)
    )
  })
  sections <- Filter(length, sections)
  if (length(sections) == 0) {
    return(character())
  }

  body <- unlist(lapply(sections, function(s) c(s, "")))
  body <- body[-length(body)] # drop trailing blank line
  c("::: {.column-margin}", body, ":::")
}

# ---- footer ------------------------------------------------------------------

# With _quartodown.yml's `authors: footer: text:` instead of "Developed by".
data_developed_by <- function(path, config = list()) {
  authors <- pkg_authors(path, author_roles(config, "footer"))
  names <- vapply(authors, author_display, character(1), config = config)
  text <- config$authors$footer$text
  if (is.null(text)) {
    text <- "Developed by"
  }
  sprintf("%s %s.", trimws(text), paste(names, collapse = ", "))
}
