# From the source tree, as an installed copy may be stale. Without a
# Date/Publication (only CRAN adds it), the year comes from `Date`, so it
# doesn't change with each build; only failing that, from today.
pkg_citation_meta <- function(path) {
  desc <- desc::description$new(path)
  meta <- as.list(desc$get(desc$fields()))
  if (is.null(meta[["Date/Publication"]])) {
    date <- as.Date(trimws(c(meta[["Date"]], "")[1]), optional = TRUE)
    meta[["Date/Publication"]] <- if (is.na(date)) Sys.time() else date
  }
  if (!is.null(meta$Title)) {
    meta$Title <- squish(meta$Title)
  }
  meta
}

has_pkg_citation_file <- function(path) {
  file.exists(file.path(path, "inst", "CITATION"))
}

pkg_citation <- function(path) {
  meta <- pkg_citation_meta(path)
  if (has_pkg_citation_file(path)) {
    utils::readCitationFile(file.path(path, "inst", "CITATION"), meta = meta)
  } else {
    utils::citation(auto = meta)
  }
}

citation_source_note <- function(path, config = list()) {
  rel <- if (has_pkg_citation_file(path)) "inst/CITATION" else "DESCRIPTION"
  source_note(rel, pkg_repo(path, config)$source)
}

# A person()'s comment without the ORCID/ROR identifiers, which are shown as icons.
author_comment <- function(x) {
  comment <- x$comment
  if (is.null(comment) || is.null(names(comment))) {
    return(comment)
  }
  unname(comment[!names(comment) %in% c("ORCID", "ROR")])
}

# Icon links for a person()'s ORCID/ROR, each after a space, shipped as theme
# files since Quarto ships no Font Awesome ORCID icon.
author_id_links <- function(x) {
  comment <- x$comment
  orcid <- unname(comment[names(comment) %in% "ORCID"])
  ror <- unname(comment[names(comment) %in% "ROR"])
  paste0(
    c(
      sprintf(
        " <a href='https://orcid.org/%s'><img src='orcid.svg' class='orcid' alt='ORCID'></a>",
        orcid
      ),
      sprintf(
        " <a href='https://ror.org/%s'><img src='ror.svg' class='ror' alt='ROR'></a>",
        ror
      )
    ),
    collapse = ""
  )
}

# Links <doi:...>, <arXiv:...> and <http...> in escaped text.
linkify <- function(text) {
  text <- html_escape(text)
  text <- gsub(
    "&lt;doi:([^&]+)&gt;",
    "&lt;<a href='https://doi.org/\\1'>doi:\\1</a>&gt;",
    text,
    ignore.case = TRUE
  )
  text <- gsub(
    "&lt;arXiv:([^&]+)&gt;",
    "&lt;<a href='https://arxiv.org/abs/\\1'>arXiv:\\1</a>&gt;",
    text,
    ignore.case = TRUE
  )
  gsub("&lt;((http|ftp)[^&]+)&gt;", "&lt;<a href='\\1'>\\1</a>&gt;", text)
}

build_authors_qmd <- function(path, config = list()) {
  author_items <- vapply(
    pkg_authors(path),
    function(x) {
      comment <- linkify(author_comment(x))
      comment <- if (length(comment) > 0) {
        paste0("\n<br /><small>", comment, "</small>", collapse = "")
      } else {
        ""
      }
      sprintf(
        "<li><p><strong>%s</strong>. %s.%s%s</p></li>",
        author_display(x, config),
        author_roles_text(x),
        author_id_links(x),
        comment
      )
    },
    character(1)
  )

  cit <- pkg_citation(path)

  lines <- c(
    '---',
    'title: "Authors and Citation"',
    '---',
    "",
    "## Authors",
    "",
    '<ul class="list-unstyled">',
    author_items,
    "</ul>",
    "",
    "## Citation {#citation}",
    "",
    citation_source_note(path, config),
    "",
    format(cit, style = "html"),
    "",
    "```bibtex",
    format(cit, style = "bibtex"),
    "```"
  )

  write_utf8(lines, file.path(project_dir(path), "authors.qmd"))
  invisible()
}
