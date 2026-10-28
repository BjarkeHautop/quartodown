new_pkg <- function(articles = character(), .local_envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .local_envir)
  # Keep tests offline: don't look the package up on CRAN.
  local_mocked_bindings(
    on_cran = function(pkg) NA,
    .env = .local_envir
  )
  writeLines(
    c(
      "Package: examplepkg",
      "Version: 1.0.0",
      "Authors@R: person(\"Jane\", \"Doe\", role = c(\"aut\", \"cre\"))"
    ),
    file.path(dir, "DESCRIPTION")
  )
  dir.create(file.path(dir, "vignettes"))
  dir.create(file.path(dir, "_quarto"))
  if (length(articles) > 0) {
    dir.create(file.path(dir, "vignettes", "articles"))
    for (f in articles) {
      writeLines(
        c("---", sprintf("title: \"%s\"", f), "---", "content"),
        file.path(dir, "vignettes", "articles", paste0(f, ".qmd"))
      )
    }
  }
  dir
}

write_vignette <- function(dir, name, title = name) {
  writeLines(
    c("---", sprintf("title: \"%s\"", title), "---", "content"),
    file.path(dir, "vignettes", name)
  )
}

nav_texts <- function(items) {
  vapply(items, function(x) x$text, character(1))
}

nav_files <- function(left) {
  unlist(lapply(left, function(item) {
    c(item$file, unlist(lapply(item$menu, function(m) m$file)))
  }))
}

write_rd <- function(
  dir,
  name,
  alias = name,
  title = name,
  concept = NULL,
  keyword = NULL,
  usage = paste0(name, "()")
) {
  lines <- c(
    sprintf("\\name{%s}", name),
    sprintf("\\alias{%s}", alias),
    sprintf("\\title{%s}", title),
    if (!is.null(usage)) c("\\usage{", usage, "}"),
    if (!is.null(concept)) sprintf("\\concept{%s}", concept),
    if (!is.null(keyword)) sprintf("\\keyword{%s}", keyword),
    "\\description{d}"
  )
  writeLines(lines, file.path(dir, "man", paste0(name, ".Rd")))
}

new_pkg_with_man <- function(.local_envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .local_envir)
  writeLines(
    c(
      "Package: examplepkg",
      "Authors@R: person(\"Jane\", \"Doe\", role = c(\"aut\", \"cre\"))"
    ),
    file.path(dir, "DESCRIPTION")
  )
  dir.create(file.path(dir, "man"))
  dir.create(file.path(dir, "_quarto"))
  dir
}

reference_config <- function(text) {
  yaml::yaml.load(paste(text, collapse = "\n"))
}

# The generated reference index, with html_text()'s entities decoded.
read_reference_index <- function(dir) {
  lines <- readLines(file.path(dir, "_quarto", "reference", "index.qmd"))
  gsub_fn(lines, "&#([0-9]+);", function(m, code) intToUtf8(as.integer(code)))
}
