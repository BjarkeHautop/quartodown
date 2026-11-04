# End-to-end builds with Quarto: every other test calls the generation
# functions directly.

skip_if_no_quarto <- function() {
  skip_on_cran()
  skip_if(Sys.which("quarto") == "", "Quarto is not installed")
}

new_installable_pkg <- function(.local_envir = parent.frame()) {
  dir <- new_pkg(.local_envir = .local_envir)
  writeLines(
    c(
      "Package: examplepkg",
      "Version: 1.0.0",
      paste0(
        "Authors@R: person(\"Jane\", \"Doe\", ",
        "email = \"jane@example.com\", role = c(\"aut\", \"cre\"))"
      ),
      "URL: https://example.com/examplepkg, https://github.com/jane/examplepkg",
      "License: MIT + file LICENSE",
      "Title: An Example Package",
      "Description: Says hello.",
      "Encoding: UTF-8"
    ),
    file.path(dir, "DESCRIPTION")
  )
  writeLines(
    "YEAR: 2026\nCOPYRIGHT HOLDER: Jane Doe",
    file.path(dir, "LICENSE")
  )
  writeLines("export(hello)", file.path(dir, "NAMESPACE"))
  dir.create(file.path(dir, "R"))
  writeLines(
    "hello <- function(name = \"world\") paste0(\"Hello, \", name, \"!\")",
    file.path(dir, "R", "hello.R")
  )
  dir.create(file.path(dir, "man"))
  writeLines(
    c(
      "\\name{hello}",
      "\\alias{hello}",
      "\\title{Say hello}",
      "\\usage{hello(name = \"world\")}",
      "\\arguments{\\item{name}{Who to greet.}}",
      "\\value{A greeting.}",
      "\\description{Greets someone.}",
      "\\examples{hello(\"you\")}"
    ),
    file.path(dir, "man", "hello.Rd")
  )
  writeLines(
    c("# examplepkg", "", "Call `hello()` to say hello."),
    file.path(dir, "README.md")
  )
  writeLines(
    c("# examplepkg 1.0.0", "", "* First release."),
    file.path(dir, "NEWS.md")
  )
  write_vignette(dir, "examplepkg.qmd", "Get started")
  dir
}

expect_site <- function(dir) {
  docs <- file.path(dir, "docs")
  for (page in c(
    "index.html",
    "reference/index.html",
    "reference/hello.html",
    "vignettes/examplepkg.html",
    "NEWS.html",
    "LICENSE-text.html",
    "authors.html",
    "pkgdown.yml",
    ".nojekyll"
  )) {
    expect_true(file.exists(file.path(docs, page)), label = page)
  }

  hello <- read_utf8(file.path(docs, "reference", "hello.html"))
  # The example ran, and its call links to the topic.
  expect_match(hello, "Hello, you!", fixed = TRUE, all = FALSE)
  expect_match(hello, "quartodown-tip-payload", fixed = TRUE, all = FALSE)

  index <- read_utf8(file.path(docs, "index.html"))
  expect_match(index, "reference/hello.html", fixed = TRUE, all = FALSE)
}

test_that("build_site() renders a package's site", {
  skip_if_no_quarto()
  dir <- new_installable_pkg()

  expect_no_error(suppressMessages(build_site(dir, preview = FALSE)))
  expect_site(dir)
})

test_that("build_site() renders a site with several workers", {
  skip_if_no_quarto()
  skip_if_not_installed("mirai", "2.4.1")
  dir <- new_installable_pkg()

  expect_no_error(suppressMessages(
    build_site(dir, workers = 2, preview = FALSE)
  ))
  expect_site(dir)
})
