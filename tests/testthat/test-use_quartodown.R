test_that("use_quartodown() errors outside a package root", {
  dir <- withr::local_tempdir()
  expect_error(use_quartodown(dir), "DESCRIPTION")
})

test_that("use_quartodown() writes a starter quartodown.yml listing every topic", {
  dir <- withr::local_tempdir()
  writeLines("Package: examplepkg", file.path(dir, "DESCRIPTION"))
  dir.create(file.path(dir, "man"))
  writeLines(
    c("\\name{foo}", "\\alias{foo}", "\\title{Foo}", "\\usage{foo(x)}"),
    file.path(dir, "man", "foo.Rd")
  )
  writeLines(
    c("\\name{bar}", "\\alias{bar}", "\\title{Bar}", "\\keyword{internal}"),
    file.path(dir, "man", "bar.Rd")
  )

  expect_message(use_quartodown(dir), "Wrote")

  config <- yaml::yaml.load_file(file.path(
    dir,
    "_quartodown",
    "_quartodown.yml"
  ))
  expect_identical(config$reference[[1]]$contents, "foo")
  expect_identical(config$development$mode, "auto")
})

test_that("use_quartodown() adds the .gitignore/.Rbuildignore entries the build needs", {
  dir <- withr::local_tempdir()
  writeLines("Package: examplepkg", file.path(dir, "DESCRIPTION"))

  use_quartodown(dir)

  gitignore <- readLines(file.path(dir, ".gitignore"))
  expect_true(all(c("_quarto/*", "!_quarto/_freeze/") %in% gitignore))
  expect_false("_quartodown.yml" %in% gitignore)

  rbuildignore <- readLines(file.path(dir, ".Rbuildignore"))
  expect_true(all(
    c("^docs$", "^_quarto$", "^_quartodown$") %in% rbuildignore
  ))
})

test_that("use_quartodown() doesn't duplicate .gitignore/.Rbuildignore entries on repeat calls", {
  dir <- withr::local_tempdir()
  writeLines("Package: examplepkg", file.path(dir, "DESCRIPTION"))
  writeLines("some/existing/entry", file.path(dir, ".gitignore"))

  use_quartodown(dir)
  use_quartodown(dir)

  gitignore <- readLines(file.path(dir, ".gitignore"))
  expect_identical(sum(gitignore == "_quarto/*"), 1L)
  expect_identical(sum(gitignore == "some/existing/entry"), 1L)
})

test_that("use_quartodown() never overwrites an existing quartodown.yml", {
  dir <- withr::local_tempdir()
  writeLines("Package: examplepkg", file.path(dir, "DESCRIPTION"))
  use_quartodown(dir)

  config <- yaml::yaml.load_file(file.path(
    dir,
    "_quartodown",
    "_quartodown.yml"
  ))
  expect_identical(config$reference[[1]]$contents, list())

  dir.create(file.path(dir, "man"))
  writeLines(
    c("\\name{bar}", "\\alias{bar}", "\\title{Bar}", "\\usage{bar(x)}"),
    file.path(dir, "man", "bar.Rd")
  )

  expect_message(use_quartodown(dir), "Skipping.*quartodown\\.yml")
  config <- yaml::yaml.load_file(file.path(
    dir,
    "_quartodown",
    "_quartodown.yml"
  ))
  expect_identical(config$reference[[1]]$contents, list())
})
