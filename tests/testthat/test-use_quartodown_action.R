test_that("use_quartodown_action() errors outside a package root", {
  dir <- withr::local_tempdir()
  expect_error(use_quartodown_action(dir), "DESCRIPTION")
})

test_that("use_quartodown_action() writes the workflow and ignores .github", {
  dir <- withr::local_tempdir()
  writeLines("Package: examplepkg", file.path(dir, "DESCRIPTION"))

  expect_message(use_quartodown_action(dir), "Wrote")

  workflow <- yaml::yaml.load_file(
    file.path(dir, ".github", "workflows", "quartodown.yaml")
  )
  expect_identical(workflow$name, "quartodown")
  steps <- workflow$jobs$quartodown$steps
  run <- unlist(lapply(steps, function(step) step$run))
  expect_true("quartodown::build_site(workers = 4)" %in% run)

  rbuildignore <- readLines(file.path(dir, ".Rbuildignore"))
  expect_identical(sum(rbuildignore == "^\\.github$"), 1L)
})

test_that("use_quartodown_action() only overwrites with overwrite = TRUE", {
  dir <- withr::local_tempdir()
  writeLines("Package: examplepkg", file.path(dir, "DESCRIPTION"))
  dest <- file.path(dir, ".github", "workflows", "quartodown.yaml")
  dir.create(dirname(dest), recursive = TRUE)
  writeLines("custom", dest)

  expect_message(use_quartodown_action(dir), "Skipping")
  expect_identical(readLines(dest), "custom")

  use_quartodown_action(dir, overwrite = TRUE)
  expect_false(identical(readLines(dest), "custom"))
})
