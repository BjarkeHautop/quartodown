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

test_that("the template uses the same actions as the package's own workflow", {
  # Dependabot only updates .github/workflows/, so this catches a template left
  # behind. .github/ isn't in the built package, so this runs only from source.
  own <- test_path("..", "..", ".github", "workflows", "quartodown.yaml")
  skip_if_not(file.exists(own))
  uses <- function(file) {
    steps <- yaml::yaml.load_file(file)$jobs$quartodown$steps
    unlist(lapply(steps, function(step) step$uses))
  }
  template <- system.file(
    "templates",
    "quartodown.yaml",
    package = "quartodown"
  )
  expect_identical(uses(template), uses(own))
})
