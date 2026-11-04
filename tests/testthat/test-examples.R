test_that("example_knit_options() matches Quarto's defaults for HTML", {
  opts <- example_knit_options(quarto_config(new_pkg()))
  chunk <- opts$opts_chunk
  expect_identical(chunk$fig.width, 7)
  expect_identical(chunk$fig.height, 5)
  expect_identical(chunk$dev, "png")
  expect_identical(chunk$fig.retina, 2)
  expect_identical(chunk$dpi, 96)
  expect_true(chunk$warning)
  expect_false(chunk$error)
  # quartodown's `knitr:` settings.
  expect_true(chunk$collapse)
  expect_identical(chunk$comment, "#>")
  expect_identical(opts$opts_knit$rmarkdown.pandoc.to, "html")
})

test_that("example_knit_options() takes the format's settings over the project's", {
  settings <- list(
    execute = list(`fig-width` = 5, `fig-dpi` = 72),
    format = list(html = list(`fig-width` = 9, `fig-format` = "svg"))
  )
  chunk <- example_knit_options(settings)$opts_chunk
  expect_identical(chunk$fig.width, 9)
  expect_identical(chunk$dpi, 72)
  expect_identical(chunk$dev, "svg")
  expect_null(chunk$fig.retina)
})

test_that("example_knit_options() leaves pages to Quarto for settings it doesn't handle", {
  expect_null(example_knit_options(list(execute = list(eval = FALSE))))
  expect_null(example_knit_options(list(
    format = list(html = list(echo = FALSE))
  )))
  expect_null(example_knit_options(list(execute = list(`fig-format` = "pdf"))))
  expect_null(example_knit_options(list(
    knitr = list(opts_chunk = list(cache = TRUE))
  )))
  expect_null(example_knit_options(list(knitr = list(opts_knit = list()))))
})

write_example_page <- function(dir, name, ...) {
  file <- file.path(dir, paste0(name, ".qmd"))
  writeLines(
    c("---", paste("title:", name), "---", "", "## Examples", "", ...),
    file
  )
  normalizePath(file, winslash = "/")
}

chunk <- function(...) {
  c("```{r, collapse=TRUE, comment=\"#>\"}", ..., "```", "")
}

example_opts <- function() {
  example_knit_options(list(
    knitr = list(opts_chunk = list(collapse = TRUE, comment = "#>"))
  ))
}

test_that("run_examples_script() writes the Markdown Quarto's knitr engine would", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  page <- write_example_page(
    dir,
    "a",
    "```{r, include=FALSE}",
    "hidden <- 1",
    "```",
    "",
    chunk("1 + 1", "warning(\"careful\")", "plot(1)"),
    "```r",
    "not_run()",
    "```"
  )
  status <- run_examples_script(page, example_opts(), character())
  expect_identical(status, c(a.qmd = "done"))

  lines <- readLines(page)
  expect_false(any(startsWith(lines, "```{r")))
  expect_false(any(grepl("hidden", lines)))
  expect_identical(sum(lines == "::: {.cell}"), 1L)
  code <- which(lines == "```{.r .cell-code}")
  expect_identical(lines[code + 1:2], c("1 + 1", "#> [1] 2"))
  expect_true(any(startsWith(lines, "#> Warning")))
  figure <- which(lines == "::: {.cell-output-display}")
  expect_identical(
    lines[figure + 1],
    "![](a_files/figure-html/unnamed-chunk-2-1.png){width=672}"
  )
  expect_true(file.exists(file.path(
    dir,
    "a_files/figure-html/unnamed-chunk-2-1.png"
  )))
  # Not a chunk, so left as it is.
  expect_identical(lines[which(lines == "```r") + 1], "not_run()")
})

test_that("run_examples_script() gives output its own block with `#| collapse: false`", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  page <- write_example_page(dir, "a", chunk("#| collapse: false", "1 + 1"))
  run_examples_script(page, example_opts(), character())
  lines <- readLines(page)
  output <- which(lines == "::: {.cell-output .cell-output-stdout}")
  expect_length(output, 1)
  # As Quarto writes it, blank line included.
  expect_identical(lines[output + 1:4], c("", "```", "#> [1] 2", "```"))
})

test_that("run_examples_script() resets what a page changes before the next", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  first <- write_example_page(
    dir,
    "first",
    chunk(
      "x <- 1",
      "options(digits = 3)",
      "library(tools)",
      "setwd(tempdir())"
    )
  )
  second <- write_example_page(
    dir,
    "second",
    chunk(
      "exists(\"x\")",
      "getOption(\"digits\")",
      "\"package:tools\" %in% search()",
      "basename(getwd())"
    )
  )
  status <- run_examples_script(c(first, second), example_opts(), character())
  expect_identical(unname(status), c("done", "done"))
  lines <- readLines(second)
  expect_identical(
    lines[grep("^#>", lines)],
    c(
      "#> [1] FALSE",
      "#> [1] 7",
      "#> [1] FALSE",
      sprintf("#> [1] \"%s\"", basename(dir))
    )
  )
})

test_that("run_examples_script() leaves a page that fails or has HTML dependencies to Quarto", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  failing <- write_example_page(dir, "failing", chunk("stop(\"boom\")"))
  widget <- write_example_page(
    dir,
    "widget",
    chunk(
      "knitr::knit_meta_add(list(structure(list(), class = \"html_dependency\")))"
    )
  )
  before <- list(readLines(failing), readLines(widget))
  status <- run_examples_script(c(failing, widget), example_opts(), character())
  expect_match(status[["failing.qmd"]], "^error: .*boom")
  expect_identical(status[["widget.qmd"]], "HTML dependencies")
  expect_identical(list(readLines(failing), readLines(widget)), before)
})

test_that("run_examples_script() splits the pages between `workers` processes", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  pages <- vapply(
    1:3,
    function(i) {
      write_example_page(dir, paste0("p", i), chunk(paste0(i, " + 1")))
    },
    character(1)
  )
  status <- run_examples_script(pages, example_opts(), character(), workers = 2)
  expect_setequal(names(status), basename(pages))
  expect_true(all(status == "done"))
  expect_true(any(grepl("#> [1] 4", readLines(pages[[3]]), fixed = TRUE)))
})

test_that("run_examples() reuses a page's results with `freeze = TRUE` until it changes", {
  skip_on_cran()
  dir <- new_pkg()
  reference <- file.path(dir, "_quarto", "reference")
  dir.create(reference)
  counter <- normalizePath(
    withr::local_tempfile(),
    winslash = "/",
    mustWork = FALSE
  )
  code <- c(
    sprintf("cat(1, file = %s, append = TRUE)", deparse(counter)),
    "plot(1)"
  )
  settings <- quarto_config(dir)

  build <- function(code) {
    reset_project(dir, freeze = TRUE)
    dir.create(reference)
    write_example_page(reference, "a", chunk(code))
    suppressMessages(run_examples(dir, character(), settings, freeze = TRUE))
    readLines(file.path(reference, "a.qmd"))
  }
  first <- build(code)
  expect_identical(build(code), first)
  expect_identical(nchar(readLines(counter, warn = FALSE)), 1L)
  expect_true(file.exists(file.path(
    reference,
    "a_files/figure-html/unnamed-chunk-1-1.png"
  )))

  build(c(code, "2"))
  expect_identical(nchar(readLines(counter, warn = FALSE)), 2L)
})
