# A built site: a `.llms.md` page for each of `pages` in docs/, with `lines`.
local_llms_pages <- function(dir, pages, lines = "# Page") {
  for (page in pages) {
    file <- file.path(dir, "docs", page)
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    writeLines(lines, file)
  }
}

test_that("llms.txt has the home page, then links to articles, reference and the rest", {
  dir <- new_pkg_with_man()
  writeLines(
    c(
      "Package: examplepkg",
      "Title: Does Things",
      "Description: Does many",
      "    things."
    ),
    file.path(dir, "DESCRIPTION")
  )
  write_rd(dir, "foo", title = "Foo things")
  write_rd(dir, "bar", title = "Bar things")
  write_rd(dir, "baz", title = "Baz things")
  write_rd(dir, "hidden", keyword = "internal")
  dir.create(file.path(dir, "vignettes", "articles"), recursive = TRUE)
  write_vignette(dir, "examplepkg.qmd", "Get going")
  write_vignette(dir, "articles/advanced.qmd", "Advanced")
  local_llms_pages(
    dir,
    c(
      "404.llms.md",
      "reference/index.llms.md",
      "reference/foo.llms.md",
      "reference/bar.llms.md",
      "reference/baz.llms.md",
      "vignettes/examplepkg.llms.md",
      "vignettes/articles/advanced.llms.md"
    )
  )
  local_llms_pages(
    dir,
    "index.llms.md",
    c(
      "# examplepkg ![logo](logo.png)",
      "",
      "Intro.",
      "",
      "## Use",
      "",
      "```r",
      "# a comment",
      "```",
      ""
    )
  )
  local_llms_pages(dir, "NEWS.llms.md", "# Changelog ![logo](logo.png)")
  config <- reference_config(c(
    "reference:",
    "  - title: Main",
    "    desc: The main ones.",
    "    contents: [foo, bar]",
    "  - subtitle: More",
    "    contents: baz",
    "  - title: internal",
    "    contents: hidden"
  ))
  settings <- list(
    website = list(
      `site-url` = "https://example.com/",
      description = "Does Things"
    )
  )

  write_llms_txt(dir, config, settings, docs = file.path(dir, "docs"))
  expect_identical(
    readLines(file.path(dir, "docs", "llms.txt")),
    c(
      "# examplepkg",
      "",
      "> Does Things",
      "",
      "## Home",
      "",
      "Intro.",
      "",
      "### Use",
      "",
      "```r",
      "# a comment",
      "```",
      "",
      "## Articles",
      "",
      "- [Get going](https://example.com/vignettes/examplepkg.llms.md)",
      "- [Advanced](https://example.com/vignettes/articles/advanced.llms.md)",
      "",
      "## Reference",
      "",
      "### Main",
      "",
      "The main ones.",
      "",
      "- [`foo()`](https://example.com/reference/foo.llms.md): Foo things",
      "- [`bar()`](https://example.com/reference/bar.llms.md): Bar things",
      "",
      "#### More",
      "",
      "- [`baz()`](https://example.com/reference/baz.llms.md): Baz things",
      "",
      "## Optional",
      "",
      "- [Changelog](https://example.com/NEWS.llms.md)"
    )
  )
})

test_that("llms.txt has DESCRIPTION's Description without a home page", {
  dir <- new_pkg()
  writeLines(
    c("Package: examplepkg", "Description: Does things."),
    file.path(dir, "DESCRIPTION")
  )
  local_llms_pages(dir, "NEWS.llms.md", "# Changelog")
  write_llms_txt(dir, docs = file.path(dir, "docs"))
  expect_identical(
    readLines(file.path(dir, "docs", "llms.txt")),
    c(
      "# examplepkg",
      "",
      "Does things.",
      "",
      "## Optional",
      "",
      "- [Changelog](NEWS.llms.md)"
    )
  )
})

test_that("no llms.txt without .llms.md pages", {
  dir <- new_pkg()
  dir.create(file.path(dir, "docs"))
  write_llms_txt(dir, docs = file.path(dir, "docs"))
  expect_false(file.exists(file.path(dir, "docs", "llms.txt")))
})

test_that("the .llms.md pages lose ANSI codes and a theme's second image", {
  dir <- new_pkg()
  local_llms_pages(
    dir,
    "index.llms.md",
    c(
      "# pkg ![logo](logo-light.png)![logo](logo.png)",
      "![a](a.png)![b](b.png)",
      "#> \u001b[32mok\u001b[39m"
    )
  )
  write_llms_txt(dir, docs = file.path(dir, "docs"))
  expect_identical(
    readLines(file.path(dir, "docs", "index.llms.md"), encoding = "UTF-8"),
    c("# pkg ![logo](logo-light.png)", "![a](a.png)![b](b.png)", "#> ok")
  )
})

test_that("llms-txt is on where Quarto has it", {
  dir <- new_pkg()
  local_mocked_bindings(quarto_has_llms_txt = function() TRUE)
  expect_true(quarto_config(dir)$website$`llms-txt`)
  config <- list(quarto = list(website = list(`llms-txt` = FALSE)))
  expect_false(quarto_config(dir, config)$website$`llms-txt`)

  local_mocked_bindings(quarto_has_llms_txt = function() FALSE)
  expect_null(quarto_config(dir)$website$`llms-txt`)
})
