test_that("map_workers() is lapply() without daemons", {
  expect_identical(
    map_workers(1:3, function(x, y) x + y, y = 1L),
    list(2L, 3L, 4L)
  )
})

test_that("map_workers() passes on the daemons' warnings, messages and errors", {
  skip_on_cran()
  skip_if_not_installed("mirai", "2.4.1")
  local_workers(2)

  expect_identical(map_workers(1:3, sqrt), as.list(sqrt(1:3)))

  signalled <- character()
  keep <- function(cond) {
    signalled <<- c(signalled, conditionMessage(cond))
    tryInvokeRestart("muffleWarning")
    tryInvokeRestart("muffleMessage")
  }
  value <- withCallingHandlers(
    map_workers(c("a", "b"), warning),
    warning = keep
  )
  expect_identical(value, list("a", "b"))
  withCallingHandlers(map_workers(c("c", "d"), message), message = keep)
  expect_identical(signalled, c("a", "b", "c\n", "d\n"))

  expect_error(map_workers(c("boom", "boom"), stop), "boom")
})

test_that("build_site() checks `workers`", {
  for (workers in list(0, 1.5, NA, "2", c(1, 2))) {
    expect_error(
      build_site(withr::local_tempdir(), workers = workers),
      "workers"
    )
  }
})

test_that("shard_pages() spreads the pages with code over the shards", {
  dir <- withr::local_tempdir()
  for (f in c("a.qmd", "b.qmd", "c.qmd", "d.qmd")) {
    writeLines("text", file.path(dir, f))
  }
  writeLines(c("```{r}", "1", "```"), file.path(dir, "c.qmd"))
  writeLines(c("```{r}", "1", "```"), file.path(dir, "d.qmd"))
  pages <- c("a.qmd", "b.qmd", "c.qmd", "d.qmd")

  expect_identical(
    shard_pages(dir, pages, 2),
    list(c("c.qmd", "a.qmd"), c("d.qmd", "b.qmd"))
  )
  expect_identical(shard_pages(dir, pages[1:2], 4), list("a.qmd", "b.qmd"))
  expect_null(shard_pages(dir, pages, 1))
  expect_null(shard_pages(dir, "a.qmd", 2))
  # A `render:` list with globs is left to one process.
  expect_null(shard_pages(dir, c("a.qmd", "*.md"), 2))
})

test_that("link_shard_pages() links other shards' sources to their pages", {
  others <- c("NEWS.md", "vignettes/a.qmd", "index.qmd")
  html <- paste(
    '<a href="../NEWS.md">',
    '<a href="a.qmd#usage">',
    '<a href="/index.qmd">',
    '<a href="b.qmd">',
    '<a href="https://example.com/NEWS.md">',
    '<a href="data.md">'
  )
  expect_identical(
    link_shard_pages(html, "vignettes/b.qmd", others),
    paste(
      '<a href="../NEWS.html">',
      '<a href="a.html#usage">',
      '<a href="/index.html">',
      '<a href="b.qmd">',
      '<a href="https://example.com/NEWS.md">',
      '<a href="data.md">'
    )
  )
})

test_that("merge_shard_sites() joins the shards' sites", {
  path <- withr::local_tempdir()
  scratch <- withr::local_tempdir()
  shards <- list(c("index.qmd", "a.qmd"), "b.md")
  sites <- file.path(scratch, c("site-1", "site-2"))
  projects <- structure(file.path(scratch, c("p-1", "p-2")), site = sites)
  write <- function(lines, file) {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    writeLines(lines, file)
  }
  sitemap <- function(page) {
    c(
      '<?xml version="1.0" encoding="UTF-8"?>',
      '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
      sprintf("<url><loc>https://x.org/%s</loc></url>", page),
      "</urlset>"
    )
  }
  write("home, see <a href=\"b.md\">b</a>", file.path(sites[1], "index.html"))
  write("a", file.path(sites[1], "a.html"))
  write("b.md copied as a resource", file.path(sites[1], "b.md"))
  write("lib", file.path(sites[1], "site_libs", "x.js"))
  write(
    '[\n  {"href": "index.html"},\n  {"href": "a.html"}\n]',
    file.path(sites[1], "search.json")
  )
  write(sitemap("index.html"), file.path(sites[1], "sitemap.xml"))
  write("redirect to b.html", file.path(sites[2], "index.html"))
  write("b", file.path(sites[2], "b.html"))
  write("lib", file.path(sites[2], "site_libs", "x.js"))
  write('[\n  {"href": "b.html"}\n]', file.path(sites[2], "search.json"))
  write(sitemap("b.html"), file.path(sites[2], "sitemap.xml"))
  write(
    "{}",
    file.path(projects[2], "_freeze", "b", "execute-results", "html.json")
  )

  dest <- file.path(path, "docs")
  merge_shard_sites(path, projects, shards, dest)

  expect_setequal(
    list.files(dest, recursive = TRUE),
    c(
      "index.html",
      "a.html",
      "b.html",
      "site_libs/x.js",
      "search.json",
      "sitemap.xml"
    )
  )
  expect_identical(
    readLines(file.path(dest, "index.html")),
    "home, see <a href=\"b.html\">b</a>"
  )
  expect_identical(
    readLines(file.path(dest, "search.json")),
    c(
      "[",
      '{"href": "index.html"},',
      '  {"href": "a.html"},',
      '{"href": "b.html"}',
      "]"
    )
  )
  locs <- xml2::xml_text(xml2::xml_find_all(
    xml2::read_xml(file.path(dest, "sitemap.xml")),
    "//*[local-name() = 'loc']"
  ))
  expect_identical(locs, c("https://x.org/index.html", "https://x.org/b.html"))
  expect_true(file.exists(
    file.path(path, "_quarto", "_freeze", "b", "execute-results", "html.json")
  ))
})

test_that("one_process_reason() finds pages that read other pages", {
  dir <- withr::local_tempdir()
  writeLines(c("---", "title: A", "---"), file.path(dir, "a.qmd"))
  writeLines(
    c("---", "title: B", "---", "listing: in the text"),
    file.path(dir, "b.qmd")
  )
  pages <- c("a.qmd", "b.qmd")
  linked <- list(text = "A", href = "a.qmd")
  settings <- list(
    website = list(
      navbar = list(
        left = list(linked, list(text = "Site", href = "https://x.org"))
      ),
      sidebar = list(
        contents = list(list(
          section = "S",
          href = "b.qmd",
          contents = list(linked)
        ))
      )
    )
  )
  expect_null(one_process_reason(dir, pages, settings))

  untitled <- function(x) {
    s <- settings
    s$website$navbar$left <- x
    one_process_reason(dir, pages, s)
  }
  expect_match(untitled(list(list(href = "a.qmd"))), "no")
  expect_match(
    untitled(list(list(text = "M", menu = list(list(file = "./b.qmd"))))),
    "no"
  )
  expect_match(untitled(list("a.qmd")), "no")
  expect_match(untitled(c("a.qmd", "b.qmd")), "no")
  expect_null(untitled(list(list(
    icon = "github",
    href = "https://github.com"
  ))))
  settings$website$sidebar <- list(contents = "auto")
  expect_match(one_process_reason(dir, pages, settings), "no")

  writeLines(
    c("---", "listing:", "  contents: '*.qmd'", "---"),
    file.path(dir, "b.qmd")
  )
  expect_match(one_process_reason(dir, pages, list()), "b.qmd")
  writeLines(c("---", "title: B", "---"), file.path(dir, "b.qmd"))
  dir.create(file.path(dir, "posts"))
  writeLines("listing: default", file.path(dir, "posts", "_metadata.yml"))
  expect_match(one_process_reason(dir, pages, list()), "_metadata.yml")
})
