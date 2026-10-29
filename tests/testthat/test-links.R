# ---- topics -------------------------------------------------------------------

test_that("the package's own topics link to its reference pages", {
  local_sites()
  ctx <- link_context(
    pkg = "mypkg",
    topics = c(foo = "foo", "print.foo" = "foo"),
    topic_path = "../reference/"
  )
  expect_equal(href_topic(ctx, "print.foo"), "../reference/foo.html")
  expect_equal(href_topic(ctx, "foo", "mypkg"), "../reference/foo.html")
  # Named, the package's own topics aren't looked up anywhere else.
  expect_equal(href_topic(ctx, "median", "mypkg"), NA_character_)
})

test_that("base R's topics link to rdrr.io", {
  local_sites()
  ctx <- link_context()
  expect_equal(
    href_topic(ctx, "median", is_fun = TRUE),
    "https://rdrr.io/r/stats/median.html"
  )
  expect_equal(
    href_topic(ctx, "unit", "grid"),
    "https://rdrr.io/r/grid/unit.html"
  )
  # A dataset is a topic, but not a function.
  expect_equal(
    href_topic(ctx, "mtcars"),
    "https://rdrr.io/r/datasets/mtcars.html"
  )
  expect_equal(href_topic(ctx, "mtcars", is_fun = TRUE), NA_character_)
  expect_equal(href_topic(ctx, "not_a_topic"), NA_character_)
})

test_that("other packages' topics link to their pkgdown site, or else rdrr.io", {
  fetched <- local_sites(cli_site)
  ctx <- link_context()
  expect_equal(
    href_topic(ctx, "cli_alert", "cli"),
    "https://cli.r-lib.org/reference/cli_alert.html"
  )
  # fs has no pkgdown.yml here.
  expect_equal(
    href_topic(ctx, "path", "fs"),
    "https://rdrr.io/pkg/fs/man/path.html"
  )
  # Unnamed, only the attached packages' topics.
  expect_equal(href_topic(ctx, "cli_alert", is_fun = TRUE), NA_character_)
  attach_packages(ctx, "cli")
  expect_equal(
    href_topic(ctx, "cli_alert", is_fun = TRUE),
    "https://cli.r-lib.org/reference/cli_alert.html"
  )
  # Each site is fetched once.
  expect_equal(
    href_topic(ctx, "cli_text", "cli"),
    "https://cli.r-lib.org/reference/cli_text.html"
  )
  expect_equal(sum(fetched$urls == "https://cli.r-lib.org"), 1)
})

test_that("a package that isn't installed isn't linked or looked up", {
  fetched <- local_sites()
  ctx <- link_context()
  attach_packages(ctx, "notapkg")
  expect_equal(href_topic(ctx, "f", "notapkg"), NA_character_)
  expect_equal(href_topic(ctx, "f", is_fun = TRUE), NA_character_)
  expect_equal(href_package("notapkg"), NA_character_)
  expect_equal(href_article(ctx, "intro", "notapkg"), NA_character_)
  expect_length(fetched$urls, 0)
})

test_that("a re-exported function links to the package it comes from", {
  skip_if_not_installed("dplyr")
  skip_if_not_installed("magrittr")
  local_sites()
  expect_equal(
    href_topic(link_context(), "%>%", "dplyr"),
    href_topic(link_context(), "%>%", "magrittr")
  )
})

test_that("attaching a package attaches its Depends", {
  skip_if_not_installed("MASS")
  ctx <- link_context()
  attach_packages(ctx, "MASS")
  expect_equal(ctx$attached[[1]], "MASS")
  expect_true(all(c("stats", "graphics") %in% ctx$attached))
})

# ---- articles -----------------------------------------------------------------

test_that("vignettes link to the package's own articles, or another's site", {
  local_sites(cli_site)
  ctx <- link_context(
    pkg = "mypkg",
    articles = c(intro = "vignettes/intro.html"),
    root = "../"
  )
  expect_equal(href_article(ctx, "intro"), "../vignettes/intro.html")
  expect_equal(href_article(ctx, "other", "mypkg"), NA_character_)
  expect_equal(
    href_article(ctx, "progress", "cli"),
    "https://cli.r-lib.org/articles/progress.html"
  )
  # Unnamed, the attached packages' vignettes.
  expect_equal(href_article(ctx, "progress"), NA_character_)
  attach_packages(ctx, "cli")
  expect_equal(
    href_article(ctx, "progress"),
    "https://cli.r-lib.org/articles/progress.html"
  )
})

test_that("a vignette without a site links to CRAN", {
  local_sites()
  expect_equal(
    href_article(link_context(), "grid", "grid"),
    "https://cran.rstudio.com/web/packages/grid/vignettes/grid.pdf"
  )
})

# ---- packages and sites -------------------------------------------------------

test_that("a package links to its first URL", {
  local_sites()
  expect_equal(href_package("cli"), "https://cli.r-lib.org")
  expect_equal(href_package("stats"), NA_character_)
})

test_that("a site's pkgdown.yml is cached across sessions, but a failed download isn't", {
  local_sites()
  calls <- 0
  result <- list(
    yml = list(urls = list(reference = "https://x.org/ref")),
    failed = FALSE
  )
  local_mocked_bindings(fetch_pkgdown_yml = function(url) {
    calls <<- calls + 1
    result
  })
  expect_equal(pkgdown_yml("https://x.org")$urls$reference, "https://x.org/ref")
  expect_equal(pkgdown_yml("https://x.org")$urls$reference, "https://x.org/ref")
  expect_equal(calls, 1)

  result <- list(yml = NULL, failed = TRUE)
  expect_null(pkgdown_yml("https://y.org"))
  expect_null(pkgdown_yml("https://y.org"))
  expect_equal(calls, 3)
})

test_that("a site without urls in its pkgdown.yml has them under its root", {
  site <- site_from_yml(list(articles = list(a = "a.html")), "https://x.org")
  expect_equal(site$reference, "https://x.org/reference")
  expect_equal(site$article, "https://x.org/articles")
  expect_equal(site$articles, c(a = "a.html"))
})

test_that("a failed download falls back to a week-old copy", {
  local_sites()
  result <- list(
    yml = list(urls = list(reference = "https://x.org/ref")),
    failed = FALSE
  )
  local_mocked_bindings(fetch_pkgdown_yml = function(url) result)
  pkgdown_yml("https://x.org")
  # A week later, without a network.
  file <- list.files(
    file.path(link_cache_dir(), "pkgdown-yml"),
    full.names = TRUE
  )
  entry <- readRDS(file)
  entry$time <- entry$time - 8 * 24 * 3600
  saveRDS(entry, file)
  result <- list(yml = NULL, failed = TRUE)
  expect_equal(pkgdown_yml("https://x.org")$urls$reference, "https://x.org/ref")
})

test_that("after three failed downloads in a row, nothing more is downloaded", {
  local_sites()
  calls <- 0
  local_mocked_bindings(fetch_pkgdown_yml = function(url) {
    calls <<- calls + 1
    list(yml = NULL, failed = TRUE)
  })
  expect_null(pkgdown_yml("https://a.org"))
  expect_null(pkgdown_yml("https://b.org"))
  expect_warning(pkgdown_yml("https://c.org"), "rdrr.io")
  expect_null(pkgdown_yml("https://d.org"))
  expect_equal(calls, 3)

  # Nor in another R session, such as another worker, for a while.
  rm(list = ls(link_memo), envir = link_memo)
  expect_true(network_down())
  expect_null(pkgdown_yml("https://e.org"))
  expect_equal(calls, 3)
})

test_that("a site that's down doesn't stop the others' downloads", {
  local_sites()
  calls <- 0
  local_mocked_bindings(fetch_pkgdown_yml = function(url) {
    calls <<- calls + 1
    list(
      yml = if (url != "https://down.org") list(),
      failed = url == "https://down.org"
    )
  })
  for (url in c(
    "https://down.org",
    "https://a.org",
    "https://down.org",
    "https://b.org",
    "https://down.org"
  )) {
    pkgdown_yml(url)
  }
  expect_false(network_down())
  expect_equal(calls, 5)
})

test_that("sites downloaded a month ago and an expired offline marker are pruned", {
  local_sites()
  dir <- file.path(link_cache_dir(), "pkgdown-yml")
  dir.create(dir, recursive = TRUE)
  old <- file.path(dir, "old.rds")
  recent <- file.path(dir, "recent.rds")
  marker <- file.path(link_cache_dir(), "offline")
  file.create(c(old, recent, marker))
  Sys.setFileTime(old, Sys.time() - 31 * 24 * 3600)
  Sys.setFileTime(marker, Sys.time() - 60)

  prune_link_cache()
  expect_false(file.exists(old))
  expect_true(file.exists(recent))
  expect_true(file.exists(marker))

  Sys.setFileTime(marker, Sys.time() - 11 * 60)
  prune_link_cache()
  expect_false(file.exists(marker))
})
