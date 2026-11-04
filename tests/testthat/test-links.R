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

test_that("a package that isn't installed has no topics or vignettes linked", {
  fetched <- local_sites()
  ctx <- link_context()
  attach_packages(ctx, "notapkg")
  expect_equal(href_topic(ctx, "f", "notapkg"), NA_character_)
  expect_equal(href_topic(ctx, "f", is_fun = TRUE), NA_character_)
  expect_equal(href_article(ctx, "intro", "notapkg"), NA_character_)
  expect_length(fetched$urls, 0)
})

test_that("a package that isn't installed links to its home page on CRAN's details", {
  fetched <- local_sites(
    cran = list(
      notapkg = c(Package = "notapkg", URL = "https://a.org, https://b.org"),
      nourl = c(Package = "nourl")
    )
  )
  expect_equal(href_package("notapkg"), "https://a.org")
  expect_equal(
    href_package("nourl"),
    "https://cran.r-project.org/package=nourl"
  )
  expect_equal(href_package("notoncran"), NA_character_)
  expect_equal(href_package("not/a package"), NA_character_)

  # Each is only looked up once, even the one that isn't on CRAN.
  expect_equal(href_package("notapkg"), "https://a.org")
  expect_equal(href_package("notoncran"), NA_character_)
  expect_equal(
    fetched$urls,
    sprintf(
      "https://cloud.r-project.org/web/packages/%s/DESCRIPTION",
      c("notapkg", "nourl", "notoncran")
    )
  )
})

test_that("help(package = ) of a package that isn't installed finds its site", {
  local_sites(
    list("https://a.org" = list(urls = list(reference = "https://a.org/ref"))),
    cran = list(
      hassite = c(URL = "https://a.org"),
      nosite = c(Package = "nosite")
    )
  )
  ctx <- link_context()
  expect_no_warning(url <- inline_ref(ctx, "help(package = \"hassite\")")$url)
  expect_equal(url, "https://a.org/ref")
  expect_equal(
    inline_ref(ctx, "help(package = \"nosite\")")$url,
    "https://rdrr.io/pkg/nosite/man"
  )
  expect_equal(
    inline_ref(ctx, "help(package = \"notoncran\")")$url,
    NA_character_
  )
})

test_that("a package that isn't installed isn't linked without a network", {
  local_sites()
  local_mocked_bindings(
    fetch_cran_description = function(url) list(value = NULL, failed = TRUE)
  )
  expect_equal(href_package("a"), NA_character_)
  expect_equal(href_package("b"), NA_character_)
  expect_warning(expect_equal(href_package("c"), NA_character_), "rdrr.io")
  expect_true(network_down())
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

test_that("a package links to its site before its repository", {
  fetched <- local_sites(
    cran = list(
      repofirst = c(URL = "https://github.com/a/b, https://a.github.io/b/"),
      gitlabfirst = c(URL = "https://gitlab.com/a/b, https://b.org"),
      twosites = c(URL = "https://b.org, https://c.org"),
      repoonly = c(URL = "https://github.com/a/b")
    )
  )
  expect_equal(href_package("repofirst"), "https://a.github.io/b/")
  expect_equal(href_package("gitlabfirst"), "https://b.org")
  expect_equal(href_package("twosites"), "https://b.org")
  expect_equal(href_package("repoonly"), "https://github.com/a/b")

  # The site's pkgdown.yml is looked for before the repository's.
  fetched$urls <- character()
  package_site("repofirst")
  expect_equal(fetched$urls[[1]], "https://a.github.io/b")
})

test_that("a site's pkgdown.yml is cached across sessions, but a failed download isn't", {
  local_sites()
  calls <- 0
  result <- list(
    value = list(urls = list(reference = "https://x.org/ref")),
    failed = FALSE
  )
  local_mocked_bindings(fetch_pkgdown_yml = function(url) {
    calls <<- calls + 1
    result
  })
  expect_equal(pkgdown_yml("https://x.org")$urls$reference, "https://x.org/ref")
  expect_equal(pkgdown_yml("https://x.org")$urls$reference, "https://x.org/ref")
  expect_equal(calls, 1)

  result <- list(value = NULL, failed = TRUE)
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
    value = list(urls = list(reference = "https://x.org/ref")),
    failed = FALSE
  )
  local_mocked_bindings(fetch_pkgdown_yml = function(url) result)
  pkgdown_yml("https://x.org")
  # A week later, without a network.
  file <- list.files(
    file.path(link_cache_dir(), "sites"),
    full.names = TRUE
  )
  entry <- readRDS(file)
  entry$time <- entry$time - 8 * 24 * 3600
  saveRDS(entry, file)
  result <- list(value = NULL, failed = TRUE)
  expect_equal(pkgdown_yml("https://x.org")$urls$reference, "https://x.org/ref")
})

test_that("after three failed downloads in a row, nothing more is downloaded", {
  local_sites()
  calls <- 0
  local_mocked_bindings(fetch_pkgdown_yml = function(url) {
    calls <<- calls + 1
    list(value = NULL, failed = TRUE)
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
      value = if (url != "https://down.org") list(),
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
  dir <- file.path(link_cache_dir(), "sites")
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
