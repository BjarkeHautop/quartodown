# A package with a _pkgdown.yml using most of what translate_pkgdown()
# handles: vignettes at the top level and in articles/, a NEWS.md, an
# exported topic linking an internal one, and pkgdown/extra.scss.
new_pkgdown_pkg <- function(.local_envir = parent.frame()) {
  dir <- new_pkg_with_man(.local_envir)
  writeLines(
    c("Package: examplepkg", "URL: https://github.com/me/examplepkg"),
    file.path(dir, "DESCRIPTION")
  )
  writeLines(
    c(
      "\\name{foo}",
      "\\alias{foo}",
      "\\title{Foo}",
      "\\usage{foo()}",
      "\\description{Uses \\link{helper}.}"
    ),
    file.path(dir, "man", "foo.Rd")
  )
  write_rd(dir, "helper", keyword = "internal")
  dir.create(file.path(dir, "vignettes", "articles"), recursive = TRUE)
  write_vignette(dir, "examplepkg.Rmd", "Get going")
  write_vignette(dir, "basics.Rmd", "Basics")
  write_vignette(dir, "extras.qmd", "Extras")
  write_vignette(dir, "articles/advanced.Rmd", "Advanced")
  writeLines("# examplepkg 1.0.0", file.path(dir, "NEWS.md"))
  dir.create(file.path(dir, "pkgdown"))
  writeLines(
    ".navbar { font-weight: bold; }",
    file.path(dir, "pkgdown", "extra.scss")
  )
  writeLines(
    c(
      "url: https://me.github.io/examplepkg",
      "title: Example",
      "development:",
      "  mode: auto",
      "  version_label: danger",
      "home:",
      "  title: An example package",
      "  description: Does examples.",
      "  links:",
      "  - text: Blog",
      "    href: https://example.com",
      "template:",
      "  bootstrap: 5",
      "  package: sometemplate",
      "  bootswatch: flatly",
      "  bslib:",
      "    primary: '#0054AD'",
      "    base_font: {google: Inter}",
      "  includes:",
      "    in_header: <script src='analytics.js'></script>",
      "    before_navbar: <div>Hi</div>",
      "  opengraph:",
      "    image:",
      "      src: man/figures/card.png",
      "      alt: A card",
      "    twitter:",
      "      creator: '@me'",
      "      card: summary_large_image",
      "  math-rendering: katex",
      "  params:",
      "    ganalytics: G-123",
      "lang: da",
      "toc:",
      "  depth: 3",
      "figures:",
      "  fig.width: 8",
      "  dev: ragg::agg_png",
      "navbar:",
      "  structure:",
      "    left: [intro, reference, articles, guides, news]",
      "    right: [search, github, mastodon]",
      "  components:",
      "    guides:",
      "      text: Guides",
      "      menu:",
      "      - text: Concepts",
      "      - text: Basics",
      "        href: articles/basics.html",
      "      - text: The Foo topic",
      "        href: reference/foo.html",
      "      - text: -------",
      "      - text: Old index",
      "        href: articles/index.html",
      "    mastodon:",
      "      icon: fab fa-mastodon fa-lg",
      "      href: https://fosstodon.org/@me",
      "      aria-label: Mastodon",
      "articles:",
      "- title: Main",
      "  navbar: ~",
      "  contents: [basics, starts_with('ext')]",
      "- title: Deeper",
      "  contents: [articles/advanced]",
      "reference:",
      "- title: Functions",
      "  contents: [foo]",
      "redirects:",
      "- ['old.html', 'articles/basics.html']",
      "footer:",
      "  structure:",
      "    left: developed_by",
      "news:",
      "  one_page: true",
      "search:",
      "  exclude: ['news/index.html']"
    ),
    file.path(dir, "_pkgdown.yml")
  )
  dir
}

test_that("translate_pkgdown() translates a _pkgdown.yml", {
  dir <- new_pkgdown_pkg()
  translation <- translate_pkgdown(dir, "_pkgdown.yml")

  expect_snapshot({
    writeLines(pkgdown_config_yml(translation$config, "_pkgdown.yml"))
    writeLines(translation$skipped)
    translation$files
  })

  # The result is a valid configuration.
  config <- yaml::yaml.load(paste(
    pkgdown_config_yml(translation$config, "_pkgdown.yml"),
    collapse = "\n"
  ))
  expect_no_error(navbar_layout(dir, config))
  expect_true(config$`internal-pages`)
})

test_that("translate_pkgdown() keeps a package without articles: on the articles keyword", {
  dir <- new_pkg_with_man()
  writeLines("Package: examplepkg", file.path(dir, "DESCRIPTION"))
  writeLines("url: https://example.com", file.path(dir, "_pkgdown.yml"))

  translation <- translate_pkgdown(dir, "_pkgdown.yml")
  expect_identical(
    translation$config$navbar,
    list(
      left = list("intro", "reference", "articles", "news"),
      tools = list("repo")
    )
  )
  expect_null(translation$config$`internal-pages`)
  expect_null(translation$config$redirects)
  expect_identical(translation$skipped, character())
})

test_that("pkgdown_href() maps pkgdown's addresses to quartodown's", {
  dir <- new_pkg_with_man()
  writeLines("Package: examplepkg", file.path(dir, "DESCRIPTION"))
  file.create(file.path(dir, c("NEWS.md", "LICENSE.md")))
  vignettes <- c("intro.Rmd", "articles/deep.qmd")

  href <- function(x) pkgdown_href(x, dir, vignettes)
  expect_identical(
    href("https://x.org/a.html"),
    list(href = "https://x.org/a.html")
  )
  expect_identical(href("index.html"), list(href = "index.qmd"))
  expect_identical(href("reference/"), list(href = "reference/index.qmd"))
  expect_identical(
    href("reference/foo.html#arguments"),
    list(href = "reference/foo.qmd#arguments")
  )
  expect_identical(href("news/index.html"), list(href = "NEWS.md"))
  expect_identical(href("authors.html"), list(href = "authors.qmd"))
  expect_identical(href("LICENSE.html"), list(href = "LICENSE.md"))
  expect_identical(href("articles/intro.html"), list(vignette = "intro.Rmd"))
  expect_identical(
    href("./articles/deep.html"),
    list(vignette = "articles/deep.qmd")
  )
  expect_identical(
    href("articles/deep.html#x"),
    list(href = "vignettes/articles/deep.qmd#x")
  )
  expect_null(href("articles/index.html"))
  expect_null(href("articles/missing.html"))
  expect_null(href("CONTRIBUTING.html"))
})

test_that("pkgdown_icon() turns Font Awesome classes into Bootstrap Icons names", {
  expect_identical(pkgdown_icon("fab fa-github fa-lg"), "github")
  expect_identical(pkgdown_icon("fa-brands fa-x-twitter"), "twitter-x")
  expect_identical(pkgdown_icon("fas fa-home fa-2x"), "house")
  expect_identical(pkgdown_icon("bi-github"), "bi-github")
})

test_that("use_quartodown() translates _pkgdown.yml, keeping existing files", {
  dir <- new_pkgdown_pkg()
  dir.create(file.path(dir, "_quartodown"))
  writeLines("// mine", file.path(dir, "_quartodown", "extra.scss"))

  expect_message(use_quartodown(dir), "translated from")
  config <- yaml::read_yaml(file.path(dir, "_quartodown", "_quartodown.yml"))
  expect_identical(config$theme$light, "flatly")
  expect_identical(
    read_utf8(file.path(dir, "_quartodown", "extra.scss")),
    "// mine"
  )
  expect_true(file.exists(file.path(dir, "_quartodown", "in-header.html")))
})
