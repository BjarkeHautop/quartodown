annotated_page <- function(code) {
  paste0(
    "<html><body><div class=\"sourceCode\">",
    "<pre class=\"sourceCode r code-annotation-code code-annotated\">",
    "<code class=\"sourceCode r\">",
    "<span id=\"cell-1\" class=\"code-annotation-target\">",
    code,
    "</span></code></pre></div></body></html>"
  )
}

test_that("link_code_html() drops the other site's content", {
  page <- paste0(
    "<html><body>",
    "<div class=\"quartodown-devel\">dev</div>",
    "<p><span class=\"x quartodown-release\">release</span></p>",
    "<p class=\"quartodown-releases\">kept</p>",
    "</body></html>"
  )
  html <- link_code_html(page, drop = other_site_classes("devel"))
  expect_match(html, "dev</div>")
  expect_no_match(html, ">release<")
  expect_match(html, "kept")

  html <- link_code_html(page, drop = other_site_classes("release"))
  expect_no_match(html, "dev</div>")
  expect_match(html, ">release<")
  expect_identical(
    other_site_classes("unreleased"),
    other_site_classes("devel")
  )
})

test_that("annotated blocks keep their markup and get calls linked", {
  html <- annotated_page(paste0(
    "x <span class=\"ot\">&lt;-</span> stats<span class=\"sc\">::</span>",
    "<span class=\"fu\">median</span>(<span class=\"dv\">1</span>) ",
    "<span class=\"fu\">sd</span>(x) ",
    "<span class=\"fu\">not_a_function_anywhere</span>()"
  ))
  html <- link_code_html(html)

  expect_match(html, "code-annotation-target", fixed = TRUE)
  expect_match(html, "<span class=\"ot\">", fixed = TRUE)
  expect_match(
    html,
    "x <span class=\"ot\">&lt;-</span> <span class=\"fu\">stats</span><span class=\"fu\">::</span>",
    fixed = TRUE
  )
  expect_match(
    html,
    "<span class=\"fu\"><a href=\"https://rdrr.io/r/stats/median.html\">median</a></span>",
    fixed = TRUE
  )
  expect_match(
    html,
    "<span class=\"fu\"><a href=\"https://rdrr.io/r/stats/sd.html\">sd</a></span>",
    fixed = TRUE
  )
  expect_match(
    html,
    "<span class=\"fu\">not_a_function_anywhere</span>",
    fixed = TRUE
  )
})

test_that("annotated blocks get %op% operators linked, other operators not", {
  html <- annotated_page(paste0(
    "x <span class=\"sc\">%in%</span> y ",
    "<span class=\"sc\">%not_an_op_anywhere%</span> ",
    "<span class=\"sc\">+</span> <span class=\"dv\">1</span>"
  ))
  html <- link_code_html(html)

  expect_match(
    html,
    "<span class=\"sc\"><a href=\"https://rdrr.io/r/base/match.html\">%in%</a></span>",
    fixed = TRUE
  )
  expect_match(
    html,
    "<span class=\"sc\">%not_an_op_anywhere%</span>",
    fixed = TRUE
  )
  expect_match(html, "<span class=\"sc\">+</span>", fixed = TRUE)
})

test_that("library() calls in an annotated block attach packages", {
  skip_if_not_installed("pkgload")
  html <- annotated_page(paste0(
    "<span class=\"fu\">library</span>(pkgload)\n",
    "<span class=\"fu\">load_all</span>()"
  ))
  html <- link_code_html(html)

  expect_match(html, "load_all.html\">load_all</a>", fixed = TRUE)
})

test_that("the package's own topics are linked from `topics`", {
  html <- annotated_page(paste0(
    "<span class=\"fu\">library</span>(mypkg)\n",
    "<span class=\"fu\">foo</span>() ",
    "mypkg<span class=\"sc\">::</span><span class=\"fu\">foo</span>()"
  ))
  html <- link_code_html(
    html,
    pkg = "mypkg",
    topics = c(foo = "foo-topic"),
    topic_path = "../reference/"
  )

  expect_match(
    html,
    "<span class=\"fu\"><a href=\"../reference/foo-topic.html\">foo</a></span>() ",
    fixed = TRUE
  )
  expect_match(
    html,
    "<span class=\"fu\"><a href=\"../reference/foo-topic.html\">foo</a></span>()</span>",
    fixed = TRUE
  )
})

test_that("call_namespace() reads the package before ::", {
  html <- xml2::read_html(paste0(
    "<code>a <span class=\"sc\">+</span><span class=\"fu\">f</span>",
    " stats<span class=\"sc\">::</span><span class=\"fu\">g</span>",
    "<span class=\"fu\">h</span></code>"
  ))
  calls <- xml2::xml_find_all(html, ".//span[@class = 'fu']")
  expect_null(call_namespace(calls[[1]]))
  expect_equal(call_namespace(calls[[2]])$prefix, "stats::")
  expect_null(call_namespace(calls[[3]]))
})

test_that("add_logo_html() puts the logo before the page's first <h1>", {
  page <- paste0(
    "<nav><h1>Not this</h1></nav>",
    "<main id=\"quarto-document-content\"><header></header>",
    "<h1>News</h1><h1>Older</h1></main>"
  )
  html <- add_logo_html(page, "man/figures/logo.png", "..")
  expect_match(
    html,
    "<header></header><img src=\"../man/figures/logo.png\" class=\"quartodown-logo\" alt=\"\"><h1>News",
    fixed = TRUE
  )
  expect_match(html, "<nav><h1>Not this", fixed = TRUE)

  logo <- c(light = "man/figures/logo-light.png", dark = "man/figures/logo.png")
  html <- add_logo_html(page, logo)
  expect_match(html, "class=\"quartodown-logo light-content\"", fixed = TRUE)
  expect_match(html, "class=\"quartodown-logo dark-content\"", fixed = TRUE)

  expect_identical(add_logo_html(page, NULL), page)
})

test_that("add_logo_html() keeps a grid page's intro beside the logo", {
  page <- function(intro) {
    paste0(
      "<main class=\"content page-columns page-full\" ",
      "id=\"quarto-document-content\">",
      "<header id=\"title-block-header\"><h1>Title</h1></header>",
      intro,
      "<section><h2>A</h2></section></main>"
    )
  }
  html <- add_logo_html(page("<p>Intro</p>"), "man/figures/logo.svg")
  expect_match(
    html,
    "<div class=\"quartodown-title-block\"><header .*<p>Intro</p></div><section>"
  )

  # Content placed in a column of its own stays a grid item.
  html <- add_logo_html(
    page("<div class=\"column-margin\">Aside</div>"),
    "man/figures/logo.svg"
  )
  expect_no_match(html, "quartodown-title-block")
})
