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

# ---- code linking -------------------------------------------------------------

# A page with a code block, as Quarto writes one: `code` (HTML) in a line
# span after its anchor.
block_page <- function(code, class = "sourceCode r code-with-copy") {
  paste0(
    "<html><body><div class=\"sourceCode\" id=\"cb1\">",
    "<pre class=\"",
    class,
    "\"><code class=\"sourceCode r\">",
    "<span id=\"cb1-1\"><a href=\"#cb1-1\" aria-hidden=\"true\" tabindex=\"-1\"></a>",
    code,
    "</span></code></pre></div></body></html>"
  )
}

inline_page <- function(body) paste0("<html><body>", body, "</body></html>")

# The package "mypkg": foo() and its dataset `things`.
mypkg_ctx <- function(tips = TRUE, topic_path = "../reference/") {
  index <- list(
    foo = list(
      usage = "foo(a, b)",
      brief = "Adds a and b.",
      topic = "foo-topic"
    ),
    things = list(
      usage = NULL,
      brief = "Some things.",
      topic = "things",
      data = TRUE
    ),
    `%+%` = list(usage = "x %+% y", brief = "Adds.", topic = "plus"),
    `!!` = list(usage = NULL, brief = "Injects.", topic = "inject")
  )
  link_context(
    pkg = "mypkg",
    topics = vapply(index, `[[`, character(1), "topic"),
    names = "things",
    articles = c(intro = "vignettes/intro.html"),
    topic_path = topic_path,
    root = "../",
    index = index,
    tips = tips,
    external = "all"
  )
}

code_text <- function(html) {
  xml2::xml_text(xml2::xml_find_all(xml2::read_html(html), ".//pre"))
}

test_that("code blocks keep Quarto's markup and text, with calls linked", {
  local_sites()
  code <- paste0(
    "x <span class=\"ot\">&lt;-</span> stats<span class=\"sc\">::</span>",
    "<span class=\"fu\">median</span>(<span class=\"dv\">1</span>)\n",
    "<span class=\"fu\">sd</span>(x, <span class=\"at\">na.rm =</span> y)\n",
    "<span class=\"fu\">not_a_function_anywhere</span>()"
  )
  html <- link_code_html(block_page(code), ctx = link_context(external = "all"))

  expect_identical(code_text(html), code_text(block_page(code)))
  expect_match(
    html,
    "<pre class=\"sourceCode r code-with-copy\">",
    fixed = TRUE
  )
  expect_match(
    html,
    "<a href=\"#cb1-1\" aria-hidden=\"true\" tabindex=\"-1\"></a>",
    fixed = TRUE
  )
  # The package before `::` isn't linked, nor highlighted differently.
  expect_match(
    html,
    paste0(
      "x <span class=\"ot\">&lt;-</span> stats<span class=\"sc\">::</span>",
      "<span class=\"fu\"><a href=\"https://rdrr.io/r/stats/median.html\">median</a></span>"
    ),
    fixed = TRUE
  )
  expect_match(
    html,
    "<span class=\"fu\"><a href=\"https://rdrr.io/r/stats/sd.html\">sd</a></span>(x, <span class=\"at\">na.rm =</span> y)",
    fixed = TRUE
  )
  expect_match(
    html,
    "<span class=\"fu\">not_a_function_anywhere</span>",
    fixed = TRUE
  )
})

test_that("%op% operators are linked, other operators not", {
  local_sites()
  html <- link_code_html(
    block_page(paste0(
      "x <span class=\"sc\">%in%</span> y <span class=\"sc\">%not_an_op_anywhere%</span> z ",
      "<span class=\"sc\">+</span> <span class=\"dv\">1</span>"
    )),
    ctx = link_context(external = "all")
  )
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

test_that("library() attaches a package for the calls after it, and links it", {
  local_sites()
  ctx <- link_context(external = "all")
  html <- link_code_html(
    block_page(paste0(
      "<span class=\"fu\">library</span>(cli, <span class=\"at\">quietly =</span> <span class=\"cn\">TRUE</span>)\n",
      "<span class=\"fu\">cli_alert</span>()"
    )),
    ctx = ctx
  )
  expect_match(
    html,
    "(<a href=\"https://cli.r-lib.org\">cli</a>, ",
    fixed = TRUE
  )
  expect_match(html, "cli/man/cli_alert.html\">cli_alert</a>", fixed = TRUE)
  expect_equal(ctx$attached[[1]], "cli")
})

test_that("annotated blocks are linked, their markers left out of the code", {
  local_sites()
  html <- paste0(
    "<html><body><pre class=\"sourceCode r code-annotation-code code-annotated\">",
    "<code class=\"sourceCode r\">",
    "<span id=\"c-1\"><a href=\"#c-1\"></a>x <span class=\"sc\">&lt;-</span> <span class=\"fu\">sd</span>(y)</span>\n",
    "<a class=\"code-annotation-anchor\" href=\"\">1</a>",
    "<span id=\"c-2\" class=\"code-annotation-target\"><a href=\"#c-2\"></a><span class=\"fu\">mean</span>(x)</span>",
    "</code></pre></body></html>"
  )
  out <- link_code_html(html, ctx = link_context(external = "all"))
  expect_match(out, "stats/sd.html\">sd</a>", fixed = TRUE)
  expect_match(out, "base/mean.html\">mean</a>", fixed = TRUE)
  expect_match(
    out,
    "<a class=\"code-annotation-anchor\" href=\"\">1</a>",
    fixed = TRUE
  )
})

test_that("a block that doesn't parse is left as it is", {
  local_sites()
  page <- block_page("<span class=\"fu\">sd</span>(x")
  expect_no_match(link_code_html(page), "rdrr.io", fixed = TRUE)
})

test_that("the package's own topics are linked, with a tooltip, unless another package is named", {
  local_sites()
  ctx <- mypkg_ctx()
  html <- link_code_html(
    block_page(paste0(
      "<span class=\"fu\">foo</span>()\n",
      "mypkg<span class=\"sc\">::</span><span class=\"fu\">foo</span>()\n",
      "stats<span class=\"sc\">::</span><span class=\"fu\">sd</span>()\n",
      "x <span class=\"sc\">%+%</span> y"
    )),
    ctx = ctx
  )
  link <- "<a class=\"quartodown-tip-ref\" data-quartodown-tip=\"t1\" href=\"../reference/foo-topic.html\">foo</a>"
  expect_equal(lengths(regmatches(html, gregexpr(link, html, fixed = TRUE))), 2)
  expect_match(html, "mypkg<span class=\"sc\">::</span>", fixed = TRUE)
  expect_match(
    html,
    "data-quartodown-tip=\"t2\" href=\"../reference/plus.html\">%+%</a>",
    fixed = TRUE
  )
  expect_match(html, "rdrr.io/r/stats/sd.html", fixed = TRUE)
  # One tooltip per topic, at the end of the body.
  expect_match(html, "<div data-for=\"t1\">", fixed = TRUE)
  expect_match(html, "</div>\n</body>", fixed = TRUE)
  expect_length(ctx$tip_entries, 2)
})

test_that("only the package's own datasets and objects are linked by name", {
  local_sites()
  html <- link_code_html(
    block_page(paste0(
      "<span class=\"fu\">foo</span>(things, x)\n",
      "<span class=\"fu\">foo</span>(<span class=\"at\">things =</span> foo)"
    )),
    ctx = mypkg_ctx(tips = FALSE)
  )
  expect_match(
    html,
    "(<a href=\"../reference/things.html\">things</a>, x)",
    fixed = TRUE
  )
  # Not an argument's name, nor a function's name used as a value.
  expect_match(html, "<span class=\"at\">things =</span> foo)", fixed = TRUE)
})

test_that("methods aren't linked, but an R6 class's $new() links the class", {
  local_sites()
  ctx <- link_context(
    pkg = "mypkg",
    topics = c(Counter = "Counter", add = "add")
  )
  html <- link_code_html(
    block_page(paste0(
      "Counter<span class=\"sc\">$</span><span class=\"fu\">new</span>()\n",
      "x<span class=\"sc\">$</span><span class=\"fu\">add</span>(1)"
    )),
    ctx = ctx
  )
  expect_match(
    html,
    "<a href=\"reference/Counter.html\">Counter</a><span class=\"sc\">$</span>",
    fixed = TRUE
  )
  expect_match(html, "<span class=\"fu\">add</span>(1)", fixed = TRUE)
})

test_that("inline code that's a reference is linked", {
  local_sites(cli_site)
  html <- link_code_html(
    inline_page(paste0(
      "<p><code>foo()</code> <code>mypkg::foo(1, 2)</code> <code>?foo</code> ",
      "<code>?mypkg::foo</code> <code>help(foo)</code> <code>things</code> <code>!!</code> ",
      "<code>vignette(\"intro\")</code> <code>stats::sd()</code> <code>cli::cli_text(\"x\")</code> ",
      "<code>{cli}</code> <code>library(cli)</code> <code>break</code></p>"
    )),
    ctx = mypkg_ctx(tips = FALSE)
  )
  # A call links just its function.
  expect_match(
    html,
    "<code><a href=\"../reference/foo-topic.html\">foo</a>()</code>",
    fixed = TRUE
  )
  expect_match(
    html,
    "<code>mypkg::<a href=\"../reference/foo-topic.html\">foo</a>(1, 2)</code>",
    fixed = TRUE
  )
  for (code in c(
    "?foo",
    "?mypkg::foo",
    "help(foo)"
  )) {
    expect_match(
      html,
      paste0(
        "<code><a href=\"../reference/foo-topic.html\">",
        code,
        "</a></code>"
      ),
      fixed = TRUE
    )
  }
  expect_match(
    html,
    "<code><a href=\"../reference/things.html\">things</a></code>",
    fixed = TRUE
  )
  expect_match(
    html,
    "<code><a href=\"../reference/inject.html\">!!</a></code>",
    fixed = TRUE
  )
  expect_match(
    html,
    "<code><a href=\"../vignettes/intro.html\">vignette(\"intro\")</a></code>",
    fixed = TRUE
  )
  expect_match(
    html,
    "<code>stats::<a href=\"https://rdrr.io/r/stats/sd.html\">sd</a>()</code>",
    fixed = TRUE
  )
  expect_match(
    html,
    "<code>cli::<a href=\"https://cli.r-lib.org/reference/cli_text.html\">cli_text</a>(\"x\")</code>",
    fixed = TRUE
  )
  expect_match(
    html,
    "<code><a href=\"https://cli.r-lib.org\">{cli}</a></code>",
    fixed = TRUE
  )
  expect_match(
    html,
    "<code><a href=\"https://cli.r-lib.org\">library(cli)</a></code>",
    fixed = TRUE
  )
  expect_match(
    html,
    "<code><a href=\"https://rdrr.io/r/base/Control.html\">break</a></code>",
    fixed = TRUE
  )
})

test_that("inline code that isn't a reference isn't linked", {
  local_sites()
  page <- inline_page(paste0(
    "<p><code>x = 1</code> <code>~ sd(x)</code> <code>function(x) x</code> ",
    "<code>x</code> <code>sd</code> <code>{notapkg}</code> <code>not_a_function()</code> ",
    "<code>1 + 1</code></p>"
  ))
  expect_no_match(link_code_html(page, ctx = mypkg_ctx()), "<a ", fixed = TRUE)
})

test_that("inline code isn't linked in headings, summaries or other links", {
  local_sites()
  page <- inline_page(paste0(
    "<h2><code>foo()</code></h2><details><summary><code>foo()</code></summary></details>",
    "<a class=\"dropdown-item\" href=\"x.html\"><span>Using <code>foo()</code></span></a>",
    "<a href=\"y.html\">the <code>foo()</code> function</a>"
  ))
  expect_no_match(
    link_code_html(page, ctx = mypkg_ctx()),
    "reference/",
    fixed = TRUE
  )
})

test_that("a link around just the code is relinked to the package's own topic", {
  local_sites()
  html <- link_code_html(
    inline_page(paste0(
      "<p><a href=\"https://example.com\"><code>foo()</code></a> ",
      "<code><a href=\"foo.html\">foo</a>()</code> ",
      "<a href=\"https://example.com\"><code>sd()</code></a></p>"
    )),
    ctx = mypkg_ctx(tips = FALSE)
  )
  expect_match(
    html,
    "<p><code><a href=\"../reference/foo-topic.html\">foo</a>()</code> <code><a href=\"../reference/foo-topic.html\">foo</a>()</code> ",
    fixed = TRUE
  )
  # Not to another package's.
  expect_match(
    html,
    "<a href=\"https://example.com\"><code>sd()</code></a>",
    fixed = TRUE
  )
})

test_that("without tooltips, the package's own topics are only linked", {
  local_sites()
  html <- link_code_html(
    inline_page("<p><code>foo()</code></p>"),
    ctx = mypkg_ctx(tips = FALSE)
  )
  expect_match(
    html,
    "<code><a href=\"../reference/foo-topic.html\">foo</a>()</code>",
    fixed = TRUE
  )
  expect_no_match(html, "quartodown-tip", fixed = TRUE)
})

test_that("name_span() finds the function's name in inline code", {
  span <- function(text) {
    s <- name_span(text)
    substr(text, s[1], s[2])
  }
  expect_equal(span("foo()"), "foo")
  expect_equal(span("foo(x = 1)"), "foo")
  expect_equal(span("pkg::foo(x)"), "foo")
  expect_equal(span("pkg:::foo()"), "foo")
  expect_equal(span("pkg::foo"), "foo")
  expect_equal(span(" break "), "break")
})
