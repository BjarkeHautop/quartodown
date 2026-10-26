write_rd <- function(dir, name, content) {
  dir.create(file.path(dir, "man"), showWarnings = FALSE)
  writeLines(content, file.path(dir, "man", paste0(name, ".Rd")))
}

sample_html <- function(body) {
  paste0("<html><head><title>t</title></head><body>", body, "</body></html>")
}

foo_index <- list(
  foo = list(usage = "foo(a, b)", brief = "Adds a and b.", topic = "foo")
)

# ---- build_topic_index() ------------------------------------------------------

test_that("build_topic_index() extracts usage and first-sentence brief", {
  dir <- withr::local_tempdir()
  write_rd(
    dir,
    "foo",
    c(
      "\\name{foo}",
      "\\alias{foo}",
      "\\title{Add two numbers}",
      "\\usage{",
      "foo(a, b)",
      "}",
      "\\description{",
      "Adds \\code{a} and \\code{b} together and returns the sum. See details.",
      "}"
    )
  )

  index <- build_topic_index(dir)

  expect_named(index, "foo")
  expect_equal(index$foo$usage, "foo(a, b)")
  expect_equal(
    index$foo$brief,
    "Adds `a` and `b` together and returns the sum."
  )
  expect_equal(index$foo$topic, "foo")
})

test_that("build_topic_index() narrows a multi-alias \\usage{} block by name", {
  dir <- withr::local_tempdir()
  write_rd(
    dir,
    "print.foo",
    c(
      "\\name{print.foo}",
      "\\alias{print.foo}",
      "\\alias{foo}",
      "\\title{Foo methods}",
      "\\usage{",
      "foo(x)",
      "",
      "\\method{print}{foo}(x, ...)",
      "}",
      "\\description{",
      "Prints a foo object.",
      "}"
    )
  )

  index <- build_topic_index(dir)

  expect_setequal(names(index), c("print.foo", "foo"))
  expect_equal(index[["print.foo"]]$brief, index[["foo"]]$brief)
  expect_equal(index[["foo"]]$usage, "foo(x)")
  expect_equal(index[["print.foo"]]$usage, "print.foo(x, ...)")
  expect_equal(index[["foo"]]$topic, "print.foo")
})

test_that("build_topic_index() matches a \\usage{} block wrapped over several lines", {
  dir <- withr::local_tempdir()
  write_rd(
    dir,
    "glue",
    c(
      "\\name{glue}",
      "\\alias{glue}",
      "\\alias{glue_data}",
      "\\title{Format a string}",
      "\\usage{",
      "glue_data(",
      "  .x,",
      "  ...",
      ")",
      "",
      "glue(...)",
      "}",
      "\\description{",
      "Formats a string.",
      "}"
    )
  )

  expect_no_warning(index <- build_topic_index(dir))
  expect_equal(index$glue_data$usage, "glue_data(\n  .x,\n  ...\n)")
  expect_equal(index$glue$usage, "glue(...)")
})

test_that("build_topic_index() skips internal topics, which get no page", {
  dir <- withr::local_tempdir()
  write_rd(
    dir,
    "helper",
    c(
      "\\name{helper}",
      "\\alias{helper}",
      "\\title{Helper}",
      "\\usage{",
      "helper()",
      "}",
      "\\description{",
      "Helps.",
      "}",
      "\\keyword{internal}"
    )
  )

  expect_length(build_topic_index(dir), 0)

  # Unless the reference index lists it, which gives it a page.
  config <- list(reference = list(list(contents = "helper")))
  expect_identical(build_topic_index(dir, config)$helper$topic, "helper")
})

test_that("an alias with no \\usage{} block of its own gets no signature", {
  dir <- withr::local_tempdir()
  write_rd(
    dir,
    "print.foo",
    c(
      "\\name{print.foo}",
      "\\alias{print.foo}",
      "\\alias{foo}",
      "\\title{Foo methods}",
      "\\usage{",
      "\\method{print}{foo}(x, ...)",
      "}",
      "\\description{",
      "Prints a foo object.",
      "}"
    )
  )

  expect_no_warning(index <- build_topic_index(dir))
  expect_null(index[["foo"]]$usage)
  expect_equal(index[["foo"]]$brief, "Prints a foo object.")
  expect_equal(index[["print.foo"]]$usage, "print.foo(x, ...)")
})

test_that("a shared topic's own name doesn't need a \\usage{} block", {
  dir <- withr::local_tempdir()
  write_rd(
    dir,
    "foo-queries",
    c(
      "\\name{foo-queries}",
      "\\alias{foo-queries}",
      "\\alias{foo}",
      "\\alias{bar}",
      "\\title{Foo queries}",
      "\\usage{",
      "foo(x)",
      "",
      "bar(x)",
      "}",
      "\\description{",
      "Queries a foo.",
      "}"
    )
  )

  expect_no_warning(index <- build_topic_index(dir))
  expect_equal(index[["bar"]]$usage, "bar(x)")
  expect_equal(index[["foo-queries"]]$usage, "foo(x)\n\nbar(x)")
})

test_that("an infix operator alias matches its \\usage{} block", {
  dir <- withr::local_tempdir()
  write_rd(
    dir,
    "ops",
    c(
      "\\name{ops}",
      "\\alias{ops}",
      "\\alias{\\%+-\\%}",
      "\\alias{\\%-+\\%}",
      "\\title{Operators}",
      "\\usage{",
      "lhs \\%+-\\% rhs",
      "",
      "lhs \\%-+\\% rhs",
      "}",
      "\\description{",
      "Two operators.",
      "}"
    )
  )

  expect_no_warning(index <- build_topic_index(dir))
  expect_equal(index[["%+-%"]]$usage, "lhs %+-% rhs")
  expect_equal(index[["%-+%"]]$usage, "lhs %-+% rhs")
})

test_that("replacement functions and S4 methods match their \\usage{} blocks", {
  dir <- withr::local_tempdir()
  write_rd(
    dir,
    "units",
    c(
      "\\name{units}",
      "\\alias{units}",
      "\\alias{units<-}",
      "\\alias{units<-.foo}",
      "\\alias{units,Foo-method}",
      "\\alias{units<-,Foo,character-method}",
      "\\title{Units}",
      "\\usage{",
      "units(x)",
      "",
      "units(x) <- value",
      "",
      "\\method{units}{foo}(x) <- value",
      "",
      "\\S4method{units}{Foo}(x)",
      "",
      "\\S4method{units}{Foo,character}(x) <- value",
      "}",
      "\\description{",
      "Gets or sets units.",
      "}"
    )
  )

  expect_no_warning(index <- build_topic_index(dir))
  expect_equal(index[["units"]]$usage, "units(x)")
  expect_equal(index[["units<-"]]$usage, "units(x) <- value")
  expect_equal(index[["units<-.foo"]]$usage, "units.foo(x) <- value")
  expect_equal(index[["units,Foo-method"]]$usage, "units(x)")
  expect_equal(
    index[["units<-,Foo,character-method"]]$usage,
    "units(x) <- value"
  )
})

test_that("a lifecycle badge is left out of the brief", {
  dir <- withr::local_tempdir()
  write_rd(
    dir,
    "foo",
    c(
      "\\name{foo}",
      "\\alias{foo}",
      "\\title{Foo}",
      "\\usage{foo()}",
      "\\description{",
      "\\ifelse{html}{\\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\\strong{[Experimental]}}",
      "",
      "Does \\href{https://example.com}{a thing}.\\if{latex}{ In LaTeX.}",
      "}"
    )
  )

  expect_equal(build_topic_index(dir)$foo$brief, "Does a thing.")
})

test_that("a description longer than 200 characters warns and clips with '...'", {
  dir <- withr::local_tempdir()
  # no sentence boundary at all
  write_rd(
    dir,
    "foo",
    c(
      "\\name{foo}",
      "\\alias{foo}",
      "\\title{Foo}",
      "\\usage{",
      "foo()",
      "}",
      "\\description{",
      paste(rep("word", 60), collapse = " "),
      "}"
    )
  )
  expect_warning(index <- build_topic_index(dir), "truncated mid-word")
  expect_true(endsWith(index$foo$brief, "..."))
  expect_no_warning(build_topic_index(dir, quiet = TRUE))

  # one well-formed sentence that simply runs past 200 characters
  write_rd(
    dir,
    "foo",
    c(
      "\\name{foo}",
      "\\alias{foo}",
      "\\title{Foo}",
      "\\usage{",
      "foo()",
      "}",
      "\\description{",
      paste0(paste(rep("word", 45), collapse = " "), "."),
      "}"
    )
  )
  expect_warning(index <- build_topic_index(dir), "truncated mid-word")
  expect_true(endsWith(index$foo$brief, "..."))
})

test_that("a topic with no description prose has a NULL brief", {
  dir <- withr::local_tempdir()
  write_rd(
    dir,
    "bar",
    c(
      "\\name{bar}",
      "\\alias{bar}",
      "\\title{Bar}",
      "\\usage{",
      "bar()",
      "}",
      "\\description{",
      "}"
    )
  )
  expect_null(build_topic_index(dir)$bar$brief)
})

# ---- inject_tooltips_html() ---------------------------------------------------

test_that("a downlit-linked code block reference is relinked to the local man page and gets a tooltip", {
  html <- sample_html(paste0(
    "<code class=\"sourceCode R\"><span class=\"fu\">",
    "<a href=\"https://rdrr.io/pkg/p/man/foo.html\">foo</a></span>",
    "<span class=\"op\">(</span></code>"
  ))

  result <- inject_tooltips_html(html, foo_index, "reference/")

  expect_equal(result$n, 1L)
  expect_length(result$tips, 1L)
  expect_match(
    result$html,
    '<span class="fu"><a class="quartodown-tip-ref" data-quartodown-tip="t1" href="reference/foo.html">foo</a></span>',
    fixed = TRUE
  )
  expect_equal(result$tips[["t1"]]$usage, "foo(a, b)")
})

test_that("an unlinked fu span is linked and tagged too", {
  html <- sample_html(
    "<span class=\"fu\">foo</span><span class=\"op\">(</span>"
  )
  result <- inject_tooltips_html(html, foo_index, "reference/")
  expect_equal(result$n, 1L)
  expect_match(
    result$html,
    'data-quartodown-tip="t1" href="reference/foo.html">foo</a></span>',
    fixed = TRUE
  )
})

test_that("a kw span (a function Pandoc highlights as a keyword) is tagged too", {
  index <- list(
    R6Class = list(
      usage = "R6Class()",
      brief = "Makes a class.",
      topic = "R6Class"
    )
  )
  html <- sample_html(paste0(
    "<span class=\"kw\"><a href=\"https://r6.r-lib.org/reference/R6Class.html\">",
    "R6Class</a></span><span class=\"op\">(</span>",
    "<span class=\"kw\">function</span>"
  ))

  result <- inject_tooltips_html(html, index, "reference/")

  expect_equal(result$n, 1L)
  expect_match(
    result$html,
    '<span class="kw"><a class="quartodown-tip-ref" data-quartodown-tip="t1" href="reference/R6Class.html">R6Class</a></span>',
    fixed = TRUE
  )
  expect_match(result$html, "<span class=\"kw\">function</span>", fixed = TRUE)
})

test_that("an own %op% operator in a code block is linked and tagged", {
  index <- list(
    `%+deg%` = list(
      usage = "x %+deg% n",
      brief = "Adds degrees.",
      topic = "deg"
    )
  )
  html <- paste0(
    # downlit's class, then Quarto's in an annotated block.
    "<span class=\"op\">%+deg%</span> <span class=\"sc\">%+deg%</span> ",
    "<span class=\"op\"><a href=\"https://rdrr.io/r/base/match.html\">%in%</a></span> ",
    "<span class=\"op\">(</span>"
  )

  result <- inject_tooltips_html(html, index)

  expect_equal(result$n, 2L)
  link <- paste0(
    "<a class=\"quartodown-tip-ref\" data-quartodown-tip=\"t1\" ",
    "href=\"reference/deg.html\">%+deg%</a>"
  )
  expect_match(
    result$html,
    paste0("<span class=\"op\">", link, "</span>"),
    fixed = TRUE
  )
  expect_match(
    result$html,
    paste0("<span class=\"sc\">", link, "</span>"),
    fixed = TRUE
  )
  expect_match(result$html, "match.html\">%in%</a></span>", fixed = TRUE)
  expect_match(result$html, "<span class=\"op\">(</span>", fixed = TRUE)
})

test_that("a va-class span (e.g. an R6 class name used as a value) is tagged too", {
  index <- list(
    Counter = list(usage = NULL, brief = "An R6 counter.", topic = "Counter")
  )
  html <- sample_html(paste0(
    "<span class=\"va\"><a href=\"https://rdrr.io/pkg/p/man/Counter.html\">Counter</a></span>",
    "<span class=\"op\">$</span><span class=\"fu\">new</span>"
  ))

  result <- inject_tooltips_html(html, index, "reference/")

  expect_equal(result$n, 1L)
  expect_match(
    result$html,
    '<span class="va"><a class="quartodown-tip-ref"',
    fixed = TRUE
  )
  expect_match(result$html, 'href="reference/Counter.html"', fixed = TRUE)
  expect_equal(result$tips[["t1"]]$name, "Counter")
})

test_that("inline code references are linked and tagged, with or without a downlit link", {
  html <- sample_html(paste0(
    "<p>Add numbers with <code><a href=\"https://rdrr.io/pkg/p/man/foo.html\">foo()</a></code>:</p>"
  ))
  result <- inject_tooltips_html(html, foo_index, "reference/")
  expect_equal(result$n, 1L)
  expect_match(
    result$html,
    '<code><a class="quartodown-tip-ref" data-quartodown-tip="t1" href="reference/foo.html">foo()</a></code>',
    fixed = TRUE
  )

  # e.g. an S3 method written out by name, which downlit leaves unlinked
  index <- list(
    print.animal = list(
      usage = "print.animal(x, ...)",
      brief = NULL,
      topic = "animal"
    )
  )
  html <- sample_html("<p>See <code>print.animal()</code> for details.</p>")
  result <- inject_tooltips_html(html, index, "reference/")
  expect_match(
    result$html,
    'href="reference/animal.html">print.animal()</a></code>',
    fixed = TRUE
  )
})

test_that("an inline reference to a backtick-quoted name resolves through its backticks", {
  index <- list(
    `name<-` = list(
      usage = "name(x) <- value",
      brief = "Set the name.",
      topic = "animal"
    )
  )
  html <- sample_html("<p>See <code>`name&lt;-`()</code>.</p>")
  result <- inject_tooltips_html(html, index, "reference/")
  expect_equal(result$n, 1L)
  expect_equal(result$tips[["t1"]]$usage, "name(x) <- value")
})

test_that("references qualified with the package's own namespace resolve, others don't", {
  html <- sample_html(
    "<p><code>p::foo()</code> and <code>p:::foo(1, 2)</code></p>"
  )
  result <- inject_tooltips_html(html, foo_index, "reference/", pkg = "p")
  expect_equal(result$n, 2L)
  expect_length(result$tips, 1L)
  expect_match(
    result$html,
    'href="reference/foo.html">p::foo()</a></code>',
    fixed = TRUE
  )

  html <- sample_html("<p><code>other::foo()</code></p>")
  result <- inject_tooltips_html(html, foo_index, "reference/", pkg = "p")
  expect_equal(result$n, 0L)
  expect_equal(result$html, html)
})

test_that("a help-operator reference like ?foo or ?pkg::foo resolves", {
  html <- sample_html("<p><code>?foo</code> and <code>?p::foo</code></p>")
  result <- inject_tooltips_html(html, foo_index, "reference/", pkg = "p")
  expect_equal(result$n, 2L)
  expect_match(
    result$html,
    'href="reference/foo.html">?p::foo</a></code>',
    fixed = TRUE
  )
})

test_that("inline code in a Markdown link is relinked, not nested in another link", {
  html <- '<a href="https://example.com"><code>foo()</code></a>'
  result <- inject_tooltips_html(html, foo_index)
  expect_identical(
    result$html,
    '<code><a class="quartodown-tip-ref" data-quartodown-tip="t1" href="reference/foo.html">foo()</a></code>'
  )
  # A link around code that isn't the package's own stays as it is.
  other <- '<a href="https://example.com"><code>bar()</code></a>'
  expect_identical(inject_tooltips_html(other, foo_index)$html, other)
})

test_that("references to names not in the index are left untouched", {
  html <- sample_html(paste0(
    "<span class=\"fu\"><a href=\"https://rdrr.io/r/base/library.html\">library</a></span>",
    "<span class=\"fu\">bar</span>"
  ))
  result <- inject_tooltips_html(html, foo_index, "reference/")
  expect_equal(result$n, 0L)
  expect_equal(result$html, html)
})

test_that("repeated references to the same name share one tip id", {
  link <- "<span class=\"fu\"><a href=\"foo.html\">foo</a></span>"
  result <- inject_tooltips_html(
    sample_html(paste(link, link)),
    foo_index,
    "reference/"
  )
  expect_equal(result$n, 2L)
  expect_length(result$tips, 1L)
  expect_length(
    gregexpr('data-quartodown-tip="t1"', result$html, fixed = TRUE)[[1]],
    2L
  )
})

test_that("reference_href_prefix() points at docs/reference/ relative to the page's depth", {
  expect_equal(
    reference_href_prefix("/x/docs/index.html", "/x/docs"),
    "reference/"
  )
  expect_equal(
    reference_href_prefix("/x/docs/vignettes/using.html", "/x/docs"),
    "../reference/"
  )
  expect_equal(
    reference_href_prefix("/x/docs/reference/foo.html", "/x/docs"),
    "./"
  )
})

# ---- tooltip payload ----------------------------------------------------------

test_that("a topic with no \\usage{} shows its name as the signature", {
  tips <- list(
    t1 = list(usage = NULL, brief = "An R6 counter.", name = "Counter")
  )
  expect_match(
    tooltip_payload_html(tips),
    '<code class="quartodown-tip-sig">Counter</code>',
    fixed = TRUE
  )
})

test_that("code in the brief is rendered as <code>, with its contents escaped", {
  tips <- list(
    t1 = list(usage = "f()", brief = "Stored in `dir(\"a\") < b`.", name = "f")
  )
  expect_match(
    tooltip_payload_html(tips),
    '<span class="quartodown-tip-brief">Stored in <code>dir(&quot;a&quot;) &lt; b</code>.</span>',
    fixed = TRUE
  )
})

# ---- add_tooltips_html() / postprocess_pages() --------------------------------

test_that("add_tooltips_html() adds the tips' contents before </body>", {
  html <- sample_html("<p><code>foo()</code></p>")
  result <- add_tooltips_html(html, foo_index)
  expect_equal(result$n, 1L)
  expect_match(
    result$html,
    "Adds a and b.</span></div></div>\n</body></html>$"
  )

  # Without any reference, the page is unchanged.
  other <- sample_html("<p><code>bar()</code></p>")
  expect_identical(add_tooltips_html(other, foo_index)$html, other)
})

test_that("postprocess_pages() links and adds tooltips to pages on disk", {
  dir <- withr::local_tempdir()
  writeLines("Package: p", file.path(dir, "DESCRIPTION"))
  write_rd(
    dir,
    "foo",
    c(
      "\\name{foo}",
      "\\alias{foo}",
      "\\title{Foo}",
      "\\usage{",
      "foo(a, b)",
      "}",
      "\\description{",
      "Adds a and b.",
      "}"
    )
  )
  dir.create(file.path(dir, "docs", "vignettes"), recursive = TRUE)
  page <- file.path(dir, "docs", "vignettes", "a.html")
  writeLines(sample_html("<p>Use <code>foo()</code>.</p>"), page)

  postprocess_pages(dir)
  html <- paste(readLines(page), collapse = "\n")
  expect_match(html, 'href="../reference/foo.html">foo()</a>', fixed = TRUE)
  expect_match(
    html,
    '<div class="quartodown-tip-payload" hidden>',
    fixed = TRUE
  )
})
