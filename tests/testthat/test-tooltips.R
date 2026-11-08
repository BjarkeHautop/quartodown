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

test_that("build_topic_index() extracts usage and the title as brief", {
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
  expect_equal(index$foo$brief, "Add two numbers")
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

  # Also in a hidden section, selecting every internal topic.
  config <- list(
    reference = list(
      list(title = "internal", contents = "has_keyword(\"internal\")")
    )
  )
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
  expect_equal(index[["foo"]]$brief, "Foo methods")
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

test_that("the brief is the title as plain text, with code in backticks", {
  dir <- withr::local_tempdir()
  write_rd(
    dir,
    "foo",
    c(
      "\\name{foo}",
      "\\alias{foo}",
      "\\title{Do \\code{foo} with",
      "  \\href{https://example.com}{a thing}}",
      "\\usage{foo()}",
      "\\description{Does it.}"
    )
  )

  expect_equal(build_topic_index(dir)$foo$brief, "Do `foo` with a thing")
})

# ---- reference_href_prefix() --------------------------------------------------

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

test_that("usage_html() highlights a signature like a code block, without links", {
  html <- usage_html("foo(a = \"<b>\", n = 1, f = interactive())")
  expect_match(html, '<span class="fu">foo</span>', fixed = TRUE)
  expect_match(html, '<span class="at">a</span>', fixed = TRUE)
  expect_match(
    html,
    '<span class="st">&quot;&lt;b&gt;&quot;</span>',
    fixed = TRUE
  )
  expect_match(html, '<span class="dv">1</span>', fixed = TRUE)
  expect_match(html, '<span class="fu">interactive</span>', fixed = TRUE)
  expect_no_match(html, "<a", fixed = TRUE)

  # Usage that doesn't parse is shown as is.
  expect_identical(usage_html("foo(a <"), "foo(a &lt;")
})

test_that("usage_html() gives R's tokens the classes Pandoc gives them", {
  spans <- function(code) {
    html <- xml2::read_html(paste0("<div>", usage_html(code), "</div>"))
    nodes <- xml2::xml_find_all(html, ".//span")
    paste0(xml2::xml_attr(nodes, "class"), ":", xml2::xml_text(nodes))
  }
  expect_equal(
    spans("f(1.5, 1L, TRUE, NULL, NA)"),
    c(
      "fu:f",
      "fl:1.5",
      "dv:1L",
      "cn:TRUE",
      "cn:NULL",
      "cn:NA"
    )
  )
  expect_equal(spans("x <- y %in% z$a"), c("ot:<-", "sc:%in%", "sc:$"))
  expect_equal(
    spans("function(x, y = 2) x # c"),
    c(
      "cf:function",
      "at:y",
      "at:=",
      "dv:2",
      "co:# c"
    )
  )
  # Positions are right after a multibyte character, a tab and a new line.
  expect_equal(
    spans("x <- \"æ\"; \tf(a)\ng(b)"),
    c(
      "ot:<-",
      "st:\"æ\"",
      "fu:f",
      "fu:g"
    )
  )
})

test_that("usage_html() still highlights a clipped usage", {
  usage <- paste(
    c("f(", sprintf("  a%d = %d,", 1:20, 1:20), "  x = c(", "  1)", ")"),
    collapse = "\n"
  )
  lines <- strsplit(usage_html(usage), "\n", fixed = TRUE)[[1]]
  expect_length(lines, 12)
  expect_match(lines[[2]], '<span class="at">a1</span>', fixed = TRUE)
  expect_match(lines[[11]], "# ... 13 more lines", fixed = TRUE)
})

test_that("highlight_usages() fills the payload's signature", {
  index <- highlight_usages(list(
    foo = list(usage = "foo(1)", brief = NULL, topic = "foo", name = "foo"),
    bar = list(usage = NULL, brief = NULL, topic = "bar", name = "bar")
  ))
  payload <- tooltip_payload_html(index)
  expect_match(payload, '<span class="dv">1</span>', fixed = TRUE)
  expect_match(
    payload,
    '<code class="quartodown-tip-sig">bar</code>',
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

test_that("a long \\usage{} is clipped, keeping its last line", {
  usage <- paste(c("f(", sprintf("  a%d,", 1:20), "  b", ")"), collapse = "\n")
  expect_equal(
    strsplit(clip_usage(usage), "\n")[[1]],
    c(
      "f(",
      sprintf("  a%d,", 1:9),
      '  <span class="co"># ... 12 more lines</span>',
      ")"
    )
  )
  expect_identical(clip_usage("f(x)"), "f(x)")
})

# ---- postprocess_pages() -----------------------------------------------------

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
  expect_match(html, 'href="../reference/foo.html">foo</a>()', fixed = TRUE)
  expect_match(
    html,
    '<div class="quartodown-tip-payload" hidden>',
    fixed = TRUE
  )
})
