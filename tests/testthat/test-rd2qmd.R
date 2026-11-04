parse_rd_text <- function(lines, macros = NULL) {
  file <- withr::local_tempfile(fileext = ".Rd")
  writeLines(lines, file)
  tools::parse_Rd(file, macros = macros)
}

rd_qmd <- function(lines, pkg = "examplepkg") {
  rd_to_qmd(parse_rd_text(lines), pkg)
}

# The lines of `## <heading>`'s section, without blank lines.
qmd_section <- function(qmd, heading) {
  start <- which(qmd == paste("##", heading))
  heads <- grep("^## ", qmd)
  end <- c(heads[heads > start] - 1L, length(qmd))[1]
  out <- qmd[seq(start + 1L, end)]
  out[nzchar(trimws(out))]
}

test_that("rd_to_qmd() makes the title the page title, keeping its code", {
  qmd <- rd_qmd(c(
    "\\name{foo}",
    "\\alias{foo}",
    "\\title{Use \\code{foo}}",
    "\\description{d}"
  ))
  expect_identical(
    qmd[1:3],
    c("---", "title: Use `foo`", "---")
  )
  expect_true("## Description" %in% qmd)
})

test_that("rd_to_qmd() makes Usage a fenced R block", {
  qmd <- rd_qmd(c(
    "\\name{foo}",
    "\\alias{foo}",
    "\\title{Foo}",
    "\\usage{",
    "foo(x = \"a\", y = x$z)",
    "",
    "x \\%op\\% y",
    "}"
  ))
  expect_identical(
    qmd_section(qmd, "Usage"),
    c("```r", "foo(x = \"a\", y = x$z)", "x %op% y", "```")
  )
})

test_that("rd_html_to_markdown() turns inline code into code spans", {
  expect_identical(
    rd_html_to_markdown(
      "in <code>tools::R_user_dir(&quot;pkg&quot;)</code> or <code style=\"white-space: pre;\">\\</code>."
    ),
    "in `tools::R_user_dir(\"pkg\")` or `\\`."
  )
  expect_identical(
    rd_html_to_markdown("<code>a</code> and <code>x &lt;- `y`</code>"),
    "`a` and `` x <- `y` ``"
  )
  # Linked inline code becomes a Markdown link around a code span.
  expect_identical(
    rd_html_to_markdown(
      "<code><a href=\"foo.html\">f(&quot;a&quot;)</a></code>"
    ),
    "[`f(\"a\")`](foo.html)"
  )
  # A <code> with other markup inside stays HTML.
  marked <- "<code><b>foo()</b></code>"
  expect_identical(rd_html_to_markdown(marked), marked)
})

test_that("rd_html_to_markdown() keeps code knitr would run from it", {
  expect_identical(
    rd_html_to_markdown(
      "Write <code>`r x$y`</code>, <code>r x</code> or <code>r</code>."
    ),
    "Write <code>&#96;r x&#36;y&#96;</code>, <code>r x</code> or `r`."
  )
  expect_identical(
    rd_html_to_markdown(
      "<div class=\"sourceCode markdown\"><pre>```{r}\n1\n```\n</pre></div>"
    ),
    c(
      "",
      "<pre class=\"sourceCode markdown\"><code>&#96;&#96;&#96;{r}",
      "1",
      "&#96;&#96;&#96;</code></pre>",
      ""
    )
  )
  expect_identical(
    rd_html_to_markdown("<pre>```{r}\n</pre>"),
    c("<pre>&#96;&#96;&#96;{r}", "</pre>")
  )
})

test_that("rd_html_to_markdown() turns R6 method usage into a fenced block", {
  expect_identical(
    rd_html_to_markdown(c(
      "<div class=\"r\">",
      "<pre>Account$new(owner = &quot;x&quot;)</pre>",
      "</div>"
    )),
    c("", "```r", "Account$new(owner = \"x\")", "```", "")
  )
})

test_that("rd_html_to_markdown() escapes $ outside code and math only", {
  expect_identical(
    rd_html_to_markdown(
      "<p>Costs $5, <code>x$y</code>, <code class=\"reqn\">x &lt; y</code>.</p>"
    ),
    "Costs \\$5, `x$y`, $x < y$."
  )
  expect_identical(
    rd_html_to_markdown(c(
      "<p style=\"text-align: center;\"><code class=\"reqn\">",
      "\\sum x",
      "</code>",
      "</p>"
    )),
    c("", "", "$$", "\\sum x", "$$", "", "")
  )
})

test_that("rd_html_to_markdown() leaves <pre> alone but fences language-tagged blocks", {
  pre <- "<pre>CausalDiscoSearch$new(&quot;a&quot;)</pre>"
  expect_identical(rd_html_to_markdown(pre), pre)
  expect_identical(
    rd_html_to_markdown(
      "<div class=\"sourceCode R\"><pre>x &lt;- 1\n</pre></div>"
    ),
    c("", "```r", "x <- 1", "```", "")
  )
})

test_that("rd_html_to_markdown() points figures at man/figures/", {
  expect_identical(
    rd_html_to_markdown("<img src=\"../help/figures/logo.png\">"),
    "<img src=\"../man/figures/logo.png\">"
  )
})

test_that("rd_to_qmd() renders the package's Rd macros and build-stage \\Sexpr{}s", {
  dir <- new_pkg()
  dir.create(file.path(dir, "man", "macros"), recursive = TRUE)
  writeLines(
    "\\newcommand{\\shout}{\\Sexpr[stage=build,results=rd]{toupper(\"#1\")}}",
    file.path(dir, "man", "macros", "macros.Rd")
  )
  rd <- parse_rd_text(
    c(
      "\\name{foo}",
      "\\alias{foo}",
      "\\title{Foo}",
      "\\description{See \\doi{10.1000/xyz}.}",
      "\\references{\\shout{hello}}"
    ),
    macros = rd_macros(dir)
  )
  qmd <- rd_to_qmd(rd, "examplepkg")
  expect_identical(qmd_section(qmd, "References"), "HELLO")
  expect_match(
    paste(qmd_section(qmd, "Description"), collapse = " "),
    "<a href=\"https://doi.org/10.1000/xyz\">doi:10.1000/xyz</a>",
    fixed = TRUE
  )
})

test_that("rd_to_qmd() links \\link{}s to the topic's page", {
  rd <- parse_rd_text(c(
    "\\name{foo}",
    "\\alias{foo}",
    "\\title{Foo}",
    "\\description{",
    "\\code{\\link{bar}}, \\link[=bar]{the bar}, \\code{\\link[stats:median]{median()}},",
    "\\link{not_a_topic}.",
    "}"
  ))
  qmd <- rd_to_qmd(rd, "examplepkg", pages = c(bar = "bar-page"))
  expect_identical(
    qmd_section(qmd, "Description"),
    c(
      "[`bar`](bar-page.html), <a href=\"bar-page.html\">the bar</a>, [`median()`](https://rdrr.io/r/stats/median.html),",
      "not_a_topic."
    )
  )
})

test_that("link_help_hrefs() points R6 inherited method links at the topic's page", {
  html <- "<a href='../../examplepkg/html/Parent.html#method-Parent-greet'>x</a>"
  expect_identical(
    link_help_hrefs(html, "examplepkg", c(Parent = "Parent")),
    "<a href=\"Parent.html#method-Parent-greet\">x</a>"
  )
  # An unknown topic is left as is.
  expect_identical(link_help_hrefs(html, "examplepkg", character()), html)
})

test_that("rd_to_qmd() names the Rd file whose \\Sexpr{}s fail", {
  rd <- parse_rd_text(c(
    "\\name{foo}",
    "\\alias{foo}",
    "\\title{Foo}",
    "\\description{\\Sexpr[stage=build]{stop(\"no\")}}"
  ))
  expect_warning(rd_to_qmd(rd, "examplepkg"), "man/file.*\\.Rd")
})

test_that("rd_to_qmd() runs a section's \\Sexpr{} once, not again for the examples", {
  log <- withr::local_tempfile()
  # Windows' backslashes would be escapes in the Rd and the R string.
  log <- gsub("\\", "/", log, fixed = TRUE)
  qmd <- rd_qmd(c(
    "\\name{foo}",
    "\\alias{foo}",
    "\\title{Foo}",
    sprintf(
      "\\description{\\Sexpr[stage=build]{cat('x\\\\n', file = '%s', append = TRUE)}}",
      log
    ),
    "\\examples{foo()}"
  ))
  expect_length(readLines(log), 1)
  expect_true("foo()" %in% qmd)
})

test_that("rd_to_qmd() keeps a custom Examples section as prose", {
  qmd <- rd_qmd(c(
    "\\name{foo}",
    "\\alias{foo}",
    "\\title{Foo}",
    "\\section{Examples}{Here we show \\code{foo()}.}",
    "\\examples{foo()}"
  ))
  examples <- which(qmd == "## Examples")
  expect_length(examples, 2)
  expect_true("Here we show `foo()`." %in% qmd)
  expect_identical(
    qmd[examples[2] + 2],
    "```{r, include=FALSE}"
  )
})

example_rd <- function(...) {
  parse_rd_text(c(
    "\\name{foo}",
    "\\alias{foo}",
    "\\title{Foo}",
    "\\examples{",
    ...,
    "}"
  ))
}

test_that("example_blocks() runs examples after attaching the package", {
  expect_identical(
    example_blocks(example_rd("x <- 1", "x"), "my.pkg"),
    c(
      "```{r, include=FALSE}",
      "library(my.pkg)",
      "```",
      "",
      example_chunk_header,
      "x <- 1",
      "x",
      "```",
      ""
    )
  )
})

test_that("example_blocks() shows \\dontrun{} code unrun and runs the rest", {
  blocks <- example_blocks(
    example_rd(
      "# setup",
      "\\dontrun{",
      "## a comment",
      "online()",
      "}",
      "\\donttest{slow()}",
      "\\dontshow{secret()}",
      "y"
    ),
    "examplepkg"
  )
  blocks <- blocks[nzchar(blocks)]
  expect_identical(
    blocks,
    c(
      "```{r, include=FALSE}",
      "library(examplepkg)",
      "```",
      example_chunk_header,
      "# setup",
      "```",
      "```r",
      "## a comment",
      "online()",
      "```",
      example_chunk_header,
      "slow()",
      "```",
      "```{r, include=FALSE}",
      "secret()",
      "```",
      example_chunk_header,
      "y",
      "```"
    )
  )
})

test_that("example_blocks() renders examples that are all \\dontrun{} without R", {
  blocks <- example_blocks(
    example_rd("\\dontrun{a()}", "", "\\dontrun{b()}"),
    "examplepkg"
  )
  expect_identical(blocks, c("```r", "a()", "b()", "```", ""))
})

test_that("example_blocks() runs @examplesIf examples only if the condition holds", {
  examples_if <- function(cond) {
    example_rd(
      sprintf(
        "\\dontshow{if (%s) withAutoprint(\\{ # examplesIf}",
        cond
      ),
      "f()",
      "\\dontshow{\\}) # examplesIf}",
      "g()"
    )
  }

  blocks <- example_blocks(examples_if("TRUE"), "examplepkg")
  expect_false(any(grepl("examplesIf", blocks)))
  expect_identical(
    blocks[nzchar(blocks)][-(1:3)],
    c(example_chunk_header, "f()", "g()", "```")
  )

  blocks <- example_blocks(examples_if("1 == 2"), "examplepkg")
  expect_false(any(grepl("examplesIf", blocks)))
  expect_identical(
    blocks[nzchar(blocks)][-(1:3)],
    c("```r", "f()", "```", example_chunk_header, "g()", "```")
  )
})

test_that("write_man_pages() writes a page per Rd file, except unlisted internal ones", {
  dir <- new_pkg_with_man()
  write_rd(dir, "foo")
  write_rd(dir, "hidden", keyword = "internal")
  write_rd(dir, "reexports", keyword = "internal")
  reference_dir <- file.path(dir, "_quarto", "reference")

  write_man_pages(dir)
  expect_identical(list.files(reference_dir), "foo.qmd")

  unlink(reference_dir, recursive = TRUE)
  config <- reference_config(c(
    "reference:",
    "  - title: All",
    "    contents:",
    "      - foo",
    "      - reexports"
  ))
  write_man_pages(dir, config)
  expect_setequal(list.files(reference_dir), c("foo.qmd", "reexports.qmd"))

  # `internal-pages: true` gives every topic a page.
  unlink(reference_dir, recursive = TRUE)
  write_man_pages(dir, list(`internal-pages` = TRUE))
  expect_setequal(
    list.files(reference_dir),
    c("foo.qmd", "hidden.qmd", "reexports.qmd")
  )
})

test_that("a \\link{} to an internal topic without a page is plain text and warns", {
  dir <- new_pkg_with_man()
  writeLines(
    c(
      "\\name{foo}",
      "\\alias{foo}",
      "\\title{Foo}",
      "\\description{",
      "See \\link{helper}, \\link[=helper_alias]{its alias},",
      "\\link[examplepkg:helper]{itself} and \\link[stats]{median}.",
      "}"
    ),
    file.path(dir, "man", "foo.Rd")
  )
  write_rd(dir, "helper", alias = "helper_alias", keyword = "internal")
  # Links from a page that isn't built don't count.
  writeLines(
    c(
      "\\name{other}",
      "\\alias{other}",
      "\\title{Other}",
      "\\description{\\link{helper}}",
      "\\keyword{internal}"
    ),
    file.path(dir, "man", "other.Rd")
  )
  writeLines("Package: examplepkg", file.path(dir, "DESCRIPTION"))
  topics <- rd_topics(dir)

  expect_snapshot(check_internal_links(list(), topics, "examplepkg"))

  write_man_pages(dir, topics = topics)
  page <- read_utf8(file.path(dir, "_quarto", "reference", "foo.qmd"))
  expect_false(any(grepl("helper.html", page, fixed = TRUE)))

  # `internal-pages: true` gives it a page, which the links go to.
  config <- list(`internal-pages` = TRUE)
  expect_no_warning(check_internal_links(config, topics, "examplepkg"))
  write_man_pages(dir, config, topics)
  page <- read_utf8(file.path(dir, "_quarto", "reference", "foo.qmd"))
  expect_equal(
    sum(lengths(regmatches(page, gregexpr("helper.html", page)))),
    3L
  )

  # As does listing it in the reference index.
  config <- reference_config(c(
    "reference:",
    "  - title: All",
    "    contents: [foo, helper]"
  ))
  expect_no_warning(check_internal_links(config, topics, "examplepkg"))
})

test_that("navbar links to reference pages that won't exist warn", {
  dir <- new_pkg_with_man()
  write_rd(dir, "foo")
  write_rd(dir, "topic-guide", keyword = "internal")
  topics <- rd_topics(dir)
  config <- yaml::yaml.load(paste(
    c(
      "navbar:",
      "  left:",
      "    - reference",
      "    - text: Guides",
      "      menu:",
      "        - text: Guide",
      "          href: reference/topic-guide.qmd",
      "        - text: Foo",
      "          href: ./reference/foo.html",
      "        - text: Typo",
      "          href: reference/fooo.qmd"
    ),
    collapse = "\n"
  ))

  expect_snapshot(check_internal_links(config, topics, "examplepkg"))

  config$`internal-pages` <- TRUE
  expect_warning(
    check_internal_links(config, topics, "examplepkg"),
    "fooo"
  )
})

test_that("rd_source_files() reads roxygen's header, else names the Rd file", {
  file <- withr::local_tempfile(fileext = ".Rd")
  writeLines(
    c(
      "% Generated by roxygen2: do not edit by hand",
      "% Please edit documentation in R/aaaaaaaaaaaaaaaaaaaa.R, R/bbbbbbbbbbbbbbbbbbbb.R,",
      "%   R/cccccccccccccccccccc.R",
      "\\name{foo}"
    ),
    file
  )
  expect_identical(
    rd_source_files(file),
    c(
      "R/aaaaaaaaaaaaaaaaaaaa.R",
      "R/bbbbbbbbbbbbbbbbbbbb.R",
      "R/cccccccccccccccccccc.R"
    )
  )

  writeLines("\\name{foo}", file)
  expect_identical(rd_source_files(file), file.path("man", basename(file)))
})

test_that("write_man_pages() puts the source files under the title", {
  dir <- new_pkg_with_man()
  write_rd(dir, "foo")
  rd <- file.path(dir, "man", "foo.Rd")
  writeLines(
    c("% Please edit documentation in R/foo.R", readLines(rd)),
    rd
  )
  qmd <- file.path(dir, "_quarto", "reference", "foo.qmd")

  write_man_pages(dir)
  expect_true(
    "<p><small class=\"dont-index\">Source: <code>R/foo.R</code></small></p>" %in%
      readLines(qmd)
  )

  desc::desc_set(
    URL = "https://github.com/someuser/examplepkg",
    file = dir,
    normalize = FALSE
  )
  write_man_pages(dir)
  expect_true(
    "<p><small class=\"dont-index\">Source: <a href=\"https://github.com/someuser/examplepkg/blob/HEAD/R/foo.R\"><code>R/foo.R</code></a></small></p>" %in%
      readLines(qmd)
  )
})

test_that("write_man_pages() and build_topic_index() handle no or an empty man/", {
  dir <- new_pkg()
  write_man_pages(dir)
  expect_length(build_topic_index(dir), 0)

  dir.create(file.path(dir, "man"))
  write_man_pages(dir)
  expect_length(build_topic_index(dir), 0)
  expect_length(list.files(file.path(dir, "_quarto", "reference")), 0)
})

test_that("write_man_pages() names the Rd file it failed to convert", {
  dir <- new_pkg_with_man()
  write_rd(dir, "foo")
  local_mocked_bindings(rd_to_qmd = function(...) stop("boom"))
  expect_error(write_man_pages(dir), "foo.Rd")
})
