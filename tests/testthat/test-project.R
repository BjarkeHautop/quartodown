test_that("reset_project() empties _quarto/, keeping _freeze/ only when freezing", {
  dir <- new_pkg()
  project <- file.path(dir, "_quarto")
  dir.create(file.path(project, "_freeze"))
  dir.create(file.path(project, ".quarto"))
  writeLines("x", file.path(project, "stale.qmd"))

  reset_project(dir, freeze = TRUE)
  expect_identical(
    list.files(project, all.files = TRUE, no.. = TRUE),
    "_freeze"
  )

  reset_project(dir)
  expect_identical(
    list.files(project, all.files = TRUE, no.. = TRUE),
    character()
  )
})

test_that("import_markdown_files() imports NEWS, the license and community files", {
  dir <- new_pkg()
  writeLines("# examplepkg 1.0.0", file.path(dir, "NEWS.md"))
  writeLines("# MIT License", file.path(dir, "LICENSE.md"))
  writeLines("YEAR: 2026", file.path(dir, "LICENSE"))
  dir.create(file.path(dir, ".github"))
  writeLines("Be nice.", file.path(dir, ".github", "CODE_OF_CONDUCT.md"))
  # Links to .github/'s files, copied to the root.
  writeLines(
    "See the [code of conduct](.github/CODE_OF_CONDUCT.md).",
    file.path(dir, "CONTRIBUTING.md")
  )

  import_markdown_files(dir)

  project <- file.path(dir, "_quarto")
  expect_setequal(
    list.files(project),
    c(
      "NEWS.md",
      "LICENSE.md",
      "LICENSE-text.md",
      "CODE_OF_CONDUCT.md",
      "CONTRIBUTING.md"
    )
  )
  expect_identical(
    readLines(file.path(project, "CONTRIBUTING.md")),
    "See the [code of conduct](CODE_OF_CONDUCT.md)."
  )
  expect_identical(
    readLines(file.path(project, "CODE_OF_CONDUCT.md")),
    "Be nice."
  )
  expect_identical(
    readLines(file.path(project, "LICENSE-text.md")),
    c("---", "title: License", "---", "", "```", "YEAR: 2026", "```")
  )
})

test_that("news_page() titles the page and shifts `#` headings down", {
  note <- source_note("NEWS.md", "https://x.org/", class = "source-note")
  expect_identical(
    news_page(c("# pkg 1.0.0", "", "```", "# code", "```"), "https://x.org/"),
    c(
      "---",
      "title: Changelog",
      "shift-heading-level-by: 1",
      "---",
      "",
      note,
      "",
      "# pkg 1.0.0",
      "",
      "```",
      "# code",
      "```"
    )
  )
  expect_identical(
    news_page(c("## pkg 1.0.0", "```", "# code", "```"), "https://x.org/")[1:3],
    c("---", "title: Changelog", "---")
  )
  # Front matter of its own is kept.
  expect_identical(
    news_page(c("---", "title: News", "---", "# pkg 1.0.0"), "https://x.org/"),
    c("---", "title: News", "---", "", note, "", "# pkg 1.0.0")
  )
})

test_that("link_news() links issues and users, but not in code or existing links", {
  lines <- c(
    "* Fixed a bug (#12, @jane-doe).",
    "* See [#3](https://example.com) and https://x.org/#4.",
    "* Mail me at me@example.com.",
    "```",
    "#5 @code",
    "```"
  )
  expect_identical(
    link_news(
      lines,
      list(
        issue = "https://github.com/o/r/issues/",
        user = "https://github.com/"
      )
    ),
    c(
      "* Fixed a bug ([#12](https://github.com/o/r/issues/12), [@jane-doe](https://github.com/jane-doe)).",
      "* See [#3](https://example.com) and https://x.org/#4.",
      "* Mail me at me@example.com.",
      "```",
      "#5 @code",
      "```"
    )
  )
  expect_identical(
    link_news(lines, list(issue = NA_character_, user = NA_character_)),
    lines
  )

  # `~~~` blocks, and a ``` line inside a longer fence, are code too.
  code <- c("~~~", "#5", "~~~", "````", "```", "#6", "````", "#7")
  expect_identical(
    link_news(code, list(issue = "i/", user = NA_character_)),
    c(utils::head(code, -1), "[#7](i/7)")
  )
})

test_that("code_block_lines() finds fences, closed by one at least as long", {
  expect_identical(
    code_block_lines(c("a", "````r", "```", "`````", "~~~", "x", "~~~")),
    c("", "open", "code", "close", "open", "code", "close")
  )
})

test_that("split_front_matter() splits off the YAML front matter", {
  expect_identical(
    split_front_matter(c("---", "a: 1", "---", "text")),
    list(front_matter = c("---", "a: 1", "---"), body = "text")
  )
  expect_identical(
    split_front_matter(c("text", "---", "x", "---")),
    list(front_matter = character(), body = c("text", "---", "x", "---"))
  )
})

test_that("pkg_repo() reads the repository from DESCRIPTION's URL", {
  dir <- new_pkg()
  expect_true(all(is.na(unlist(pkg_repo(dir)))))

  desc::desc_set(
    URL = "https://example.com, https://github.com/o/r/",
    file = dir
  )
  expect_identical(
    pkg_repo(dir),
    list(
      home = "https://github.com/o/r",
      source = "https://github.com/o/r/blob/HEAD/",
      issue = "https://github.com/o/r/issues/",
      user = "https://github.com/"
    )
  )

  # A package in a folder of the repository, on another branch.
  desc::desc_set(URL = "https://github.com/o/r/tree/main/pkg", file = dir)
  repo <- pkg_repo(dir)
  expect_identical(repo$home, "https://github.com/o/r/tree/main/pkg")
  expect_identical(repo$source, "https://github.com/o/r/blob/main/pkg/")
  expect_identical(repo$issue, "https://github.com/o/r/issues/")

  desc::desc_set(URL = "https://gitlab.com/o/r", file = dir)
  repo <- pkg_repo(dir)
  expect_identical(repo$source, "https://gitlab.com/o/r/-/blob/HEAD/")
  expect_identical(repo$issue, "https://gitlab.com/o/r/-/issues/")
  expect_identical(repo$user, "https://gitlab.com/")

  desc::desc_set(
    URL = "https://gitlab.example.org/o/r/-/tree/main/pkg",
    file = dir
  )
  repo <- pkg_repo(dir)
  expect_identical(
    repo$source,
    "https://gitlab.example.org/o/r/-/blob/main/pkg/"
  )
  expect_identical(repo$issue, "https://gitlab.example.org/o/r/-/issues/")

  desc::desc_set(URL = "https://codeberg.org/o/r", file = dir)
  repo <- pkg_repo(dir)
  expect_identical(repo$source, "https://codeberg.org/o/r/src/branch/HEAD/")
  expect_identical(repo$issue, "https://codeberg.org/o/r/issues/")
  expect_identical(repo$user, "https://codeberg.org/")

  desc::desc_set(
    URL = "https://codeberg.org/o/r/src/branch/main/pkg",
    file = dir
  )
  repo <- pkg_repo(dir)
  expect_identical(repo$source, "https://codeberg.org/o/r/src/branch/main/pkg/")
  expect_identical(repo$issue, "https://codeberg.org/o/r/issues/")

  # Without one in URL, the repository BugReports is in.
  desc::desc_set(
    URL = "https://example.com",
    BugReports = "https://github.com/o/r/issues",
    file = dir
  )
  repo <- pkg_repo(dir)
  expect_identical(repo$home, "https://github.com/o/r")
  expect_identical(repo$source, "https://github.com/o/r/blob/HEAD/")
  desc::desc_set(BugReports = "https://gitlab.com/o/r/-/issues/", file = dir)
  expect_identical(pkg_repo(dir)$home, "https://gitlab.com/o/r")
  desc::desc_set(BugReports = "https://example.com/bugs", file = dir)
  expect_true(is.na(pkg_repo(dir)$home))
})

test_that("repo_host() recognizes GitHub, GitLab and Codeberg", {
  expect_identical(
    repo_host(c(
      "https://github.com/o/r",
      "https://www.github.com/o/r",
      "https://gitlab.com/o/r",
      "https://gitlab.example.org/o/r",
      "https://codeberg.org/o/r",
      "https://o.github.io/r/",
      "https://example.com",
      NA
    )),
    c("github", "github", "gitlab", "gitlab", "codeberg", NA, NA, NA)
  )
})

test_that("`repo:` overrides the repository's URLs and branch", {
  dir <- new_pkg()
  desc::desc_set(URL = "https://github.com/o/r", file = dir)
  repo <- pkg_repo(dir, list(repo = list(branch = "devel")))
  expect_identical(repo$source, "https://github.com/o/r/blob/devel/")

  config <- yaml::yaml.load(
    "
repo:
  url:
    home: https://code.example.com/r/
    source: https://code.example.com/r/src/
    issue: https://bugs.example.com/
"
  )
  repo <- pkg_repo(dir, config)
  expect_identical(repo$home, "https://code.example.com/r")
  expect_identical(repo$source, "https://code.example.com/r/src/")
  expect_identical(repo$issue, "https://bugs.example.com/")
  expect_identical(repo$user, "https://github.com/")
})

test_that("github_markdown_to_quarto() turns alerts into callouts and mermaid blocks into diagrams", {
  lines <- c(
    "Intro.",
    "> [!Warning]",
    "> Be careful.",
    ">",
    "> Really.",
    "After.",
    "> A plain quote.",
    "```mermaid",
    "graph TD; A-->B;",
    "```",
    "````md",
    "> [!NOTE]",
    "```mermaid",
    "````"
  )
  converted <- github_markdown_to_quarto(lines, mermaid = TRUE)
  expect_identical(
    converted,
    c(
      "Intro.",
      "",
      "::: {.callout-warning}",
      "Be careful.",
      "",
      "Really.",
      ":::",
      "",
      "After.",
      "> A plain quote.",
      "```{mermaid}",
      "graph TD; A-->B;",
      "```",
      "````md",
      "> [!NOTE]",
      "```mermaid",
      "````"
    )
  )
  # Only the mermaid block differs without `mermaid = TRUE`.
  expect_identical(
    github_markdown_to_quarto(lines),
    replace(converted, converted == "```{mermaid}", "```mermaid")
  )
})

test_that("github_markdown_to_quarto() makes executable blocks plain in a .md", {
  lines <- c(
    "  ```{r}",
    "  f(1)",
    "  ```",
    "~~~{python echo=false}",
    "~~~",
    "```{.r}",
    "```",
    "````md",
    "```{r}",
    "````"
  )
  expected <- replace(lines, 1:2 * 3 - 2, c("  ```r", "~~~python"))
  expect_identical(github_markdown_to_quarto(lines), expected)
  expect_identical(github_markdown_to_quarto(lines, mermaid = TRUE), expected)
  expect_identical(
    github_markdown_to_quarto(lines, mermaid = TRUE, executable = TRUE),
    lines
  )
})

test_that("escape_knitr_blocks() makes code blocks holding a chunk's fence HTML", {
  lines <- c(
    "```r",
    "f()",
    "```",
    "~~~",
    "  ```{python}",
    "",
    "~~~",
    "````",
    "```{r}"
  )
  expect_identical(
    escape_knitr_blocks(lines),
    c(
      lines[1:3],
      "",
      "<pre><code>  &#96;&#96;&#96;{python}",
      "</code></pre>",
      "",
      "",
      "<pre><code>&#96;&#96;&#96;{r}</code></pre>",
      ""
    )
  )
})

test_that("copy_vignettes() puts the source file under each vignette's title", {
  dir <- new_pkg(articles = "technical")
  write_vignette(dir, "examplepkg.Rmd")
  writeLines("x", file.path(dir, "vignettes", "data.csv"))
  desc::desc_set(
    URL = "https://github.com/someuser/examplepkg",
    file = dir,
    normalize = FALSE
  )

  copy_vignettes(dir)
  out <- file.path(dir, "_quarto", "vignettes")
  expect_identical(
    readLines(file.path(out, "articles", "technical.qmd")),
    c(
      "---",
      "title: \"technical\"",
      "---",
      "",
      "<p class=\"source-note\"><small class=\"dont-index\">Source: <a href=\"https://github.com/someuser/examplepkg/blob/HEAD/vignettes/articles/technical.qmd\"><code>vignettes/articles/technical.qmd</code></a></small></p>",
      "",
      "content"
    )
  )
  rmd <- readLines(file.path(out, "examplepkg.Rmd"))
  # knitr's `#| label, echo = FALSE` options aren't YAML.
  expect_identical(rmd[[3]], "validate-yaml: false")
  expect_match(rmd[[6]], "Source: .*<code>vignettes/examplepkg.Rmd</code>")
  expect_identical(readLines(file.path(out, "data.csv")), "x")
})

test_that("insert_after_front_matter() inserts after the front matter, or at the top", {
  expect_identical(
    insert_after_front_matter(c("", "---", "a: 1", "...", "text"), "x"),
    c("", "---", "a: 1", "...", "", "x", "text")
  )
  expect_identical(insert_after_front_matter("text", "x"), c("x", "text"))
  expect_identical(
    insert_after_front_matter(c("---", "unclosed"), "x"),
    c("x", "---", "unclosed")
  )
})

write_files <- function(dir, files) {
  for (f in files) {
    dir.create(
      dirname(file.path(dir, f)),
      recursive = TRUE,
      showWarnings = FALSE
    )
    writeLines("x", file.path(dir, f))
  }
}

test_that("copy_package_files() copies man/figures/, README_files/ and `vignettes: files:`", {
  dir <- new_pkg_with_man()
  write_rd(dir, "foo")
  write_files(
    dir,
    c(
      "man/figures/logo.svg",
      "man/rmd/shared.Rmd",
      "data-raw/y.csv",
      "data-raw/z.csv",
      "notes.txt",
      "README_files/figure-gfm/plot.png"
    )
  )
  config <- list(vignettes = list(files = list("man/rmd/", "data-raw/y.csv")))

  copy_package_files(dir, config)
  expect_setequal(
    list.files(file.path(dir, "_quarto"), recursive = TRUE),
    c(
      "man/figures/logo.svg",
      "man/rmd/shared.Rmd",
      "data-raw/y.csv",
      "README_files/figure-gfm/plot.png"
    )
  )
})

test_that("vignette_files() checks `vignettes: files:`", {
  dir <- new_pkg()
  write_files(dir, c("man/rmd/shared.Rmd", "vignettes/x.csv"))
  files <- function(...) list(vignettes = list(files = list(...)))

  expect_identical(vignette_files(dir), character())
  expect_identical(vignette_files(dir, files("man/rmd/")), "man/rmd")
  expect_error(vignette_files(dir, list(vignettes = "man")), "must be a list")
  expect_error(vignette_files(dir, files(1)), "list of paths")
  expect_error(vignette_files(dir, files("man/none")), "doesn't exist")
  expect_error(vignette_files(dir, files("../x")), "isn't a path in")
  expect_error(vignette_files(dir, files("/tmp")), "isn't a path in")
  expect_error(vignette_files(dir, files("vignettes/x.csv")), "isn't outside")
  expect_error(vignette_files(dir, files(".")), "isn't outside")
})

test_that("check_vignette_paths() warns about paths out of vignettes/ that aren't copied", {
  dir <- new_pkg()
  write_files(
    dir,
    c(
      "man/rmd/shared.Rmd",
      "man/figures/logo.png",
      "data-raw/y.csv",
      "vignettes/articles/data.csv"
    )
  )
  writeLines(
    c(
      "```{r, child = '../man/rmd/shared.Rmd'}",
      "```",
      "read.csv(\"../data-raw/y.csv\")",
      "read.csv(\"../data-raw/missing.csv\")",
      "![](../man/figures/logo.png)",
      "<img src=\"../man/figures/logo.png\">"
    ),
    file.path(dir, "vignettes", "intro.Rmd")
  )
  writeLines(
    "read.csv('../articles/data.csv')",
    file.path(dir, "vignettes", "articles", "a.qmd")
  )

  warning <- conditionMessage(expect_warning(check_vignette_paths(dir)))
  expect_match(warning, "intro.Rmd': '../man/rmd/shared.Rmd'", fixed = TRUE)
  expect_match(warning, "intro.Rmd': '../data-raw/y.csv'", fixed = TRUE)
  expect_match(warning, "add \"man/rmd/\" and \"data-raw/\" to", fixed = TRUE)
  expect_no_match(warning, "missing.csv|logo.png|data.csv")
  expect_no_warning(check_vignette_paths(
    dir,
    list(vignettes = list(files = list("man/rmd", "data-raw/")))
  ))
})

test_that("write_quarto_config() lists the pages only when `vignettes: files:` hold documents", {
  dir <- new_pkg()
  write_vignette(dir, "intro.qmd")
  write_files(dir, c("data-raw/y.csv", "man/rmd/shared.Rmd"))
  reset_project(dir)
  copy_vignettes(dir)
  writeLines("# x", file.path(dir, "_quarto", "index.qmd"))

  config <- list(vignettes = list(files = list("data-raw")))
  copy_package_files(dir, config)
  expect_null(write_quarto_config(dir, config)$project$render)

  config <- list(vignettes = list(files = list("data-raw", "man/rmd")))
  copy_package_files(dir, config)
  expect_setequal(
    unlist(write_quarto_config(dir, config)$project$render),
    c("index.qmd", "vignettes/intro.qmd")
  )

  config$quarto <- list(project = list(render = list("index.qmd")))
  expect_identical(
    write_quarto_config(dir, config)$project$render,
    list("index.qmd")
  )
})

test_that("write_pkgdown_yml() points downlit at the site's reference pages and vignettes", {
  dir <- new_pkg(articles = "technical")
  write_vignette(dir, "examplepkg.qmd")
  dir.create(file.path(dir, "docs"))
  write_pkgdown_yml(dir)
  expect_false(file.exists(file.path(dir, "docs", "pkgdown.yml")))

  desc::desc_set_urls(
    c("https://github.com/o/examplepkg", "https://o.github.io/examplepkg/"),
    file = dir
  )
  write_pkgdown_yml(dir)
  meta <- yaml::read_yaml(file.path(dir, "docs", "pkgdown.yml"))
  expect_identical(
    meta$urls,
    list(
      reference = "https://o.github.io/examplepkg/reference",
      article = "https://o.github.io/examplepkg/vignettes"
    )
  )
  expect_identical(
    meta$articles,
    list(examplepkg = "examplepkg.html", technical = "articles/technical.html")
  )
})

test_that("write_pkgdown_yml() lists no articles without vignettes", {
  dir <- new_pkg()
  dir.create(file.path(dir, "docs"))
  desc::desc_set_urls("https://o.github.io/examplepkg/", file = dir)
  write_pkgdown_yml(dir)
  meta <- yaml::read_yaml(file.path(dir, "docs", "pkgdown.yml"))
  expect_length(meta$articles, 0)
})

test_that("with_libpaths() puts the temporary library first while it runs", {
  lib <- withr::local_tempdir()
  old <- .libPaths()
  envvars <- c(R_LIBS = paste(c(lib, old), collapse = .Platform$path.sep))
  expect_identical(
    with_libpaths(envvars, .libPaths()[1]),
    normalizePath(lib, "/")
  )
  expect_identical(.libPaths(), old)
  # devel = TRUE has no library.
  expect_identical(with_libpaths(c(R_PROFILE_USER = "p"), .libPaths()), old)
})

test_that("preview_site() errors when the site hasn't been built", {
  expect_error(preview_site(new_pkg()), "build_site")
})

test_that("install_envvars() installs the package into a library put first on R_LIBS", {
  dir <- new_pkg()
  desc::desc_set(
    Title = "Example",
    Description = "An example.",
    License = "MIT",
    `Authors@R` = 'person("Jane", "Doe", email = "jane@example.com", role = c("aut", "cre"))',
    file = dir
  )
  dir.create(file.path(dir, "R"))
  writeLines("hello <- function() 'hi'", file.path(dir, "R", "hello.R"))
  writeLines("export(hello)", file.path(dir, "NAMESPACE"))
  scratch <- withr::local_tempdir()

  expect_message(envvars <- install_envvars(dir, scratch), "temporary library")
  lib <- strsplit(envvars[["R_LIBS"]], .Platform$path.sep, fixed = TRUE)[[1]][1]
  expect_true(file.exists(file.path(lib, "examplepkg", "DESCRIPTION")))
})

test_that("install_envvars() errors when the package doesn't install", {
  dir <- new_pkg()
  dir.create(file.path(dir, "R"))
  writeLines("this is not R code(", file.path(dir, "R", "broken.R"))
  expect_error(
    suppressMessages(install_envvars(dir, withr::local_tempdir())),
    "devel = TRUE"
  )
})

test_that("load_all_envvars() writes a profile that loads the package from source", {
  dir <- new_pkg()
  envvars <- load_all_envvars(dir, withr::local_tempdir())
  profile <- readLines(envvars[["R_PROFILE_USER"]])
  expect_match(profile, "pkgload::load_all(", fixed = TRUE, all = FALSE)
  expect_match(profile, deparse(normalizePath(dir)), fixed = TRUE, all = FALSE)
})

test_that("local_envvars() restores set and unset variables on exit", {
  withr::local_envvar(QUARTODOWN_A = "old", QUARTODOWN_B = NA)
  f <- function() {
    local_envvars(c(QUARTODOWN_A = "new", QUARTODOWN_B = "set"))
    Sys.getenv(c("QUARTODOWN_A", "QUARTODOWN_B"))
  }
  expect_identical(unname(f()), c("new", "set"))
  expect_identical(Sys.getenv("QUARTODOWN_A"), "old")
  expect_identical(Sys.getenv("QUARTODOWN_B", unset = NA), NA_character_)
})

test_that("dev_mode() is `development: mode:`, release by default", {
  withr::local_envvar(QUARTODOWN_DEV_MODE = NA)
  dir <- new_pkg()
  expect_identical(dev_mode(dir), "release")
  expect_identical(
    dev_mode(dir, list(development = list(mode = "devel"))),
    "devel"
  )
  expect_error(
    dev_mode(dir, list(development = list(mode = "dev"))),
    "development: mode:"
  )

  withr::local_envvar(QUARTODOWN_DEV_MODE = "unreleased")
  expect_identical(
    dev_mode(dir, list(development = list(mode = "devel"))),
    "unreleased"
  )
  withr::local_envvar(QUARTODOWN_DEV_MODE = "dev")
  expect_error(dev_mode(dir), "QUARTODOWN_DEV_MODE")
})

test_that("dev_mode() picks from the version with `mode: auto`", {
  withr::local_envvar(QUARTODOWN_DEV_MODE = NA)
  dir <- new_pkg()
  auto <- list(development = list(mode = "auto"))
  expect_identical(dev_mode(dir, auto), "release")
  desc::desc_set_version("1.0.0.9000", file = dir)
  expect_identical(dev_mode(dir, auto), "devel")
  desc::desc_set_version("1.0.9000", file = dir)
  expect_identical(dev_mode(dir, auto), "devel")
  desc::desc_set_version("0.0.0.9000", file = dir)
  expect_identical(dev_mode(dir, auto), "unreleased")
})

test_that("dev_mode() with `mode: auto` keeps a package not on CRAN in the root", {
  withr::local_envvar(QUARTODOWN_DEV_MODE = NA)
  dir <- new_pkg()
  auto <- list(development = list(mode = "auto"))
  desc::desc_set_version("0.1.0.9000", file = dir)
  local_mocked_bindings(on_cran = function(pkg) FALSE)
  expect_identical(dev_mode(dir, auto), "unreleased")
  local_mocked_bindings(on_cran = function(pkg) TRUE)
  expect_identical(dev_mode(dir, auto), "devel")
  desc::desc_set_version("0.1.0", file = dir)
  expect_identical(dev_mode(dir, auto), "release")
})

test_that("a development site goes in docs/dev/, under the site URL's dev/", {
  withr::local_envvar(QUARTODOWN_DEV_MODE = NA)
  dir <- new_pkg()
  desc::desc_set_urls("https://o.github.io/examplepkg/", file = dir)
  devel <- list(development = list(mode = "devel"))
  expect_identical(site_dir(dir), file.path(dir, "docs"))
  expect_identical(site_dir(dir, devel), file.path(dir, "docs", "dev"))

  settings <- quarto_config(dir, devel)
  expect_identical(settings$project$`output-dir`, "../docs/dev/")
  expect_identical(
    settings$website$`site-url`,
    "https://o.github.io/examplepkg/dev"
  )
  expect_identical(
    quarto_config(dir)$website$`site-url`,
    "https://o.github.io/examplepkg"
  )
})

test_that("clear_site() keeps the development site in a release build", {
  withr::local_envvar(QUARTODOWN_DEV_MODE = NA)
  dir <- new_pkg()
  docs <- file.path(dir, "docs")
  dir.create(file.path(docs, "dev"), recursive = TRUE)
  file.create(file.path(docs, c("index.html", "CNAME", "dev/index.html")))

  clear_site(dir, list(development = list(mode = "devel")))
  expect_setequal(
    list.files(docs, recursive = TRUE),
    c("index.html", "CNAME")
  )

  file.create(file.path(docs, "dev/index.html"))
  clear_site(dir)
  expect_setequal(
    list.files(docs, recursive = TRUE),
    c("CNAME", "dev/index.html")
  )
})

test_that("postprocess_pages() leaves the development site to its own build", {
  withr::local_envvar(QUARTODOWN_DEV_MODE = NA)
  dir <- new_pkg()
  docs <- file.path(dir, "docs")
  dir.create(file.path(docs, "dev"), recursive = TRUE)
  page <- "<html><body><p>Hi</p></body></html>"
  writeLines(page, file.path(docs, "index.html"))
  writeLines(page, file.path(docs, "dev", "index.html"))

  postprocess_pages(dir, tooltips = FALSE, topics = list())
  expect_identical(readLines(file.path(docs, "dev", "index.html")), page)
  expect_false(identical(readLines(file.path(docs, "index.html")), page))
})

test_that("postprocess_pages() puts the logo on pages not showing it already", {
  withr::local_envvar(QUARTODOWN_DEV_MODE = NA)
  dir <- new_pkg()
  dir.create(file.path(dir, "man", "figures"), recursive = TRUE)
  file.create(file.path(dir, "man", "figures", "logo.png"))
  docs <- file.path(dir, "docs")
  dir.create(file.path(docs, "reference"), recursive = TRUE)
  page <- function(title) {
    paste0(
      "<html><body><main id=\"quarto-document-content\"><h1>",
      title,
      "</h1></main></body></html>"
    )
  }
  writeLines(page("Hi"), file.path(docs, "404.html"))
  writeLines(page("Hi"), file.path(docs, "reference", "foo.html"))
  readme <- page("pkg <img src=\"man/figures/logo.png\" align=\"right\">")
  writeLines(readme, file.path(docs, "index.html"))

  postprocess_pages(dir, tooltips = FALSE, topics = list())
  expect_true(file.exists(file.path(docs, "man", "figures", "logo.png")))
  expect_true(any(grepl(
    "<img src=\"../man/figures/logo.png\" class=\"quartodown-logo\"",
    readLines(file.path(docs, "reference", "foo.html")),
    fixed = TRUE
  )))
  expect_false(any(grepl(
    "quartodown-logo",
    readLines(file.path(docs, "index.html"))
  )))
  expect_false(any(grepl(
    "quartodown-logo",
    readLines(file.path(docs, "404.html"))
  )))

  # A home page without the logo gets it.
  writeLines(page("pkg"), file.path(docs, "index.html"))
  postprocess_pages(dir, tooltips = FALSE, topics = list())
  expect_true(any(grepl(
    "<img src=\"./man/figures/logo.png\" class=\"quartodown-logo\"",
    readLines(file.path(docs, "index.html")),
    fixed = TRUE
  )))
})

test_that("render_inputs() lists the files Quarto renders", {
  dir <- withr::local_tempdir()
  for (f in c(
    "index.qmd",
    "NEWS.md",
    "reference/foo.qmd",
    "a/b.Rmd",
    "man/rmd/c.Rmd",
    "_quartodown/x.md",
    "_freeze/y.md",
    ".hidden.qmd",
    "_draft.qmd",
    "theme.scss"
  )) {
    dir.create(
      dirname(file.path(dir, f)),
      recursive = TRUE,
      showWarnings = FALSE
    )
    file.create(file.path(dir, f))
  }
  expect_setequal(
    render_inputs(dir),
    c("index.qmd", "NEWS.md", "reference/foo.qmd", "a/b.Rmd", "man/rmd/c.Rmd")
  )
  expect_setequal(
    render_inputs(dir, exclude = c("man", "a/b.Rmd")),
    c("index.qmd", "NEWS.md", "reference/foo.qmd")
  )
})

test_that("render_error_log() keeps Quarto's output from the failing file", {
  stderr <- paste(
    "\r[1/3] a.qmd\r",
    "\r[2/3] index.qmd\r",
    "\r\r",
    "processing file: index.qmd\r\r",
    "Error:\r\r",
    "! boom {x}\r\r",
    "Quitting from index.qmd:5-7 [unnamed-chunk-1]\r\r",
    "WARN: Error encountered when rendering files\r",
    sep = "\n"
  )
  expect_identical(
    unname(render_error_log(stderr)),
    c(
      "[2/3] index.qmd",
      "processing file: index.qmd",
      "Error:",
      "! boom {{x}}",
      "Quitting from index.qmd:5-7 [unnamed-chunk-1]",
      "WARN: Error encountered when rendering files"
    )
  )
  expect_identical(unname(render_error_log("ERROR: bad")), "ERROR: bad")
})

test_that("rendering_file() reads the page Quarto starts", {
  expect_identical(
    rendering_file("\r[ 2/31] reference/foo.qmd\r"),
    "reference/foo.qmd"
  )
  expect_identical(
    rendering_file("[2/2] vignettes\\a\\b.qmd"),
    "vignettes/a/b.qmd"
  )
  expect_identical(rendering_file("processing file: index.qmd"), NA_character_)
  expect_identical(rendering_file("1/3 [unnamed-chunk-1]"), NA_character_)
})

test_that("render_time_bullets() sums the reference and other pages", {
  times <- c(
    "index.qmd" = 1.25,
    "reference/index.qmd" = 0.5,
    "reference/a.qmd" = 1,
    "reference/b.qmd" = 2,
    "vignettes/fast.qmd" = 3,
    "vignettes/articles/slow.qmd" = 20,
    "NEWS.md" = 0.25,
    "authors.qmd" = 0.5
  )
  expect_identical(
    render_time_bullets(times),
    structure(
      c(
        "2 reference pages: 3.0s",
        "vignettes/articles/slow.qmd: 20.0s",
        "vignettes/fast.qmd: 3.0s",
        "Home page: 1.2s",
        "3 other pages: 1.2s"
      ),
      names = rep("*", 5)
    )
  )
  expect_identical(
    unname(render_time_bullets(times, max_vignettes = 1))[2:3],
    c("vignettes/articles/slow.qmd: 20.0s", "1 more vignette: 3.0s")
  )
  expect_identical(
    unname(render_time_bullets(c("vignettes/{x}.qmd" = 1))),
    "vignettes/{{x}}.qmd: 1.0s"
  )
  expect_identical(
    render_time_bullets(numeric()),
    structure(character(), names = character())
  )
})
