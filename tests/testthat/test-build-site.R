test_that("vignette_title() reads the YAML title, falling back to the filename", {
  dir <- withr::local_tempdir()
  titled <- file.path(dir, "titled.qmd")
  writeLines(c("---", "title: \"Getting Started\"", "---", "content"), titled)
  expect_identical(vignette_title(titled), "Getting Started")

  untitled <- file.path(dir, "untitled.qmd")
  writeLines(c("---", "format: html", "---", "content"), untitled)
  expect_identical(vignette_title(untitled), "untitled")

  no_frontmatter <- file.path(dir, "plain.qmd")
  writeLines("just content", no_frontmatter)
  expect_identical(vignette_title(no_frontmatter), "plain")

  quoted <- file.path(dir, "quoted.qmd")
  writeLines(c("---", "title: 'It''s here'", "---"), quoted)
  expect_identical(vignette_title(quoted), "It's here")

  long <- file.path(dir, "long.qmd")
  writeLines(c("---", paste0("a", 1:50, ": 1"), "title: Late", "---"), long)
  expect_identical(vignette_title(long), "Late")

  # A horizontal rule pair in the text isn't front matter.
  ruled <- file.path(dir, "ruled.qmd")
  writeLines(c("text", "---", "title: No", "---"), ruled)
  expect_identical(vignette_title(ruled), "ruled")
})

test_that("author_roles_text() keeps role codes it doesn't know", {
  expect_identical(
    author_roles_text(list(role = c("aut", "rth"))),
    "Author, rth"
  )
})

test_that("find_vignettes() lists top-level vignettes, then subfolders", {
  dir <- new_pkg(articles = c("b-article", "a-article"))
  write_vignette(dir, "z.qmd")
  write_vignette(dir, "examplepkg.Rmd")
  write_vignette(dir, "_child.Rmd")
  dir.create(file.path(dir, "vignettes", "guide"))
  write_vignette(dir, "guide/intro.qmd")

  expect_identical(
    find_vignettes(dir),
    c(
      "examplepkg.Rmd",
      "z.qmd",
      "articles/a-article.qmd",
      "articles/b-article.qmd",
      "guide/intro.qmd"
    )
  )

  dir_none <- withr::local_tempdir()
  expect_identical(find_vignettes(dir_none), character())
})

test_that("find_getting_started_vignette() picks the single top-level vignette", {
  dir <- new_pkg(articles = "technical")
  write_vignette(dir, "getting-started.qmd")

  expect_identical(find_getting_started_vignette(dir), "getting-started.qmd")
})

test_that("find_getting_started_vignette() disambiguates by package name", {
  dir <- new_pkg()
  write_vignette(dir, "examplepkg.qmd")
  write_vignette(dir, "other.qmd")

  expect_identical(find_getting_started_vignette(dir), "examplepkg.qmd")
})

test_that("find_getting_started_vignette() is NULL when ambiguous or missing", {
  dir <- new_pkg()
  write_vignette(dir, "a.qmd")
  write_vignette(dir, "b.qmd")
  expect_message(expect_null(find_getting_started_vignette(dir)), "several")

  expect_null(find_getting_started_vignette(new_pkg(articles = "technical")))
})

test_that("navbar_items() links Get started, Reference, Articles and Changelog", {
  dir <- new_pkg(articles = "technical")
  write_vignette(dir, "examplepkg.qmd", title = "Get started with examplepkg")
  write_vignette(dir, "usage.qmd", title = "Using examplepkg")
  writeLines("# examplepkg 1.0.0", file.path(dir, "NEWS.md"))

  left <- navbar_items(dir)$left
  expect_identical(
    nav_texts(left),
    c("Get started", "Reference", "Articles", "Changelog")
  )
  expect_identical(
    nav_files(left),
    c(
      "vignettes/examplepkg.qmd",
      "reference/index.qmd",
      "vignettes/usage.qmd",
      "vignettes/articles/technical.qmd",
      "NEWS.md"
    )
  )
  expect_identical(left[[3]]$menu[[1]]$text, "Using examplepkg")
})

test_that("navbar_items() leaves out Articles and Changelog when there are none", {
  dir <- new_pkg()
  write_vignette(dir, "examplepkg.qmd")

  items <- navbar_items(dir)
  expect_identical(nav_texts(items$left), c("Get started", "Reference"))
  expect_length(items$right, 0)
  expect_length(items$tools, 0)
})

test_that("navbar_items() picks the repository icon from its host", {
  dir <- new_pkg()
  repo_icon <- function(url, config = list()) {
    desc::desc_set(URL = url, file = dir)
    navbar_items(dir, config)$tools[[1]][c("icon", "aria-label", "text")]
  }
  expect_identical(
    repo_icon("https://gitlab.example.org/o/r"),
    list(icon = "gitlab", `aria-label` = "GitLab", text = "GitLab")
  )
  expect_identical(
    repo_icon("https://codeberg.org/o/r"),
    list(icon = "git", `aria-label` = "Codeberg", text = "Codeberg")
  )
  expect_identical(
    repo_icon(
      "https://example.com",
      list(repo = list(url = list(home = "https://git.sr.ht/~o/r")))
    ),
    list(icon = "git", `aria-label` = "Source code", text = "Source code")
  )
})

test_that("`navbar:` puts the repository among the tools or right items", {
  dir <- new_pkg()
  desc::desc_set(URL = "https://github.com/o/r", file = dir)

  items <- navbar_items(dir)
  expect_length(items$right, 0)
  expect_identical(items$tools[[1]]$href, "https://github.com/o/r")

  config <- list(navbar = list(right = list("repo"), tools = list()))
  items <- navbar_items(dir, config)
  expect_length(items$tools, 0)
  # On the navbar, `text` would show beside the icon.
  expect_identical(
    items$right[[1]],
    list(
      icon = "github",
      href = "https://github.com/o/r",
      `aria-label` = "GitHub"
    )
  )

  mastodon <- list(icon = "mastodon", href = "https://fosstodon.org/@me")
  config <- list(navbar = list(tools = list(mastodon, "repo")))
  items <- navbar_items(dir, config)
  expect_identical(items$tools[[1]], mastodon)
  expect_identical(items$tools[[2]]$icon, "github")

  settings <- quarto_config(dir, config)
  expect_length(settings$website$navbar$tools, 2)
  expect_null(settings$website$navbar$right)
})

test_that("`navbar: tools` takes only icons", {
  dir <- new_pkg()
  tools <- list("reference", list(text = "Blog", href = "https://example.com"))
  for (tool in tools) {
    expect_error(
      navbar_items(dir, list(navbar = list(tools = list(tool)))),
      "isn't an icon"
    )
  }
})

test_that("`navbar:` lays out built-in and custom items", {
  dir <- new_pkg(articles = "technical")
  write_vignette(dir, "examplepkg.qmd")
  writeLines("# examplepkg 1.0.0", file.path(dir, "NEWS.md"))
  for (folder in c("get-started", "guide")) {
    dir.create(file.path(dir, "vignettes", folder))
  }
  write_vignette(dir, "get-started/install.qmd", title = "Installation")
  write_vignette(dir, "get-started/basics.qmd", title = "Basics")
  write_vignette(dir, "guide/b.qmd")
  write_vignette(dir, "guide/a.qmd")

  config <- yaml::yaml.load(
    "
navbar:
  left:
    - reference
    - news
    - text: Get started
      menu: [get-started/install, get-started/basics]
    - text: User guide
      menu:
        - guide/
        - text: Website
          href: https://example.com
    - articles
  right:
    - text: Blog
      href: https://example.com/blog
"
  )
  items <- navbar_items(dir, config)

  expect_identical(
    nav_texts(items$left),
    c("Reference", "Changelog", "Get started", "User guide", "Articles")
  )
  expect_identical(
    nav_files(items$left),
    c(
      "reference/index.qmd",
      "NEWS.md",
      "vignettes/get-started/install.qmd",
      "vignettes/get-started/basics.qmd",
      "vignettes/guide/a.qmd",
      "vignettes/guide/b.qmd",
      "vignettes/examplepkg.qmd",
      "vignettes/articles/technical.qmd"
    )
  )
  expect_identical(
    items$left[[3]]$menu[[1]],
    list(text = "Installation", file = "vignettes/get-started/install.qmd")
  )
  expect_identical(
    items$left[[4]]$menu[[3]],
    list(text = "Website", href = "https://example.com")
  )
  expect_identical(items$right, config$navbar$right)
})

test_that("`navbar:` links a vignette given as a string", {
  dir <- new_pkg(articles = c("a", "b"))
  left <- navbar_items(
    dir,
    list(navbar = list(left = c("articles/b", "articles")))
  )$left
  expect_identical(
    nav_files(left),
    c("vignettes/articles/b.qmd", "vignettes/articles/a.qmd")
  )
})

test_that("`navbar:` warns about vignettes it leaves out, and rejects unknowns", {
  dir <- new_pkg(articles = c("a", "b"))
  extra <- list(text = "Extra", menu = "articles/a")
  config <- list(navbar = list(left = list("reference", extra)))
  expect_warning(navbar_items(dir, config), "articles/b")

  config$navbar$left <- list("nope")
  expect_error(navbar_items(dir, config), "nope")

  extra$menu <- "articles/c"
  config$navbar$left <- list(extra)
  expect_error(navbar_items(dir, config), "articles/c")

  extra$menu <- "guide/"
  config$navbar$left <- list(extra)
  expect_error(navbar_items(dir, config), "guide/")

  expect_error(navbar_items(dir, list(navbar = list(top = "news"))), "top")
})

test_that("`navbar: style: sidebar` moves the left items into a docked sidebar", {
  dir <- new_pkg(articles = "a")
  dir.create(file.path(dir, "man"))
  write_rd(dir, "foo", usage = "foo(x)")
  write_rd(dir, "bar", usage = "bar(x)")
  writeLines("# News", file.path(dir, "NEWS.md"))

  settings <- quarto_config(dir, list(navbar = list(style = "sidebar")))
  expect_null(settings$website$navbar$left)
  expect_true(settings$website$navbar$search)
  expect_identical(settings$website$sidebar$style, "docked")

  contents <- settings$website$sidebar$contents
  expect_identical(
    contents[[1]],
    list(
      section = "Reference",
      href = "reference/index.qmd",
      # One group, so no section per group.
      contents = list(
        list(text = "bar()", href = "reference/bar.qmd"),
        list(text = "foo()", href = "reference/foo.qmd")
      )
    )
  )
  expect_identical(contents[[2]]$section, "Articles")
  expect_identical(contents[[2]]$contents[[1]]$href, "vignettes/articles/a.qmd")
  expect_identical(contents[[3]], list(text = "Changelog", href = "NEWS.md"))

  expect_null(quarto_config(dir)$website$sidebar)
})

test_that("the sidebar's Reference section has a section per titled group", {
  dir <- new_pkg()
  dir.create(file.path(dir, "man"))
  write_rd(dir, "foo", usage = "foo(x)")
  write_rd(dir, "bar", usage = "bar(x)")
  config <- list(
    navbar = list(style = "sidebar", left = list("reference")),
    reference = list(
      list(title = "Foo", contents = list("foo")),
      list(subtitle = "Bar", contents = list("bar"))
    )
  )
  reference <- quarto_config(dir, config)$website$sidebar$contents[[1]]
  expect_identical(
    reference$contents,
    list(
      list(
        section = "Foo",
        contents = list(list(text = "foo()", href = "reference/foo.qmd"))
      ),
      list(
        section = "Bar",
        contents = list(list(text = "bar()", href = "reference/bar.qmd"))
      )
    )
  )
})

test_that("the sidebar's Reference entry is a plain link without topics", {
  dir <- new_pkg()
  config <- list(navbar = list(style = "sidebar", left = list("reference")))
  expect_identical(
    quarto_config(dir, config)$website$sidebar$contents,
    list(list(text = "Reference", href = "reference/index.qmd"))
  )
})

test_that("navbar_style() accepts only `top` and `sidebar`", {
  expect_identical(navbar_style(), "top")
  expect_identical(
    navbar_style(list(navbar = list(style = "sidebar"))),
    "sidebar"
  )
  expect_error(
    navbar_style(list(navbar = list(style = "left"))),
    "top.*sidebar"
  )
})

test_that("topic_label() turns a usage name into plain text", {
  expect_identical(topic_label(list(names = "foo()")), "foo()")
  expect_identical(topic_label(list(names = "&#96;%&gt;%&#96;")), "`%>%`")
  expect_identical(
    topic_label(list(names = "print(<i>&lt;foo&gt;</i>)")),
    "print(<foo>)"
  )
})

test_that("write_quarto_config() writes a complete _quarto/_quarto.yml", {
  dir <- new_pkg()
  write_vignette(dir, "examplepkg.qmd")

  write_quarto_config(dir)
  out_path <- file.path(dir, "_quarto", "_quarto.yml")
  settings <- yaml::read_yaml(out_path)
  expect_identical(settings$website$title, "examplepkg 1.0.0")
  expect_identical(settings$project$`output-dir`, "../docs/")
  expect_null(settings$format$html$`code-link`)
  expect_identical(
    settings$knitr$opts_chunk,
    list(collapse = TRUE, comment = "#>")
  )
  expect_null(settings$website$navbar$right)
  expect_match(settings$website$`page-footer`$left, "Jane Doe")
})

test_that("quarto_config() turns on Quarto's freeze when asked", {
  dir <- new_pkg()
  expect_null(quarto_config(dir)$execute)
  expect_identical(quarto_config(dir, freeze = TRUE)$execute$freeze, "auto")
})

test_that("quarto_config() sets the site URL from DESCRIPTION's URL", {
  dir <- new_pkg()
  expect_null(quarto_config(dir)$website$`site-url`)

  desc::desc_set(
    URL = "https://github.com/someuser/examplepkg, https://someuser.github.io/examplepkg/",
    file = dir
  )
  expect_identical(
    quarto_config(dir)$website$`site-url`,
    "https://someuser.github.io/examplepkg"
  )
  # `url:` in _quartodown.yml comes first.
  expect_identical(
    quarto_config(
      dir,
      list(url = "https://example.org/pkg/")
    )$website$`site-url`,
    "https://example.org/pkg"
  )
})

test_that("quarto_config() turns on social cards with the title and logo", {
  dir <- new_pkg()
  desc::desc_set(Title = "Does Useful\n    Things", file = dir)

  website <- quarto_config(dir)$website
  expect_true(website$`open-graph`)
  expect_identical(website$`twitter-card`$`card-style`, "summary")
  expect_identical(website$description, "Does Useful Things")
  expect_null(website$image)

  dir.create(file.path(dir, "man", "figures"), recursive = TRUE)
  file.create(file.path(dir, "man", "figures", "logo.png"))
  expect_identical(quarto_config(dir)$website$image, "/man/figures/logo.png")
})

test_that("write_404() writes a 404 page only for a site with a URL", {
  dir <- new_pkg()
  page <- file.path(dir, "_quarto", "404.qmd")

  write_404(dir, NULL)
  expect_false(file.exists(page))

  write_404(dir, "https://someuser.github.io/examplepkg")
  lines <- readLines(page)
  expect_true("title: Page not found" %in% lines)
  expect_true("search: false" %in% lines)
})

test_that("quarto_config() uses the logo as favicon, preferring the PNG", {
  dir <- new_pkg()
  expect_null(quarto_config(dir)$website$favicon)

  figures <- file.path(dir, "man", "figures")
  dir.create(figures, recursive = TRUE)
  file.create(file.path(figures, "logo.svg"))
  expect_identical(quarto_config(dir)$website$favicon, "man/figures/logo.svg")

  file.create(file.path(figures, "logo.png"))
  expect_identical(quarto_config(dir)$website$favicon, "man/figures/logo.png")
})

test_that("site_logo() picks the logo, preferring SVG, and its light variant", {
  dir <- new_pkg()
  expect_null(site_logo(dir, list()))

  figures <- file.path(dir, "man", "figures")
  dir.create(figures, recursive = TRUE)
  file.create(file.path(figures, "logo.png"))
  expect_identical(site_logo(dir, list()), "man/figures/logo.png")
  expect_null(site_logo(dir, list(logo = FALSE)))

  file.create(file.path(figures, "logo-light.png"))
  expect_identical(
    site_logo(dir, list()),
    c(light = "man/figures/logo-light.png", dark = "man/figures/logo.png")
  )

  # SVGs first, unlike the favicon.
  file.create(file.path(figures, c("logo.svg", "logo-light.svg")))
  expect_identical(
    site_logo(dir, list()),
    c(light = "man/figures/logo-light.svg", dark = "man/figures/logo.svg")
  )
  expect_identical(quarto_config(dir)$website$favicon, "man/figures/logo.png")
})

test_that("read_config() requires `logo:` to be true or false", {
  dir <- new_pkg()
  dir.create(file.path(dir, "_quartodown"))
  config_path <- file.path(dir, "_quartodown", "_quartodown.yml")
  writeLines("logo: false", config_path)
  expect_identical(read_config(dir), list(logo = FALSE))

  writeLines("logo: man/figures/hex.png", config_path)
  expect_error(read_config(dir), "`logo:` must be")
})

test_that("quarto_config() links GitHub and merges `quarto:` overrides", {
  dir <- new_pkg()
  write(
    "URL: https://github.com/someuser/examplepkg",
    file.path(dir, "DESCRIPTION"),
    append = TRUE
  )

  config <- list(
    quarto = list(
      format = list(
        html = list(
          theme = list(light = c("default", "quartodown.scss", "custom.scss"))
        )
      ),
      website = list(`site-url` = "https://example.org")
    )
  )
  settings <- quarto_config(dir, config)

  expect_identical(
    settings$website$navbar$tools[[1]]$href,
    "https://github.com/someuser/examplepkg"
  )
  expect_identical(settings$website$`site-url`, "https://example.org")
  expect_identical(
    settings$format$html$theme$light,
    c("default", "quartodown.scss", "custom.scss")
  )
  expect_identical(
    settings$format$html$theme$dark,
    c("darkly", "quartodown-fonts.scss", "quartodown.scss")
  )
  expect_true(settings$format$html$toc)
})

test_that("`quarto:` adds navbar entries and `~` removes a setting", {
  dir <- new_pkg()
  write(
    "URL: https://github.com/someuser/examplepkg",
    file.path(dir, "DESCRIPTION"),
    append = TRUE
  )

  config <- yaml::yaml.load(
    "
quarto:
  website:
    navbar:
      right:
        - text: Blog
          href: https://example.com
  format:
    html:
      toc: ~
"
  )
  settings <- quarto_config(dir, config)

  expect_identical(
    settings$website$navbar$right,
    list(list(text = "Blog", href = "https://example.com"))
  )
  expect_identical(settings$website$navbar$tools[[1]]$icon, "github")
  expect_false("toc" %in% names(settings$format$html))
  expect_null(settings$format$html$`code-link`)
})

test_that("build_website_readme() moves badges into the sidebar and keeps the body", {
  dir <- new_pkg()
  readme <- c(
    "<!-- badges: start -->",
    "[![badge](b.svg)](https://example.org)",
    "<!-- badges: end -->",
    "",
    "# examplepkg",
    "",
    "Some text."
  )

  home <- build_website_readme(readme, dir)
  expect_identical(
    home[1:5],
    c("::::: {.quartodown-home}", "# examplepkg", "", "Some text.", ":::::")
  )
  expect_identical(home[7], "::: {.column-margin}")
  expect_identical(home[length(home)], ":::")
  expect_false(any(grepl("badges:", home, fixed = TRUE)))
  dev_status <- which(home == "#### Dev status")
  expect_identical(
    home[dev_status + 2],
    "[![badge](b.svg)](https://example.org)"
  )
})

test_that("theme_pictures_to_images() turns a light/dark <picture> into themed <img>s", {
  readme <- c(
    "# pkg <picture><source media=\"(prefers-color-scheme: dark)\" srcset=\"dark.png\" /><img src=\"light.png\" align=\"right\" alt=\"logo\" /></picture>",
    ""
  )
  expect_identical(
    theme_pictures_to_images(readme),
    c(
      paste0(
        "# pkg ",
        "<img src=\"light.png\" align=\"right\" alt=\"logo\" class=\"light-content\" />",
        "<img src=\"dark.png\" align=\"right\" alt=\"logo\" class=\"dark-content\" />"
      ),
      ""
    )
  )
})

test_that("theme_pictures_to_images() handles multi-line pictures, both sources and existing classes", {
  readme <- c(
    "<picture>",
    "  <source media=\"(prefers-color-scheme: light)\" srcset=\"l.png 1x, l@2x.png 2x\">",
    "  <source media='(prefers-color-scheme: dark)' srcset='d.png'>",
    "  <img src=\"fallback.png\" class=\"logo\">",
    "</picture>",
    "Text."
  )
  expect_identical(
    theme_pictures_to_images(readme),
    c(
      "<img src=\"l.png\" class=\"logo light-content\"><img src=\"d.png\" class=\"logo dark-content\">",
      "Text."
    )
  )
})

test_that("theme_pictures_to_images() leaves other pictures and plain READMEs alone", {
  other <- "<picture><source media=\"(min-width: 600px)\" srcset=\"w.png\"><img src=\"n.png\"></picture>"
  expect_identical(theme_pictures_to_images(other), other)
  plain <- c("# pkg", "", "Text.", "")
  expect_identical(theme_pictures_to_images(plain), plain)
})

test_that("write_home() writes _quarto/index.qmd and leaves README.md alone", {
  dir <- new_pkg()
  readme_path <- file.path(dir, "README.md")
  writeBin(charToRaw("# examplepkg\n\nSome text.\n"), readme_path)
  before <- readBin(readme_path, "raw", file.size(readme_path))

  write_home(dir)

  expect_identical(readBin(readme_path, "raw", file.size(readme_path)), before)
  home <- readLines(file.path(dir, "_quarto", "index.qmd"))
  expect_true("# examplepkg" %in% home)
  expect_true("Some text." %in% home)
})

test_that("write_home() uses `home: file:` instead of README.md", {
  dir <- new_pkg()
  writeLines("# examplepkg\n\nFor GitHub.", file.path(dir, "README.md"))
  writeLines("# examplepkg\n\nFor the site.", file.path(dir, "site.md"))

  write_home(dir, list(home = list(file = "site.md")))

  home <- readLines(file.path(dir, "_quarto", "index.qmd"))
  expect_true("For the site." %in% home)
  expect_false("For GitHub." %in% home)

  expect_error(
    write_home(dir, list(home = list(file = "missing.md"))),
    "doesn't exist"
  )
  expect_error(
    write_home(dir, list(home = list(file = c("a.md", "b.md")))),
    "single file path"
  )
})

test_that("a .qmd home page keeps its front matter and code comments", {
  dir <- new_pkg()
  lines <- c(
    "---",
    "execute:",
    "  echo: false",
    "---",
    "",
    "```{r}",
    "# not a title",
    "1 + 1",
    "```"
  )

  home <- build_website_readme(lines, dir, qmd = TRUE)

  expect_identical(home[1:4], lines[1:4])
  expect_true("# not a title" %in% home)
  expect_identical(sum(home == "```{r}"), 1L)
})

test_that("a .md home page's code doesn't run", {
  dir <- new_pkg()
  lines <- c(
    "```{r}",
    "1 + 1",
    "```",
    "",
    "````markdown",
    "```{r}",
    "x < 1",
    "```",
    "````"
  )

  home <- build_website_readme(lines, dir, list(home = list(sidebar = FALSE)))

  expect_identical(
    home,
    c(
      "```r",
      "1 + 1",
      "```",
      "",
      "",
      "<pre class=\"sourceCode markdown\"><code>&#96;&#96;&#96;{r}",
      "x &lt; 1",
      "&#96;&#96;&#96;</code></pre>",
      ""
    )
  )
})

test_that("`home: sidebar: false` shows the home page as written", {
  dir <- new_pkg()
  writeLines(
    c(
      "# examplepkg",
      "",
      "<!-- badges: start -->",
      "[![badge](b.svg)](https://example.com)",
      "<!-- badges: end -->",
      "",
      "> [!NOTE]",
      "> Note."
    ),
    file.path(dir, "README.md")
  )

  write_home(dir, list(home = list(sidebar = FALSE)))

  home <- readLines(file.path(dir, "_quarto", "index.qmd"))
  expect_identical(home[1:5], readLines(file.path(dir, "README.md"))[1:5])
  expect_false(any(grepl("quartodown-home|column-margin", home)))
  expect_true("::: {.callout-note}" %in% home)
})

test_that("`home: sidebar:` orders the sections and adds components", {
  dir <- new_pkg()
  desc::desc_set(License = "MIT + file LICENSE", file = dir)
  home <- yaml::yaml.load(
    "
sidebar:
  structure: [funding, authors, license]
  components:
    funding:
      title: Funding
      text: Supported by [a grant](https://example.com).
"
  )

  sidebar <- build_sidebar_markdown(dir, config = list(home = home))
  headings <- grep("^#### ", sidebar, value = TRUE)
  expect_identical(
    headings,
    c("#### Funding", "#### Developers", "#### License")
  )
  expect_true("Supported by [a grant](https://example.com)." %in% sidebar)

  home$sidebar$structure <- list("nope")
  expect_error(check_home_config(home), "nope")

  # Without `structure:`, components come after the built-in sections.
  home$sidebar$structure <- NULL
  headings <- grep(
    "^#### ",
    build_sidebar_markdown(dir, config = list(home = home)),
    value = TRUE
  )
  expect_identical(headings[length(headings)], "#### Funding")
})

test_that("`home: links:` are added to the sidebar's Links", {
  dir <- new_pkg()
  home <- list(links = list(list(text = "Blog", href = "https://example.com")))
  sidebar <- build_sidebar_markdown(dir, config = list(home = home))
  links <- which(sidebar == "#### Links")
  expect_identical(sidebar[links + 2], "[Blog](https://example.com)")
})

test_that("check_home_config() rejects malformed `home:` settings", {
  expect_error(check_home_config(list(sidebar = "no")), "true, false")
  expect_error(
    check_home_config(list(
      sidebar = list(components = list(x = list(title = "X")))
    )),
    "components: x"
  )
  expect_error(
    check_home_config(list(links = list(list(text = "Blog")))),
    "href"
  )
  expect_warning(
    check_home_config(list(sidebar = list(html = "x.html"))),
    "html"
  )
  expect_silent(check_home_config(list(sidebar = FALSE)))
})

test_that("write_home() makes the home page from DESCRIPTION without a README", {
  dir <- new_pkg()
  desc::desc_set(
    Description = "Does useful\n    things.",
    file = dir
  )

  write_home(dir)

  home <- readLines(file.path(dir, "_quarto", "index.qmd"))
  expect_true("# examplepkg" %in% home)
  expect_true("Does useful things." %in% home)
})

test_that("read_config() treats a missing or empty _quartodown.yml as no settings", {
  dir <- new_pkg()
  expect_identical(read_config(dir), list())

  dir.create(file.path(dir, "_quartodown"))
  config_path <- file.path(dir, "_quartodown", "_quartodown.yml")
  file.create(config_path)
  expect_identical(read_config(dir), list())

  writeLines("navbar: {left: [news]}", config_path)
  expect_identical(read_config(dir), list(navbar = list(left = "news")))
})

test_that("read_config() warns about a _quartodown.yml at the package root", {
  dir <- new_pkg()
  writeLines("navbar: {left: [news]}", file.path(dir, "_quartodown.yml"))
  expect_warning(
    config <- read_config(dir),
    "is ignored.*_quartodown/_quartodown.yml"
  )
  expect_identical(config, list())
})

test_that("read_config() warns about unknown fields, suggesting close ones", {
  dir <- new_pkg()
  dir.create(file.path(dir, "_quartodown"))
  config_path <- file.path(dir, "_quartodown", "_quartodown.yml")

  writeLines("refrence: []", config_path)
  expect_warning(read_config(dir), "Did you mean.*reference")

  writeLines("template: {bootstrap: 5}", config_path)
  expect_warning(read_config(dir), "Known fields")

  writeLines("home: {fil: index.md}", config_path)
  expect_warning(read_config(dir), "under `home:`.*Did you mean.*file")

  writeLines("home: {file: index.md, sidebar: false}", config_path)
  expect_no_warning(read_config(dir))
})

test_that("copy_theme_files() copies the SCSS and scripts into _quarto/", {
  dir <- new_pkg()
  copy_theme_files(dir)
  expect_true(all(file.exists(file.path(dir, "_quarto", theme_files))))
})

test_that("copy_site_files() copies _quartodown/ into _quarto/, if there is one", {
  dir <- new_pkg()
  expect_no_error(copy_site_files(dir))
  expect_false(dir.exists(file.path(dir, "_quarto", "_quartodown")))

  dir.create(file.path(dir, "_quartodown", "partials"), recursive = TRUE)
  file.create(file.path(dir, "_quartodown", "partials", "title-block.html"))
  copy_site_files(dir)
  expect_true(file.exists(
    file.path(dir, "_quarto", "_quartodown", "partials", "title-block.html")
  ))
})

test_that("quarto_config() adds _quartodown/extra.scss and extra.css when present", {
  dir <- new_pkg()
  html <- quarto_config(dir)$format$html
  expect_null(html$css)

  dir.create(file.path(dir, "_quartodown"))
  file.create(file.path(dir, "_quartodown", c("extra.scss", "extra.css")))
  html <- quarto_config(dir)$format$html
  expect_identical(
    html$theme$light,
    c(
      "default",
      "quartodown-fonts.scss",
      "quartodown.scss",
      "_quartodown/extra.scss"
    )
  )
  expect_identical(
    html$theme$dark,
    c(
      "darkly",
      "quartodown-fonts.scss",
      "quartodown.scss",
      "_quartodown/extra.scss"
    )
  )
  expect_identical(html$css, "_quartodown/extra.css")
})

test_that("quarto_theme() uses `theme:`, with quartodown's fonts only on `default`", {
  dir <- new_pkg()
  expect_identical(
    quarto_theme(dir),
    list(
      light = c("default", "quartodown-fonts.scss", "quartodown.scss"),
      dark = c("darkly", "quartodown-fonts.scss", "quartodown.scss")
    )
  )
  expect_identical(
    quarto_theme(dir, list(theme = list(dark = "cyborg")))$dark,
    c("cyborg", "quartodown-fonts.scss", "quartodown.scss")
  )
  expect_identical(
    quarto_theme(dir, list(theme = list(light = "flatly", dark = "darkly"))),
    list(
      light = c("flatly", "quartodown.scss"),
      dark = c("darkly", "quartodown.scss")
    )
  )
})

test_that("quarto_theme() rejects a `theme:` that isn't light/dark names", {
  dir <- new_pkg()
  expect_error(quarto_theme(dir, list(theme = "flatly")), "light.*dark")
  expect_error(
    quarto_theme(dir, list(theme = list(light = c("a", "b")))),
    "single theme name"
  )
})

test_that("read_config() warns about unknown fields under `theme:`", {
  dir <- new_pkg()
  dir.create(file.path(dir, "_quartodown"))
  config_path <- file.path(dir, "_quartodown", "_quartodown.yml")
  writeLines("theme: {ligth: flatly}", config_path)
  expect_warning(read_config(dir), "under `theme:`.*Did you mean.*light")
})

test_that("data_sidebar_authors() links to authors.html only when a non-default-role author exists", {
  dir <- new_pkg()
  bullets <- data_sidebar_authors(dir)
  expect_false(any(grepl("More about authors", bullets, fixed = TRUE)))

  dir_ctb <- withr::local_tempdir()
  writeLines(
    c(
      "Package: examplepkg",
      "Authors@R: c(",
      "    person(\"Jane\", \"Doe\", role = c(\"aut\", \"cre\")),",
      "    person(\"John\", \"Smith\", role = \"ctb\"))"
    ),
    file.path(dir_ctb, "DESCRIPTION")
  )
  bullets_ctb <- data_sidebar_authors(dir_ctb)
  expect_true(any(grepl(
    "More about authors...\\]\\(authors.html\\)",
    bullets_ctb
  )))
  expect_false(any(grepl("John Smith", bullets_ctb)))
})

test_that("data_sidebar_citation() links to the authors.html #citation anchor", {
  dir <- new_pkg()
  bullets <- data_sidebar_citation(dir)
  expect_true(any(grepl("authors.html#citation", bullets, fixed = TRUE)))
})

test_that("data_sidebar_community() links the community files, from the root or .github/", {
  dir <- new_pkg()
  expect_identical(data_sidebar_community(dir), character())

  dir.create(file.path(dir, ".github"))
  writeLines("x", file.path(dir, ".github", "CODE_OF_CONDUCT.md"))
  writeLines("x", file.path(dir, "CONTRIBUTING.md"))

  bullets <- data_sidebar_community(dir)
  expect_true(any(grepl(
    "[Code of conduct](CODE_OF_CONDUCT.html)",
    bullets,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "[Contributing guide](CONTRIBUTING.html)",
    bullets,
    fixed = TRUE
  )))
  expect_false(any(grepl("SUPPORT", bullets)))
})

test_that("data_sidebar_license() links LICENSE.md and the LICENSE file", {
  dir <- new_pkg()
  desc::desc_set(License = "MIT + file LICENSE", file = dir)
  writeLines("YEAR: 2026", file.path(dir, "LICENSE"))
  bullets <- data_sidebar_license(dir)
  expect_true(any(grepl("[LICENSE](LICENSE-text.html)", bullets, fixed = TRUE)))
  expect_false(any(grepl("Full license", bullets)))

  writeLines("# MIT License", file.path(dir, "LICENSE.md"))
  bullets <- data_sidebar_license(dir)
  expect_true(any(grepl("[Full license](LICENSE.html)", bullets, fixed = TRUE)))
})

test_that("`authors:` links names, picks roles and changes the footer's text", {
  dir <- new_pkg()
  desc::desc_set(Title = "Example Package", file = dir)
  desc::desc_set_authors(
    c(
      person("Jane", "Doe", role = c("aut", "cre")),
      person("Acme", role = c("cph", "fnd"))
    ),
    file = dir
  )
  config <- yaml::yaml.load(
    "
authors:
  Jane Doe:
    href: https://jane.example.com
  Acme:
    href: https://acme.example.com
    html: <img src='acme.png' alt='Acme'>
  sidebar:
    roles: [cre]
    after: Thanks to all contributors.
  footer:
    roles: [fnd]
    text: Funded by
"
  )

  sidebar <- data_sidebar_authors(dir, config)
  expect_true(any(grepl(
    "<a href='https://jane.example.com'>Jane Doe</a>",
    sidebar,
    fixed = TRUE
  )))
  expect_false(any(grepl("acme", sidebar)))
  expect_true(any(grepl("Thanks to all contributors.", sidebar, fixed = TRUE)))

  expect_identical(
    data_developed_by(dir, config),
    "Funded by <a href='https://acme.example.com'><img src='acme.png' alt='Acme'></a>."
  )
  expect_identical(data_developed_by(dir), "Developed by Jane Doe, Acme.")

  build_authors_qmd(dir, config)
  authors <- readLines(file.path(dir, "_quarto", "authors.qmd"))
  expect_true(any(grepl(
    "<strong><a href='https://jane.example.com'>Jane Doe</a></strong>",
    authors,
    fixed = TRUE
  )))
})

test_that("build_authors_qmd() writes an Authors and Citation page with all authors and a fenced BibTeX code block", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "_quarto"))
  writeLines(
    c(
      "Package: examplepkg",
      "Version: 1.0.0",
      "Title: Example Package",
      "Authors@R: c(",
      "    person(\"Jane\", \"Doe\", role = c(\"aut\", \"cre\")),",
      "    person(\"John\", \"Smith\", role = \"ctb\"))"
    ),
    file.path(dir, "DESCRIPTION")
  )

  build_authors_qmd(dir)

  out_path <- file.path(dir, "_quarto", "authors.qmd")
  expect_true(file.exists(out_path))
  lines <- readLines(out_path)

  expect_identical(lines[1:3], c("---", 'title: "Authors and Citation"', "---"))
  expect_true(any(grepl("Jane Doe", lines, fixed = TRUE)))
  expect_true(any(grepl("John Smith", lines, fixed = TRUE)))
  expect_true(any(grepl("^## Citation \\{#citation\\}$", lines)))
  # Without a repository to link to, as DESCRIPTION isn't part of the site.
  expect_true(any(grepl(
    "Source: <code>DESCRIPTION</code></small>",
    lines,
    fixed = TRUE
  )))
  expect_true(any(lines == "```bibtex"))
  open_idx <- which(lines == "```bibtex")
  close_idx <- which(lines == "```")
  bibtex_block <- lines[seq(
    open_idx + 1,
    close_idx[close_idx > open_idx][1] - 1
  )]
  expect_true(any(grepl("@Manual\\{", bibtex_block)))
})

test_that("build_authors_qmd() shows author comments without ORCID/ROR, and the sidebar links to them", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "_quarto"))
  writeLines(
    c(
      "Package: examplepkg",
      "Version: 1.0.0",
      "Title: Example Package",
      "Authors@R: c(",
      "    person(\"Jane\", \"Doe\", role = c(\"aut\", \"cre\"),",
      "           comment = c(ORCID = \"0000-0001-2345-6789\", \"See <https://example.com>\")),",
      "    person(\"John\", \"Smith\", role = \"aut\"),",
      "    person(\"Org\", role = \"fnd\"))"
    ),
    file.path(dir, "DESCRIPTION")
  )

  build_authors_qmd(dir)
  lines <- readLines(file.path(dir, "_quarto", "authors.qmd"))

  expect_true(any(grepl(
    "<br /><small>See &lt;<a href='https://example.com'>https://example.com</a>&gt;</small></p></li>",
    lines,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Author, maintainer. <a href='https://orcid.org/0000-0001-2345-6789'><img src='orcid.svg' class='orcid' alt='ORCID'></a>\n<br /><small>See",
    paste(lines, collapse = "\n"),
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "<li><p><strong>John Smith</strong>. Author.</p></li>",
    lines,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "<li><p><strong>Org</strong>. Funder.</p></li>",
    lines,
    fixed = TRUE
  )))

  bullets <- data_sidebar_authors(dir)
  expect_true(any(grepl("More about authors", bullets, fixed = TRUE)))
  expect_false(any(grepl("example.com", bullets, fixed = TRUE)))
  expect_true(any(grepl(
    "</small> <a href='https://orcid.org/0000-0001-2345-6789'>",
    bullets,
    fixed = TRUE
  )))
})

test_that("build_authors_qmd() prefers an inst/CITATION file and links the Source note to it", {
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "_quarto"))
  writeLines(
    c(
      "Package: examplepkg",
      "Version: 1.0.0",
      "Title: Example Package",
      "URL: https://github.com/someuser/examplepkg",
      "Authors@R: person(\"Jane\", \"Doe\", role = c(\"aut\", \"cre\"))"
    ),
    file.path(dir, "DESCRIPTION")
  )
  dir.create(file.path(dir, "inst"), recursive = TRUE)
  writeLines(
    'bibentry("Manual", title = "Custom Citation Title", author = "Someone Else")',
    file.path(dir, "inst", "CITATION")
  )

  build_authors_qmd(dir)

  lines <- readLines(file.path(dir, "_quarto", "authors.qmd"))
  expect_true(any(grepl("Custom Citation Title", lines, fixed = TRUE)))
  expect_true(any(grepl(
    "Source: <a href=\"https://github.com/someuser/examplepkg/blob/HEAD/inst/CITATION\"><code>inst/CITATION</code></a>",
    lines,
    fixed = TRUE
  )))
})

test_that("the citation year comes from DESCRIPTION's Date, not the build date", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "Package: examplepkg",
      "Version: 1.0.0",
      "Title: Example Package",
      "Date: 2001-02-03",
      "Authors@R: person(\"Jane\", \"Doe\", role = c(\"aut\", \"cre\"))"
    ),
    file.path(dir, "DESCRIPTION")
  )
  expect_identical(pkg_citation(dir)$year, "2001")

  desc::desc_del("Date", file = dir)
  expect_identical(pkg_citation(dir)$year, format(Sys.Date(), "%Y"))
})

test_that("rd_topics() reads alias/title/concept/keyword tags from every man/*.Rd", {
  dir <- new_pkg_with_man()
  write_rd(
    dir,
    "foo_bar",
    title = "Foo bar",
    concept = "strings",
    keyword = "internal"
  )

  topics <- rd_topics(dir)
  expect_length(topics, 1)
  expect_identical(topics[[1]]$alias, "foo_bar")
  expect_identical(topics[[1]]$title, "Foo bar")
  expect_identical(topics[[1]]$concepts, "strings")
  expect_identical(topics[[1]]$keywords, "internal")
})

test_that("build_reference_qmd() supports starts_with()/-exclude/has_concept()/has_keyword() selectors", {
  dir <- new_pkg_with_man()
  write_rd(dir, "str_foo", concept = "strings")
  write_rd(dir, "str_bar", concept = "strings")
  write_rd(dir, "num_add", keyword = "internal")

  config <- reference_config(c(
    "reference:",
    "  - title: Strings",
    "    contents:",
    '      - starts_with("str_")',
    '      - -has_keyword("internal")',
    "  - title: Internal",
    "    contents:",
    '      - has_keyword("internal")'
  ))

  build_reference_qmd(dir, config)
  lines <- readLines(file.path(dir, "_quarto", "reference", "index.qmd"))

  expect_true(any(grepl("str_foo()", lines, fixed = TRUE)))
  expect_true(any(grepl("str_bar()", lines, fixed = TRUE)))
  # has_keyword() selects internal topics.
  expect_true("## Internal" %in% lines)
  expect_true(any(grepl("num_add()", lines, fixed = TRUE)))
})

test_that("build_reference_qmd() lists internal topics only when named or selected", {
  dir <- new_pkg_with_man()
  write_rd(dir, "foo")
  write_rd(dir, "examplepkg-package", usage = NULL, keyword = "internal")
  write_rd(
    dir,
    "reexports",
    alias = "\\%>\\%",
    usage = NULL,
    keyword = "internal"
  )
  index_lines <- function() {
    readLines(file.path(dir, "_quarto", "reference", "index.qmd"))
  }

  # Unlisted internal topics don't warn.
  expect_no_warning(build_reference_qmd(dir))
  expect_true(any(grepl("foo()", index_lines(), fixed = TRUE)))
  expect_false(any(grepl("examplepkg-package|reexports", index_lines())))

  # A selector skips them unless `internal = TRUE`.
  config <- reference_config(c(
    "reference:",
    "  - title: Main",
    "    contents:",
    '      - matches(".")',
    "  - title: Internal",
    "    contents:",
    '      - starts_with("examplepkg")'
  ))
  expect_no_warning(build_reference_qmd(dir, config))
  expect_false("## Internal" %in% index_lines())
  expect_false(any(grepl("examplepkg-package|reexports", index_lines())))

  config <- reference_config(c(
    "reference:",
    "  - title: Main",
    "    contents:",
    "      - foo",
    '      - starts_with("examplepkg", internal = TRUE)',
    "  - title: Re-exported",
    "    contents:",
    "      - reexports"
  ))
  build_reference_qmd(dir, config)
  expect_true("## Re-exported" %in% index_lines())
  expect_true(any(grepl(
    '<a href="examplepkg-package.qmd">examplepkg-package</a>',
    index_lines(),
    fixed = TRUE
  )))
  expect_true(any(grepl(
    '<a href="reexports.qmd">',
    index_lines(),
    fixed = TRUE
  )))
})

test_that("a `reference:` list starting with an exclusion leaves internal topics out", {
  dir <- new_pkg_with_man()
  write_rd(dir, "foo")
  write_rd(dir, "bar")
  write_rd(dir, "hidden", keyword = "internal")

  config <- reference_config(c(
    "reference:",
    "  - title: All",
    "    contents:",
    "      - -bar"
  ))
  # bar is unlisted, hidden isn't (being internal).
  expect_warning(build_reference_qmd(dir, config), "\"bar\", which is not")
  lines <- readLines(file.path(dir, "_quarto", "reference", "index.qmd"))
  expect_true(any(grepl("foo()", lines, fixed = TRUE)))
  expect_false(any(grepl("bar()|hidden", lines)))
})

test_that("build_reference_qmd() renders title-only and subtitle blocks", {
  dir <- new_pkg_with_man()
  write_rd(dir, "foo")

  config <- reference_config(c(
    "reference:",
    "  - title: Verbs",
    "  - subtitle: Rows",
    "    desc: Row verbs.",
    "    contents:",
    "      - foo"
  ))

  build_reference_qmd(dir, config)
  lines <- readLines(file.path(dir, "_quarto", "reference", "index.qmd"))
  expect_true("## Verbs" %in% lines)
  expect_true("### Rows" %in% lines)
  expect_false("## " %in% lines)
  # The title-only block gets no empty <dl>.
  expect_length(grep('<dl class="ref-index">', lines, fixed = TRUE), 1)
})

test_that("build_reference_qmd() lists every topic in one section without `reference:`", {
  dir <- new_pkg_with_man()
  write_rd(dir, "foo")
  write_rd(dir, "bar")

  expect_no_warning(build_reference_qmd(dir))
  lines <- readLines(file.path(dir, "_quarto", "reference", "index.qmd"))
  expect_true("## All functions" %in% lines)
  expect_true(any(grepl("foo()", lines, fixed = TRUE)))
  expect_true(any(grepl("bar()", lines, fixed = TRUE)))
})

test_that("build_reference_qmd() warns about topics missing from `reference:`", {
  dir <- new_pkg_with_man()
  write_rd(dir, "listed_fun")
  write_rd(dir, "orphan_fun")

  config <- reference_config(c(
    "reference:",
    "  - title: All",
    "    contents:",
    "      - listed_fun"
  ))
  expect_warning(build_reference_qmd(dir, config), "orphan_fun")
})

test_that("build_reference_qmd() accepts a non-syntactic alias like a *-package alias verbatim", {
  dir <- new_pkg_with_man()
  write_rd(
    dir,
    "examplepkg-package",
    alias = "examplepkg-package",
    title = "examplepkg",
    usage = NULL
  )

  config <- reference_config(c(
    "reference:",
    "  - title: All",
    "    contents:",
    "      - examplepkg-package"
  ))
  build_reference_qmd(dir, config)
  lines <- readLines(file.path(dir, "_quarto", "reference", "index.qmd"))
  expect_true(any(grepl(">examplepkg-package</a>", lines, fixed = TRUE)))
})

test_that("build_reference_qmd() lists every function in a topic's usage", {
  dir <- new_pkg_with_man()
  write_rd(
    dir,
    "is_bound",
    alias = c("is_bound", "check_bound"),
    usage = c("is_bound(pkg)", "", "check_bound(pkg)")
  )

  build_reference_qmd(dir)
  lines <- readLines(file.path(dir, "_quarto", "reference", "index.qmd"))
  expect_true(any(grepl(
    '<dt><code><a href="is_bound.qmd">is_bound()</a></code> <code><a href="is_bound.qmd">check_bound()</a></code></dt>',
    lines,
    fixed = TRUE
  )))
})

test_that("usage_names() formats data, infix, replacement and method usages", {
  rd_usage <- function(usage) {
    rd <- tools::parse_Rd(textConnection(c(
      "\\name{x}",
      "\\usage{",
      usage,
      "}"
    )))
    rd[vapply(rd, attr, character(1), "Rd_tag") == "\\usage"]
  }

  expect_identical(usage_names(rd_usage("mtcars")), "mtcars")
  # Backticks as entities, which Pandoc doesn't take for a code span.
  expect_identical(usage_names(rd_usage("x \\%in\\% table")), "&#96;%in%&#96;")
  expect_identical(
    usage_names(rd_usage("names(x) <- value")),
    "&#96;names&lt;-&#96;()"
  )
  expect_identical(
    usage_names(rd_usage("\\method{print}{foo}(x, \\dots)")),
    "print(<i>&lt;foo&gt;</i>)"
  )
  expect_identical(
    usage_names(rd_usage("\\method{print}{`NULL`}(x, \\dots)")),
    "print(<i>&lt;NULL&gt;</i>)"
  )
  # A method of a generic documented in the same topic is dropped.
  expect_identical(
    usage_names(rd_usage(c(
      "summary(object, ...)",
      "\\method{summary}{foo}(object, ...)"
    ))),
    "summary()"
  )
  expect_identical(usage_names(list()), character())
})

test_that("build_reference_qmd() errors on a selector matching no topic", {
  dir <- new_pkg_with_man()
  write_rd(dir, "listed_fun")

  config <- reference_config(c(
    "reference:",
    "  - title: All",
    "    contents:",
    "      - not_a_real_function"
  ))
  expect_error(build_reference_qmd(dir, config), "known")
})

test_that("check_config() catches mistakes the build would only hit later", {
  dir <- new_pkg_with_man()
  write_rd(dir, "foo")
  topics <- rd_topics(dir)
  expect_no_error(check_config(dir, list(), topics))

  check <- function(config) check_config(dir, config, topics)
  expect_error(check(list(home = list(file = "missing.md"))), "doesn't exist")
  expect_error(check(list(theme = "flatly")), "light.*dark")
  expect_error(check(list(navbar = list(style = "left"))), "top.*sidebar")
  expect_error(check(list(navbar = list(left = list("nope")))), "nope")
  expect_error(
    check(list(reference = list(list(contents = "bar")))),
    "known"
  )
})

test_that("copy_theme_files() marks a development site for the navbar and search engines", {
  withr::local_envvar(QUARTODOWN_DEV_MODE = NA)
  dir <- new_pkg()
  header <- file.path(dir, "_quarto", "quartodown-header.html")

  # Without `development:`, there's no other site to link.
  copy_theme_files(dir)
  expect_false(any(grepl("quartodown-dev-mode", readLines(header))))

  copy_theme_files(dir, list(development = list(mode = "release")))
  lines <- readLines(header)
  expect_true(
    '<meta name="quartodown-dev-mode" content="release">' %in% lines
  )
  expect_false(any(grepl("noindex", lines)))

  copy_theme_files(dir, list(development = list(mode = "devel")))
  lines <- readLines(header)
  expect_true('<meta name="quartodown-dev-mode" content="devel">' %in% lines)
  expect_true('<meta name="robots" content="noindex">' %in% lines)
})

test_that("read_config() warns about unknown fields under `development:`", {
  dir <- new_pkg()
  dir.create(file.path(dir, "_quartodown"))
  writeLines(
    c("development:", "  mode: devel", "  destination: dev"),
    file.path(dir, "_quartodown", "_quartodown.yml")
  )
  expect_warning(read_config(dir), "destination")
})
