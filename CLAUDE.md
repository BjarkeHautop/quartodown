# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

## What this is

quartodown is an R package that builds Bootstrap 5 package websites with Quarto.
It generates a Quarto website project in `_quarto/` from the package sources,
renders it into `docs/` with `quarto render` (run through `processx`), then
post-processes the HTML. Configuration is the optional
`_quartodown/_quartodown.yml`. The exported API is `use_quartodown()` (one-time
setup, writes `_quartodown/_quartodown.yml` and gitignore entries),
`use_quartodown_action()` (writes the GitHub Pages workflow from
[inst/templates/quartodown.yaml](inst/templates/quartodown.yaml)),
`build_site()` and `preview_site()`.

## Commands

```r
devtools::load_all() # load package for interactive use
devtools::document() # regenerate man/ and NAMESPACE from roxygen
devtools::test() # run all tests
testthat::test_file("tests/testthat/test-tooltips.R") # run one test file
devtools::test(filter = "build-site") # run tests matching a file name
devtools::check() # R CMD check
```

Build this package's own site (needs Quarto installed; CI uses these args):

```r
install.packages(".", repos = NULL, type = "source")
quartodown::build_site() # also previews it when interactive
```

`README.md` is generated from `README.qmd` --- edit the `.qmd` and re-render.

## Architecture

`build_site()` in [R/build_site.R](R/build_site.R) runs three stages:

1. **Before rendering** --- empties `_quarto/` (keeping `_freeze/` with
   `freeze = TRUE`) and writes the whole project into it
   ([R/project.R](R/project.R)): `vignettes/`, `man/figures/`,
   `_quartodown.yml`'s `vignettes: files:` (`vignette_files()`, so vignettes'
   `../` paths resolve; `check_vignette_paths()` warns about uncopied ones, and
   documents in them are kept from rendering by an explicit `project: render:`
   list, `render_list()`, as Quarto ignores a negated `!man/`), `NEWS.md` (with
   `#123`/`@user` links), `LICENSE.md`, `LICENSE` as `LICENSE-text.md`, and
   CODE_OF_CONDUCT/CONTRIBUTING/SUPPORT `.md` from the root or `.github/`
   (pkgdown's layout), plus:
   - These `.md` files and the README go through `github_markdown_to_quarto()`:
     GitHub alerts (`> [!NOTE]`) → callouts; in the README only (written as
     `.qmd`, since Quarto won't run code cells in `.md`), ```` ```mermaid ```` →
     ```` ```{mermaid} ````. Executable ```` ```{r} ```` blocks become plain,
     except in a `.qmd` `home: file:`, and a README.md's code blocks holding a
     chunk's fence become raw HTML (`escape_knitr_blocks()`), since knitr runs
     fences nested in code blocks too.
   - `reference/<topic>.qmd` ([R/rd2qmd.R](R/rd2qmd.R)): each Rd parsed with the
     package's Rd macros ([R/rd-macros.R](R/rd-macros.R)), rendered with
     `tools::Rd2HTML()` running build/install/render `\Sexpr` stages (with the
     package's temp library on `.libPaths()`, so it is installed first, before
     stage 1), then cut into sections (YAML `title:`, `##` headings). `\link`s
     are first rewritten to `\href`s (own topic page, else
     `downlit::href_topic()`), since Rd2HTML drops link targets; roxygen's R6
     `../../pkg/html/` hrefs likewise. Usage and R6 method usage → fenced
     ```` ```r ````; prose stays HTML except inline `<code>` → Markdown code
     spans, `reqn` → `$`/`$$` math, `$` escaped, language-tagged `<pre>` →
     fenced blocks. Inline `` `r x` `` and blocks holding a chunk's fence stay
     raw HTML (`html_code()`, `html_code_block()`), as knitr would run them. A
     `Source:` line under the title links the files roxygen's
     `% Please edit documentation in` header names (else the `.Rd`) to
     `<GitHub URL>/blob/HEAD/` (`source_note()`, shared with the authors page
     and the vignettes, which `copy_vignettes()` gives one after their front
     matter). The repository's URLs come from `pkg_repo()` (DESCRIPTION's
     GitHub/GitLab/Codeberg `URL`, recognized by `repo_host()`, which
     `site_url()` skips; a `/tree/<branch>/<folder>` URL for a package in a
     subfolder; and `_quartodown.yml`'s pkgdown-style `repo:`). Examples are
     rebuilt from `tools::Rd2ex()` markers into run / `\dontrun` (plain block) /
     `\dontshow` (hidden chunk) blocks, with `@examplesIf` evaluated at build
     time.
   - `_quarto.yml`: fully resolved Quarto config (navbar, footer, theme). User
     overrides from `_quartodown.yml`'s `quarto:` key are applied by
     `merge_quarto_config()` (unnamed lists append, other values replace, `~`
     removes).
   - `index.qmd`: README (or `_quartodown.yml`'s `home: file:`, or without
     either a page from `DESCRIPTION`) + sidebar (unless `home: sidebar: false`,
     which leaves the page as written) ([R/sidebar.R](R/sidebar.R)), whose
     sections `home: sidebar: structure:`/`components:` and `home: links:` set
     as in pkgdown; badges are pulled out of the `<!-- badges: start/end -->`
     block, and `<picture>` light/dark images are rewritten to
     `.light-content`/`.dark-content` `<img>`s.
   - `reference/index.qmd` ([R/reference.R](R/reference.R)): pkgdown-compatible
     `reference:` grouping with selectors (`starts_with()`, `has_concept()`,
     `-`, ...) evaluated against Rd topics. `\keyword{internal}` topics are
     listed (and get a page, see `has_page()`) only when named or selected with
     `internal = TRUE`.
   - `authors.qmd` ([R/authors.R](R/authors.R)). `_quartodown.yml`'s
     pkgdown-style `authors:` links names or swaps them for HTML
     (`author_display()`, also in the sidebar and footer) and picks the
     sidebar's and footer's roles.
   - Theme files copied verbatim from [inst/quarto/](inst/quarto/)
     (`quartodown.scss`, header/after-body HTML), with `ansi_scss_rules()`
     ([R/ansi.R](R/ansi.R)) appended to `quartodown.scss`. `quarto_theme()`
     builds the light/dark theme lists from `_quartodown.yml`'s `theme:`
     (default `default`/`darkly`); `quartodown-fonts.scss` (system fonts, no web
     fonts) is layered in only when the light theme is `default`, so Bootswatch
     themes keep their fonts.
   - The package's `_quartodown/` folder, copied to `_quarto/_quartodown/` so
     paths in `quarto:` settings resolve the same (Quarto doesn't render `_`
     folders). `extra.scss` is appended to both themes and `extra.css` set as
     `css:`. `docs/.nojekyll` keeps GitHub Pages from dropping it.
   - Then `run_examples()` ([R/examples.R](R/examples.R)) knits every reference
     page with R chunks in one fresh `Rscript` process
     ([inst/scripts/run-examples.R](inst/scripts/run-examples.R)), with the
     package's envvars, rather than Quarto starting one per page. Its hooks copy
     those of Quarto's knitr engine (`share/rmd/hooks.R`) for the options the
     pages use, and `example_knit_options()` takes Quarto's knitr options from
     the generated `_quarto.yml` (NULL, leaving all pages to Quarto, for
     settings it doesn't handle). Each page is overwritten with the executed
     Markdown, unless knitting fails or leaves `knit_meta` (htmlwidgets) or
     preserved HTML, in which case Quarto runs it. Between pages the script
     clears `globalenv()`, detaches new search-path entries and resets the
     working directory, RNG kind and changed options; options added (e.g. by a
     namespace's `.onLoad()`) and envvars aren't reset. With `freeze`, results
     are cached in `_quarto/_freeze/quartodown/reference/<page>/`, keyed by the
     page's source and the knitr options.
2. **Rendering** --- clears the site's folder (`site_dir()`: `docs/`, or
   `docs/dev/` when `dev_mode()`, from `_quartodown.yml`'s pkgdown-style
   `development: mode:` or `QUARTODOWN_DEV_MODE`, is `devel`, which `auto` picks
   for a `.9000` version of a package `on_cran()`; that also puts `site-url`
   under `dev/` and a `noindex` meta in `quartodown-header.html`), keeping
   `CNAME`, hidden files and, in `docs/`, `dev/` (which `postprocess_pages()`
   then skips too), makes the package available to the pages' R processes
   (installed into a temp library prepended via `R_LIBS`, or with `devel = TRUE`
   a generated `R_PROFILE_USER` profile that runs `pkgload::load_all()`; plus
   `R_CLI_NUM_COLORS=256` for colored output), renders `_quarto/` into it
   (`output-dir: ../docs/`; with `processx` rather than
   `quarto::quarto_render()`, whose `quiet` passes `--quiet`, which hides errors
   too, so `render_error_log()` can show them) and writes `docs/pkgdown.yml`
   (`urls` + `articles`, all downlit reads). The name is fixed: downlit fetches
   `<site URL>/pkgdown.yml` to link other packages' code to this site, so it
   must not be renamed.
3. **After rendering** --- `postprocess_pages()`
   ([R/code-link.R](R/code-link.R)) reads and writes each `docs/**/*.html` once.
   In between, `link_code_html()` drops the other site's `.quartodown-devel`/
   `-release` elements (`other_site_classes()`) and runs downlit over it
   (replacing Quarto's `code-link`, which would wipe code annotations).
   `pre.code-annotated` blocks are hidden from downlit and their `span.fu`
   tokens (and `span.sc` `%op%`s) linked one by one with
   `downlit::autolink_url()` instead. xml2 drops the ESC of ANSI codes, so
   `protect_ansi()` ([R/ansi.R](R/ansi.R)) first swaps it for a private-use
   character, and `ansi_to_html()` afterwards converts them with
   `fansi::sgr_to_html()` (not `cli::ansi_html()`, which loses colors of nested
   styles); `strip_search_ansi()` strips them from `search.json`. Before that,
   `add_tooltips_html()` ([R/tooltips.R](R/tooltips.R)) regex-rewrites the HTML
   xml2 wrote, linking references to the package's own topics
   (`<span class="fu|va|kw">` in code blocks, and `op`/`sc` spans holding a
   `%op%`, and `<code>name()</code>` inline) to their reference page, and
   appends the tips' contents in a hidden `.quartodown-tip-payload`; the
   tooltip's style and script are in `quartodown.scss` and
   `quartodown-after-body.html`. Then `write_redirects()`
   ([R/redirects.R](R/redirects.R)) writes redirect pages for
   `_quartodown.yml`'s `redirects:` (pkgdown's format; Quarto's `aliases:` needs
   front matter, which generated pages lack). Finally, with
   `preview = interactive()`, `serve_site()` ([R/project.R](R/project.R)) serves
   `docs/` with `servr::httw()`. The server is kept in the package-level `the`
   environment and reused by later builds, whose open page live-reloads.

### Navbar / articles

[R/articles.R](R/articles.R) finds vignettes: top-level, then every subfolder
(skipping `_`/`.`-prefixed names, as Quarto does). [R/navbar.R](R/navbar.R)
builds the navbar from the `left:` and `right:` lists under `_quartodown.yml`'s
`navbar:`. Each entry is one of:

- A keyword that quartodown expands into a generated item, dropped if the
  package has nothing for it:
  - `intro`: "Get started", linking the vignette named after the package, or the
    only top-level vignette.
  - `reference`: the reference index.
  - `articles`: an "Articles" menu of every vignette no other entry links.
  - `news`: "Changelog", linking `NEWS.md`.
  - `repo`: the repository icon (GitHub, GitLab, else generic), linking the repo
    from `DESCRIPTION`'s `URL`.
- Any other string: a vignette path relative to `vignettes/`, without extension,
  linked with its title.
- A Quarto navbar item (`text:` plus `href:`/`menu:`), passed through. Strings
  in its `menu:` are vignette paths too, and one ending in `/` expands to every
  vignette in that folder.

The default is `left: [intro, reference, articles, news]`, `right: [repo]`.

With `navbar: style: sidebar`, `quarto_config()` moves the resolved `left` items
into a docked `website: sidebar:` (`sidebar_contents()`): menus become sections,
and the reference item becomes a section listing every topic the index lists
(`sidebar_reference()`, one sub-section per titled group). Labels are plain
text, not code spans, since `add_tooltips_html()` would nest a link in the
sidebar link; `quartodown.scss` sets them in the code font. Pages then have
`body.docked`, which turns off the navbar's width cap and the after-body
script's title alignment, so the title sits above the sidebar.

## Repo layout notes

- `build_site()` parses `man/*.Rd` once with `rd_topics()` (each topic keeps its
  parsed `rd`) and passes the topics to `write_man_pages()`,
  `build_reference_qmd()`, `quarto_config()` and `postprocess_pages()`; their
  `topics = rd_topics(path)` default is for tests. Use `has_page()` for whether
  a topic has a page.

- Shared helpers live in [R/utils.R](R/utils.R): `html_escape()`/
  `html_unescape()`, `split_lines()`, `code_block_lines()` (Markdown code
  fences) and `split_front_matter()`. Use them rather than re-deriving. Errors
  and warnings about a `_quartodown.yml` field go through
  `config_abort()`/`config_warn()` ([R/build_site.R](R/build_site.R)); checks
  that need the package's files belong in `check_config()`, which runs before
  the install.

- `_quartodown/_quartodown.yml` configures this package's *own* site
  (dogfooding); `_quarto/` and `docs/` are build outputs.

- Tests create throwaway packages in temp dirs (see `new_pkg()` helper in
  [tests/testthat/helper-pkg.R](tests/testthat/helper-pkg.R)) and test the
  generation functions directly rather than running a full Quarto render.

- [vignettes/articles/technical.qmd](vignettes/articles/technical.qmd) is a
  user-facing overview of the build (stages, `_quarto/` layout, man pages, code
  links, theme); configuration belongs in `customizing.qmd`. Keep it in sync
  when changing build behavior.

- [vignettes/articles/gallery.qmd](vignettes/articles/gallery.qmd) shows
  screenshots of this site built with each entry in
  `vignettes/articles/gallery/` (its `config.yml` and `_quartodown/` files,
  shown on the page too). Rerun `Rscript tools/gallery.R [entry ...]` (needs
  Quarto and Chrome) after changing the styling.

- Roxygen with markdown. [vignettes/customizing.qmd](vignettes/customizing.qmd)
  is the user-facing reference for `_quartodown.yml`; `@section Configuration:`
  in `build_site()` summarizes each field in a line. Update both when adding
  config keys.
