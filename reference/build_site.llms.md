# Build the package website

Source: [`R/build_site.R`](https://github.com/bjarkehautop/quartodown/blob/HEAD/R/build_site.R)

## Description

Builds the package website into `docs/` with [Quarto](https://quarto.org/).

## Usage

``` r
build_site(
  path = ".",
  tooltips = TRUE,
  freeze = FALSE,
  quiet = TRUE,
  devel = FALSE,
  workers = 1,
  preview = interactive()
)
```

## Arguments

|  |  |
|----|----|
| `path` | Path to the package root. |
| `tooltips` | Logical. Give references to the package’s own functions (in code blocks and inline code) a hover tooltip showing the function’s usage and title? |
| `freeze` | Logical. Reuse the results of code that ran in an earlier build, for pages whose source hasn’t changed since? Uses Quarto’s [freeze](https://quarto.org/docs/projects/code-execution.html#freeze), which keeps them in `_quarto/_freeze/`. Changes to the package’s code or its dependencies aren’t detected; delete `_quarto/_freeze/` to rerun everything. |
| `quiet` | Logical. Hide Quarto’s rendering output? If a page fails to render, its error is shown either way. |
| `devel` | Logical. By default, the examples and vignettes run against the package installed from `path` into a temporary library. With `devel = TRUE`, they run against it loaded with [`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html) instead, which is faster, but code that starts other R processes (e.g. with callr or parallel workers) won’t find the package there. |
| `workers` | Number of processes to build the site with. More than 1 needs the [mirai](https://mirai.r-lib.org/) package. |
| `preview` | Logical. Open the built site in a web browser with [`preview_site()`](preview_site.llms.md)? By default, only in interactive sessions. |

## Details

The site is built in a scratch folder, `_quarto/_site/`, and replaces the one in `docs/` only once it is complete. So the previous site stays there, and viewable with [`preview_site()`](preview_site.llms.md), while the build runs and if it fails.

`build_site()` assembles a Quarto website project in `_quarto/` from the package sources and `_quartodown/_quartodown.yml`, then renders it. It uses `README.md` (or, without one, `DESCRIPTION`), `man/`, the vignettes (including those in subfolders), `NEWS.md`, `LICENSE`/`LICENSE.md`, and `CODE_OF_CONDUCT.md`, `CONTRIBUTING.md` and `SUPPORT.md` (at the top level or in `.github/`). See `vignette("quartodown")` for what the site contains.

While it builds, the environment variable `IN_QUARTODOWN` is `"true"`, so the package’s examples and vignettes can check `identical(Sys.getenv("IN_QUARTODOWN"), "true")`. `IN_QUARTODOWN_PKG` is the name of the package whose site it is, which code shared between packages can check.

## Value

Invisibly, the path to the built site.

## Configuration

`_quartodown/_quartodown.yml` is optional; [`use_quartodown()`](use_quartodown.llms.md) creates one. It can contain:

- `url`: the site’s address. It defaults to the first `URL` in `DESCRIPTION` that isn’t a source repository.

- `reference`: the sections of the function reference index and the topics each lists, by name or with selectors such as `starts_with()`. The format is based on pkgdown’s.

- `navbar`: the items on the navbar’s `left` and `right`: keywords such as `reference` and `articles`, vignettes, and Quarto navbar items and menus; and the icons in its `tools`, by default `repo`. With `style: sidebar`, the `left` items go in a sidebar instead, with a link to every reference page.

- `home`: `file`, the `.md` or `.qmd` file the home page shows instead of `README.md`; `sidebar`, the sidebar’s `structure` (its sections, in order) and `components` (sections of your own), or `false` to show the page as written, without it; and `links`, added to the sidebar’s links.

- `authors`: by a person’s name, an `href` to link it and `html` to replace it; and under `sidebar` and `footer`, the `roles` shown, with `before`/`after` text in the sidebar and `text` in the footer.

- `repo`: the repository’s `url`s (`home`, and the prefixes of links to a file, `source`; an issue, `issue`; and a user, `user`) and `branch`. They default to those of DESCRIPTION’s GitHub, GitLab or Codeberg `URL`.

- `theme`: the `light` and `dark` themes: names of Quarto’s built-in themes, such as `flatly` and `darkly`, or `.scss` files in `_quartodown/`. By default, `default` and a dark version of it.

- `redirects`: pairs of paths, from an old page to the one replacing it, such as `["reference/old.html", "reference/new.html"]`.

- `development`: `mode`, `release` (the default) to build the site into `docs/`, or `devel` to build a site for the development version into `docs/dev/`, next to the released version’s; `unreleased` marks the version as unreleased, and `auto` picks from the version number and whether the package is on CRAN. The environment variable `QUARTODOWN_DEV_MODE` overrides it. The format is based on pkgdown’s. Content in `.quartodown-devel` or `.quartodown-release` shows on only that site.

- `vignettes`: `files`, files and folders outside `vignettes/` that the vignettes use by a relative path, such as `man/rmd/` for a child document `../man/rmd/shared.Rmd`, copied with them.

- `logo`: `false` to leave the logo, `man/figures/logo.svg` or `.png` (and in light mode `logo-light`, if it exists), off the pages’ titles.

- `links`: `external`, what references to other packages, in code and in Rd links, link to: `sites` (the default; only the packages’ own sites), `all` (also rdrr.io and CRAN) or `none` (nothing outside the package).

- `quarto`: settings merged over the generated Quarto configuration.

`_quartodown/extra.scss` and `_quartodown/extra.css`, if they exist, style every page, on top of quartodown’s theme. The whole `_quartodown/` folder is copied into the Quarto project, so `quarto` settings can refer to other files in it by the same path.

See `vignette("customizing", package = "quartodown")` for the details.

## Examples

``` r
use_quartodown()
build_site()
```
