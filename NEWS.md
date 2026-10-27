# quartodown (development version)

- New `development: mode:` setting in `_quartodown.yml`, as in pkgdown: with
  `devel` (or `auto` and a `.9000` version), the site for the development
  version is built into `docs/dev/`, next to the released version's. A link next
  to the navbar's title switches between them. `auto` keeps a package that isn't
  on CRAN yet in `docs/`, as unreleased. Content marked `.quartodown-devel` or
  `.quartodown-release` is shown on only that site. `use_quartodown()` sets
  `mode: auto`.

- New `workers` argument to `build_site()`, to build the site with several
  processes. `use_quartodown_action()`'s workflow uses 4.

- The reference pages' examples now run before rendering, all in one R process,
  rather than Quarto starting one for each page, which made most of the build's
  time for packages with many examples.

- The package's logo, `man/figures/logo.svg` or `.png` (`.svg` if both exist),
  now shows right of the title of every page, not just the home page, with
  `logo-light.svg` or `.png` in light mode if it exists. `logo: false` in
  `_quartodown.yml` turns it off.

- `build_site()` reports its progress in a few steps (installing the package,
  writing the Quarto project, rendering, linking code), each with the time it
  took, and where it built the site. When a page fails to render with
  `quiet = TRUE`, Quarto's error is now shown rather than a suggestion to rerun
  with `quiet = FALSE`.

- The changelog page is now titled "Changelog", with its versions' headings a
  level down, and links its source, `NEWS.md`.

- The search box's magnifying glass and clear button are now readable in dark
  mode.

# quartodown 0.1.0

- Initial release.
