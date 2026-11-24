# Set up a package to use quartodown

Source: [`R/use_quartodown.R`](https://github.com/bjarkehautop/quartodown/blob/HEAD/R/use_quartodown.R)

## Description

Writes `_quartodown/_quartodown.yml`, the configuration file [`build_site()`](build_site.llms.md) reads. It starts with a `reference:` field listing every function currently documented in `<path>/man/*.Rd` in one "All functions" section. Also adds the `.gitignore`/`.Rbuildignore` entries the build needs (see Details) if they’re not already there.

## Usage

``` r
use_quartodown(path = ".")
```

## Arguments

|  |  |
|----|----|
| `path` | Path to the target package root. Defaults to the current directory. |

## Details

In a package with a pkgdown configuration (`_pkgdown.yml`, also in `pkgdown/`), it’s translated instead, with any files it needs, such as `_quartodown/extra.scss`, and the settings that couldn’t be translated are listed. See "Coming from pkgdown" in `vignette("customizing", package = "quartodown")`.

An existing `_quartodown.yml`, or file in `_quartodown/`, is never overwritten.

`use_quartodown()` adds the following lines to `.gitignore` and `.Rbuildignore` (only the ones not already present in each file):

    # .gitignore
    _quarto/*
    !_quarto/_freeze/

    # .Rbuildignore
    ^docs$
    ^_quarto$
    ^_quartodown$

`_quarto/` is the Quarto project [`build_site()`](build_site.llms.md) generates and renders into `docs/`. Its `_freeze/` directory holds the results of code run with `build_site(freeze = TRUE)`, which can be committed so later builds, e.g. on CI, reuse them (they aren’t rerun when only the package’s code or its dependencies change, so delete it to refresh them). `_quartodown/` holds the site’s configuration and any files it uses, such as `extra.scss`.

## Value

Invisibly, the path to `_quartodown/_quartodown.yml`.

## Examples

``` r
pkg <- tempfile("examplepkg")
dir.create(pkg)
write(
  "Package: examplepkg\nVersion: 0.0.0.9000",
  file.path(pkg, "DESCRIPTION")
)
use_quartodown(pkg)
#> ✔ Wrote /tmp/RtmphckYhf/examplepkg187e2f1b7dda/_quartodown/_quartodown.yml (edit this to group your functions).
#> ✔ Added `_quarto/*` and `!_quarto/_freeze/` to /tmp/RtmphckYhf/examplepkg187e2f1b7dda/.gitignore.
#> ✔ Added `^docs$`, `^_quarto$`, and `^_quartodown$` to /tmp/RtmphckYhf/examplepkg187e2f1b7dda/.Rbuildignore.
#> 
#> Next steps:
#> 1. Edit _quartodown/_quartodown.yml to group your functions the way you want.
#> 2. Build and preview the site with `quartodown::build_site()`.
readLines(file.path(pkg, "_quartodown", "_quartodown.yml"))
#> [1] "# Configuration for quartodown::build_site(). See"                    
#> [2] "# vignette(\"customizing\", package = \"quartodown\") for all fields."
#> [3] ""                                                                     
#> [4] "reference:"                                                           
#> [5] "  - title: All functions"                                             
#> [6] "    contents:"                                                        
#> [7] "      []"
unlink(pkg, recursive = TRUE)
```
