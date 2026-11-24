# Set up a GitHub Actions workflow that publishes the site

Source: [`R/use_quartodown_action.R`](https://github.com/bjarkehautop/quartodown/blob/HEAD/R/use_quartodown_action.R)

## Description

Writes `.github/workflows/quartodown.yaml`, a GitHub Actions workflow that builds the site and deploys it to the `gh-pages` branch. It runs [`build_site()`](build_site.llms.md) on every push, pull request and release, but only deploys when it isn’t a pull request. Also adds `^\.github$` to `.Rbuildignore` if it’s not already there.

## Usage

``` r
use_quartodown_action(path = ".", overwrite = FALSE)
```

## Arguments

|  |  |
|----|----|
| `path` | Path to the target package root. Defaults to the current directory. |
| `overwrite` | Logical. Replace an existing `.github/workflows/quartodown.yaml`? By default it’s left as is. |

## Details

Once the workflow’s first run has created the `gh-pages` branch, go to the *Pages* section of the repository’s settings on GitHub and choose to deploy from that branch.

## Value

Invisibly, the path to the workflow file.

## Examples

``` r
pkg <- tempfile("examplepkg")
dir.create(pkg)
write(
  "Package: examplepkg\nVersion: 0.0.0.9000",
  file.path(pkg, "DESCRIPTION")
)
use_quartodown_action(pkg)
#> ✔ Wrote /tmp/RtmpZGp9hp/examplepkg191d33710aed/.github/workflows/quartodown.yaml.
#> ✔ Added `^\.github$` to /tmp/RtmpZGp9hp/examplepkg191d33710aed/.Rbuildignore.
#> 
#> Next steps:
#> 1. Commit and push the workflow.
#> 2. Once its first run has created the `gh-pages` branch, go to the Pages
#> section of the repository's settings on GitHub and choose to deploy from that
#> branch.
list.files(pkg, recursive = TRUE, all.files = TRUE)
#> [1] ".github/workflows/quartodown.yaml" ".Rbuildignore"                    
#> [3] "DESCRIPTION"
unlink(pkg, recursive = TRUE)
```
