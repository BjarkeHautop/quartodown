# Preview the built site

Source: [`R/project.R`](https://github.com/bjarkehautop/quartodown/blob/HEAD/R/project.R)

## Description

Serves `docs/` (or `docs/dev/`, for a development site; see the Configuration section of [`build_site()`](build_site.llms.md)), as built by [`build_site()`](build_site.llms.md), in a web browser. The server keeps running for the rest of the R session and is reused by later calls, including [`build_site()`](build_site.llms.md)’s (which, with a server already running, leaves the open page to reload instead of opening another). It keeps serving the previous site while [`build_site()`](build_site.llms.md) runs. An open page reloads if the new build changed it.

## Usage

``` r
preview_site(path = ".")
```

## Arguments

|        |                           |
|--------|---------------------------|
| `path` | Path to the package root. |

## Value

The site’s local URL, invisibly.

## Examples

``` r
build_site()
preview_site()
```
