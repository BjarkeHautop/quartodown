# An empty cache, and a pkgdown.yml for each site in `sites` (url -> yml),
# so no test downloads anything: any other site has none.
local_sites <- function(sites = list(), env = parent.frame()) {
  withr::local_options(
    quartodown.cache_dir = withr::local_tempdir(.local_envir = env),
    .local_envir = env
  )
  rm(list = ls(link_memo), envir = link_memo)
  withr::defer(rm(list = ls(link_memo), envir = link_memo), envir = env)
  fetched <- new.env()
  fetched$urls <- character()
  local_mocked_bindings(
    fetch_pkgdown_yml = function(url) {
      fetched$urls <- c(fetched$urls, url)
      list(yml = sites[[url]], failed = FALSE)
    },
    .env = env
  )
  fetched
}

cli_site <- list(
  "https://cli.r-lib.org" = list(
    urls = list(
      reference = "https://cli.r-lib.org/reference",
      article = "https://cli.r-lib.org/articles"
    ),
    articles = list(progress = "progress.html")
  )
)
