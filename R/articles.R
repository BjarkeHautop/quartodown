# ---- vignettes in the navbar --------------------------------------------------
#
# Vignettes are identified by their path relative to vignettes/, e.g.
# "getting-started.qmd" or "articles/technical.qmd". Top-level vignettes and
# those in subfolders (such as vignettes/articles/) are all rendered.

vignette_pattern <- "\\.(qmd|[Rr]md)$"

# Top-level vignettes first, then those in subfolders, each sorted. Like
# Quarto, skips files and folders whose name starts with `_` or `.`.
find_vignettes <- function(path) {
  files <- list.files(
    file.path(path, "vignettes"),
    pattern = vignette_pattern,
    recursive = TRUE
  )
  files <- files[!grepl("(^|/)[_.]", files)]
  top <- dirname(files) == "."
  c(sort(files[top]), sort(files[!top]))
}

# The top-level vignette named after the package, or the only top-level
# vignette if there's just one. NULL if neither exists.
find_getting_started_vignette <- function(
  path,
  vignettes = find_vignettes(path)
) {
  top <- vignettes[dirname(vignettes) == "."]
  if (length(top) == 1) {
    return(top)
  }

  pkg <- pkg_name(path)
  named <- top[strip_ext(top) == pkg]
  if (length(named) == 1) {
    return(named)
  }

  if (length(top) > 1) {
    cli::cli_inform(c(
      "No \"Get started\" link: found several top-level vignettes and none is named {.file {pkg}.qmd}.",
      "i" = "They're all listed under \"Articles\" instead."
    ))
  }
  NULL
}

strip_ext <- function(x) sub("\\.[^./]*$", "", x)

vignette_title <- function(vignette_path) {
  fallback <- strip_ext(basename(vignette_path))

  lines <- read_utf8(vignette_path)
  front_matter <- split_front_matter(lines)$front_matter
  yaml <- tryCatch(
    yaml::yaml.load(paste(utils::head(front_matter[-1], -1), collapse = "\n")),
    error = function(e) NULL
  )
  title <- if (is.list(yaml)) yaml$title
  if (is_string(title)) title else fallback
}
