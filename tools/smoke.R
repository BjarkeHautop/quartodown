# Builds the sites of real packages with this checkout of quartodown, to catch
# what the tests' throwaway packages don't exercise. Each package is cloned
# from GitHub, its _pkgdown.yml (if any) copied to _quartodown/_quartodown.yml
# without the fields quartodown doesn't read, and its site built in a fresh R
# process. Then the built pages are checked (see check_site()).
#
# Run from the package root, with Quarto and git installed:
#
#   Rscript tools/smoke.R                 # every package below
#   Rscript tools/smoke.R rlang cli       # only these
#   Rscript tools/smoke.R tidyverse/glue  # any GitHub repo
#   Rscript tools/smoke.R --no-deps rlang # skip installing dependencies
#   Rscript tools/smoke.R --preview rlang # then serve the results to browse
#   Rscript tools/smoke.R --no-build --preview  # browse the last results
#
# Clones, a library of the packages' dependencies (kept apart from yours) and
# the results are kept in SMOKE_DIR (default: quartodown's user cache folder),
# so later runs only update them. Per package, <pkg>.log holds the build's
# output and <pkg>-issues.txt what each check found; summary.csv (one row per
# package ever run) and index.html, the page --preview opens, sum them up.

packages <- c(
  # Lifecycle badges, Rd macros, `@examplesIf`, many internal topics.
  rlang = "r-lib/rlang",
  # ANSI output in examples, `\Sexpr`s.
  cli = "r-lib/cli",
  purrr = "tidyverse/purrr",
  dplyr = "tidyverse/dplyr",
  # Many plotting examples and articles.
  ggplot2 = "tidyverse/ggplot2",
  # R6 classes.
  R6 = "r-lib/R6",
  mlr3 = "mlr-org/mlr3",
  # Hand-written Rd.
  data.table = "Rdatatable/data.table",
  # htmlwidgets in examples.
  leaflet = "rstudio/leaflet",
  testthat = "r-lib/testthat"
)

smoke_dir <- Sys.getenv(
  "SMOKE_DIR",
  file.path(tools::R_user_dir("quartodown", "cache"), "smoke")
)
lib <- file.path(smoke_dir, "library")

# pkgdown's top-level fields that quartodown reads too; the others (template:,
# articles:, ...) are dropped rather than warned about, as are pkgdown's
# navbar:, whose structure differs, and theme: (pkgdown's syntax highlighting).
config_fields <- c(
  "url",
  "reference",
  "home",
  "authors",
  "repo",
  "redirects",
  "development"
)
home_fields <- c("file", "sidebar", "links")

# The checks, in the order of summary.csv's columns, with a description for
# index.html. "unlinked_calls" is the percentage of function calls in code
# blocks that link nowhere; some are expected (local functions), so it's
# informative rather than a failure, as is "quarto_examples" (reference pages
# run_examples() left for Quarto, which is slower).
checks <- c(
  broken_links = "Relative links to a missing file or anchor",
  missing_tooltips = "Calls and inline code naming an own topic, with no tooltip",
  missing_pages = "Topics and vignettes without a page",
  rd_leftovers = "Rd markup, raw HTML or double escapes in the text",
  empty_sections = "Sections with only a heading",
  ansi_leftovers = "ANSI codes not converted to HTML",
  example_errors = "Errors in reference pages' example output",
  quarto_examples = "Reference pages whose examples Quarto ran",
  unlinked_calls = "% of function calls in code blocks without a link"
)

clone <- function(repo, dir) {
  git <- function(...) {
    status <- system2("git", c("-C", shQuote(dir), ...))
    if (status != 0) {
      stop("git ", paste(c(...), collapse = " "), " failed", call. = FALSE)
    }
  }
  if (dir.exists(dir)) {
    git("fetch", "--quiet", "--depth", "1", "origin")
    git("reset", "--quiet", "--hard", "FETCH_HEAD")
    git("clean", "--quiet", "-fdx")
  } else {
    dir.create(dirname(dir), recursive = TRUE, showWarnings = FALSE)
    url <- paste0("https://github.com/", repo, ".git")
    status <- system2(
      "git",
      c("clone", "--quiet", "--depth", "1", url, shQuote(dir))
    )
    if (status != 0) {
      stop("Can't clone ", url, call. = FALSE)
    }
  }
}

pkgdown_config <- function(dir) {
  files <- file.path(
    dir,
    c(
      "_pkgdown.yml",
      "_pkgdown.yaml",
      "pkgdown/_pkgdown.yml",
      "pkgdown/_pkgdown.yaml"
    )
  )
  files <- files[file.exists(files)]
  if (length(files) == 0) {
    return(NULL)
  }
  config <- yaml::read_yaml(files[[1]])
  config <- config[intersect(names(config), config_fields)]
  if (is.list(config$home)) {
    config$home <- config$home[intersect(names(config$home), home_fields)]
  }
  if (length(config$home) == 0) {
    config$home <- NULL
  }
  config
}

write_config <- function(dir) {
  config <- pkgdown_config(dir)
  if (length(config) == 0) {
    return(invisible())
  }
  dir.create(file.path(dir, "_quartodown"), showWarnings = FALSE)
  yaml::write_yaml(config, file.path(dir, "_quartodown", "_quartodown.yml"))
}

install_deps <- function(dir) {
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  pak::local_install_deps(
    dir,
    lib = lib,
    dependencies = c("all", "Config/Needs/website"),
    ask = FALSE
  )
}

# Builds the site in a fresh R process (so packages don't share state), with
# its output in `log`. Returns `error`, the error message or NA, and
# `examples`, run_examples()'s status of each reference page ("done", or why
# Quarto ran it), NULL if it ran none.
build <- function(dir, log) {
  callr::r(
    function(quartodown, dir) {
      pkgload::load_all(quartodown, quiet = TRUE)
      trace(
        "run_examples",
        exit = quote(assign("smoke_examples", returnValue(), globalenv())),
        where = asNamespace("quartodown"),
        print = FALSE
      )
      error <- tryCatch(
        {
          build_site(dir, quiet = FALSE, preview = FALSE)
          NA_character_
        },
        error = function(e) conditionMessage(e)
      )
      list(error = error, examples = get0("smoke_examples", globalenv()))
    },
    args = list(quartodown = normalizePath("."), dir = dir),
    libpath = c(lib, .libPaths()),
    # The site goes to docs/, which the checks read.
    env = c(callr::rcmd_safe_env(), QUARTODOWN_DEV_MODE = "release"),
    stdout = log,
    stderr = "2>&1"
  )
}

# ---- checks ---------------------------------------------------------------------
#
# Each returns its findings as "page: detail" lines.

findings <- function(page, details) {
  if (length(details) > 0) paste0(page, ": ", details) else character()
}

# The relative links (href/src) of `pages` whose file, or anchor in that
# file, doesn't exist.
check_links <- function(docs, pages) {
  ids <- new.env()
  page_ids <- function(file) {
    if (is.null(ids[[file]])) {
      html <- xml2::read_html(file)
      ids[[file]] <- c(
        xml2::xml_attr(xml2::xml_find_all(html, "//*[@id]"), "id"),
        xml2::xml_attr(xml2::xml_find_all(html, "//a[@name]"), "name")
      )
    }
    ids[[file]]
  }
  broken <- character()
  for (page in names(pages)) {
    file <- file.path(docs, page)
    html <- pages[[page]]
    links <- c(
      xml2::xml_attr(xml2::xml_find_all(html, "//a[@href]"), "href"),
      xml2::xml_attr(xml2::xml_find_all(html, "//link[@href]"), "href"),
      xml2::xml_attr(xml2::xml_find_all(html, "//img[@src]"), "src"),
      xml2::xml_attr(xml2::xml_find_all(html, "//script[@src]"), "src")
    )
    links <- unique(links[!grepl("^([a-zA-Z][a-zA-Z0-9+.-]*:|//)", links)])
    for (link in links) {
      target <- sub("[?#].*", "", link)
      anchor <- if (grepl("#", link)) sub("^[^#]*#", "", link)
      target <- if (nzchar(target)) {
        file.path(dirname(file), utils::URLdecode(target))
      } else {
        file
      }
      if (dir.exists(target)) {
        target <- file.path(target, "index.html")
      }
      ok <- file.exists(target) &&
        (is.null(anchor) ||
          !nzchar(anchor) ||
          !grepl("\\.html$", target) ||
          utils::URLdecode(anchor) %in% page_ids(target))
      if (!ok) {
        broken <- c(broken, paste0(page, ": ", link))
      }
    }
  }
  broken
}

has_tooltip <- function(nodes) {
  !is.na(xml2::xml_find_first(nodes, ".//a[@class='quartodown-tip-ref']"))
}

# The highlighting classes of code block tokens that may name a function:
# besides `fu`, Pandoc's R syntax puts some (e.g. R6Class) in `kw`.
code_span <- paste0(
  "@class='",
  c("fu", "kw", "va", "bu", "cf", "ex", "ot"),
  "'",
  collapse = " or "
)

# Names in code blocks and inline code (`foo()`, `pkg::foo`, `foo`) naming
# one of `aliases`, the own topics with a page, without a tooltip link.
# Independent of add_tooltips_html()'s regex, so also catches tokens it
# doesn't look at.
check_tooltips <- function(pages, aliases, pkg) {
  out <- character()
  strip <- function(x) {
    x <- sub("\\(.*$", "", x)
    x <- sub(paste0("^", pkg, ":::?"), "", x)
    sub("^`(.*)`$", "\\1", x)
  }
  for (page in names(pages)) {
    main <- xml2::xml_find_all(pages[[page]], "//main")
    nodes <- xml2::xml_find_all(
      main,
      paste0(
        ".//pre//span[",
        code_span,
        "] | ",
        ".//code[not(ancestor::pre) and not(.//span)]"
      )
    )
    called <- strip(xml2::xml_text(nodes))
    missing <- called %in% aliases & !has_tooltip(nodes)
    if (any(missing)) {
      counts <- table(called[missing])
      out <- c(
        out,
        paste0(page, ": ", names(counts), " (", as.integer(counts), "x)")
      )
    }
  }
  out
}

# Calls in code blocks that link nowhere, as the percentage of all calls
# (attribute "percent") and the most common names.
check_unlinked <- function(pages) {
  unlinked <- character()
  total <- 0
  for (html in pages) {
    calls <- xml2::xml_find_all(html, "//main//pre//span[@class='fu']")
    total <- total + length(calls)
    linked <- !is.na(xml2::xml_find_first(calls, ".//a"))
    unlinked <- c(unlinked, xml2::xml_text(calls[!linked]))
  }
  counts <- sort(table(unlinked), decreasing = TRUE)
  out <- paste0(names(counts), " (", as.integer(counts), "x)")
  attr(out, "percent") <- if (total > 0) round(100 * length(unlinked) / total)
  out
}

# Topics that should have a page (not internal, or listed; see has_page()) and
# vignettes without one.
check_pages <- function(dir, docs) {
  config <- suppressWarnings(read_config(dir))
  topics <- rd_topics(dir)
  expected <- vapply(topics, `[[`, character(1), "basename")
  expected <- paste0("reference/", expected[has_page(topics, config)], ".html")
  vignettes <- articles_rel_paths(dir)
  expected <- c(expected, paste0("vignettes/", vignettes, ".html"))
  missing <- expected[!file.exists(file.path(docs, expected))]
  if (length(missing) > 0) paste0(missing, ": missing")
}

articles_rel_paths <- function(dir) {
  files <- list.files(
    file.path(dir, "vignettes"),
    "\\.([Rr]md|qmd|md)$",
    recursive = TRUE
  )
  files <- files[!grepl("(^|/)[_.]", files)]
  sub("\\.[^.]+$", "", files)
}

# Text outside code that still holds Rd markup, HTML tags or escaped entities.
check_rd_leftovers <- function(pages) {
  pattern <- paste0(
    "\\\\(code|link|linkS4class|emph|strong|bold|eqn|deqn|item|itemize|",
    "enumerate|describe|value|pkg|sQuote|dQuote|href|url|var|env|option|",
    "file|samp|verb|method|S4method|dots|ldots|cr|Sexpr|figure)\\b",
    "|</?(code|pre|em|strong|p|a|span|div|dl|dt|dd|ul|ol|li|table|tr|td|br)\\b[^>]*>",
    "|&(lt|gt|amp|quot|#[0-9]+);"
  )
  out <- character()
  for (page in names(pages)) {
    text <- xml2::xml_text(xml2::xml_find_all(
      pages[[page]],
      paste0(
        "//main//text()[not(ancestor::pre) and not(ancestor::code) and ",
        "not(ancestor::script) and not(ancestor::style) and ",
        "not(ancestor::*[contains(@class, 'math')])]"
      )
    ))
    hits <- text[grepl(pattern, text, perl = TRUE)]
    out <- c(out, findings(page, substr(trimws(hits), 1, 80)))
  }
  out
}

check_empty_sections <- function(pages) {
  out <- character()
  for (page in names(pages)) {
    sections <- xml2::xml_find_all(
      pages[[page]],
      "//main//section[contains(@class, 'level2') or contains(@class, 'level3')]"
    )
    for (section in sections) {
      heading <- xml2::xml_find_first(section, "./*[self::h2 or self::h3]")
      text <- trimws(sub(
        xml2::xml_text(heading),
        "",
        xml2::xml_text(section),
        fixed = TRUE
      ))
      media <- xml2::xml_find_first(
        section,
        ".//*[self::img or self::svg or self::iframe]"
      )
      if (!nzchar(text) && is.na(media)) {
        out <- c(out, paste0(page, ": ", xml2::xml_text(heading)))
      }
    }
  }
  out
}

# ANSI escapes (or protect_ansi()'s placeholder) left in the pages or the
# search index. xml2 drops the ESC itself, so "[31m" is what's left of one.
check_ansi <- function(docs, pages) {
  pattern <- "\033||\\[(?:[0-9]{1,3};)*(?:3[0-9]|9[0-7]|1|22|39|49)m"
  out <- character()
  for (page in names(pages)) {
    text <- xml2::xml_text(xml2::xml_find_all(pages[[page]], "//main"))
    if (any(grepl(pattern, text, perl = TRUE))) {
      out <- c(out, paste0(page, ": ANSI codes in the text"))
    }
  }
  search <- file.path(docs, "search.json")
  if (file.exists(search)) {
    text <- readLines(search, warn = FALSE, encoding = "UTF-8")
    if (any(grepl(pattern, text, perl = TRUE))) {
      out <- c(out, "search.json: ANSI codes")
    }
  }
  out
}

check_example_errors <- function(pages) {
  out <- character()
  for (page in grep("^reference/", names(pages), value = TRUE)) {
    errors <- xml2::xml_find_all(
      pages[[page]],
      "//main//*[contains(@class, 'cell-output-error')]"
    )
    out <- c(out, findings(page, substr(trimws(xml2::xml_text(errors)), 1, 80)))
  }
  out
}

# Reference pages run_examples() left for Quarto, with why.
check_quarto_examples <- function(examples) {
  left <- examples[examples != "done"]
  if (length(left) > 0) paste0("reference/", names(left), ": ", left)
}

# Every check's findings, named as `checks`.
check_site <- function(dir, examples) {
  docs <- file.path(dir, "docs")
  files <- list.files(docs, "\\.html$", recursive = TRUE)
  files <- files[!startsWith(files, "dev/")]
  pages <- lapply(file.path(docs, files), xml2::read_html)
  names(pages) <- files
  pkg <- desc::desc_get_field("Package", file = dir)
  topics <- rd_topics(dir)
  config <- suppressWarnings(read_config(dir))
  aliases <- unlist(lapply(topics[has_page(topics, config)], `[[`, "alias"))

  list(
    broken_links = check_links(docs, pages),
    missing_tooltips = check_tooltips(pages, aliases, pkg),
    missing_pages = check_pages(dir, docs),
    rd_leftovers = check_rd_leftovers(pages),
    empty_sections = check_empty_sections(pages),
    ansi_leftovers = check_ansi(docs, pages),
    example_errors = check_example_errors(pages),
    quarto_examples = check_quarto_examples(examples),
    unlinked_calls = check_unlinked(pages)
  )
}

write_issues <- function(found, file) {
  lines <- unlist(lapply(names(checks), function(check) {
    x <- found[[check]]
    if (length(x) == 0) {
      return(NULL)
    }
    c(paste0("## ", checks[[check]], " (", length(x), ")"), x, "")
  }))
  writeLines(if (length(lines) > 0) lines else "No issues found.", file)
}

# ---- running ------------------------------------------------------------------

empty_row <- function(name, error) {
  row <- data.frame(package = name, ok = FALSE, seconds = NA, warnings = NA)
  row[names(checks)] <- NA
  row$error <- gsub("\\s+", " ", error)
  row
}

smoke <- function(name, repo, deps = TRUE) {
  dir <- file.path(smoke_dir, "packages", name)
  log <- file.path(smoke_dir, paste0(name, ".log"))
  issues <- file.path(smoke_dir, paste0(name, "-issues.txt"))
  unlink(issues)
  message("== ", name, " (", repo, ")")
  clone(repo, dir)
  write_config(dir)
  if (deps) {
    install_deps(dir)
  }
  start <- Sys.time()
  result <- build(dir, log)
  seconds <- round(as.numeric(Sys.time() - start, units = "secs"))
  output <- cli::ansi_strip(readLines(log, warn = FALSE))
  warnings <- sum(grepl("^(WARN|Warning)", output))
  if (!is.na(result$error)) {
    message("   FAILED: ", result$error, " (see ", log, ")")
    row <- empty_row(name, result$error)
    row$seconds <- seconds
    row$warnings <- warnings
    return(row)
  }

  found <- check_site(dir, result$examples)
  write_issues(found, issues)
  counts <- lengths(found)
  counts[["unlinked_calls"]] <- NA
  percent <- attr(found$unlinked_calls, "percent")
  if (!is.null(percent)) {
    counts[["unlinked_calls"]] <- percent
  }
  message(
    sprintf("   ok (%ds, %d warnings) ", seconds, warnings),
    paste(names(counts), counts, sep = ": ", collapse = ", ")
  )
  row <- data.frame(
    package = name,
    ok = TRUE,
    seconds = seconds,
    warnings = warnings
  )
  row[names(counts)] <- as.list(counts)
  row$error <- ""
  row
}

# The page --preview opens: the summary, with links to each site, its log and
# its issues.
write_index <- function(results) {
  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  link <- function(href, text, ok = file.exists(file.path(smoke_dir, href))) {
    ifelse(ok, sprintf('<a href="%s">%s</a>', href, text), "")
  }
  cols <- c("seconds", "warnings", names(checks))
  header <- paste0(
    "<tr><th>Package</th><th></th>",
    paste0(
      "<th title=\"",
      c("", "", checks),
      "\">",
      cols,
      "</th>",
      collapse = ""
    ),
    "<th>Error</th></tr>"
  )
  rows <- vapply(
    seq_len(nrow(results)),
    function(i) {
      r <- results[i, ]
      cells <- vapply(
        cols,
        function(col) {
          value <- r[[col]]
          flag <- col %in% names(checks)[1:7] && !is.na(value) && value > 0
          sprintf(
            "<td%s>%s</td>",
            if (flag) " class=\"bad\"" else "",
            if (is.na(value)) "" else value
          )
        },
        character(1)
      )
      paste0(
        "<tr",
        if (!isTRUE(r$ok)) " class=\"failed\"",
        "><td>",
        link(paste0("packages/", r$package, "/docs/index.html"), r$package),
        "</td><td>",
        link(paste0(r$package, "-issues.txt"), "issues"),
        " ",
        link(paste0(r$package, ".log"), "log"),
        "</td>",
        paste(cells, collapse = ""),
        "<td>",
        esc(r$error),
        "</td></tr>"
      )
    },
    character(1)
  )
  legend <- paste0(
    "<dt>",
    names(checks),
    "</dt><dd>",
    checks,
    "</dd>",
    collapse = ""
  )
  writeLines(
    c(
      "<!doctype html><meta charset=\"utf-8\"><title>quartodown smoke test</title>",
      "<style>body{font-family:system-ui,sans-serif;margin:2rem}",
      "table{border-collapse:collapse}td,th{padding:.3rem .6rem;border-bottom:1px solid #ddd;text-align:left}",
      ".bad{background:#fde2e1}.failed{background:#fbd0cd}dt{font-family:monospace;margin-top:.4rem}</style>",
      "<h1>quartodown smoke test</h1><table>",
      header,
      rows,
      "</table>",
      "<h2>Checks</h2><dl>",
      legend,
      "</dl>"
    ),
    file.path(smoke_dir, "index.html")
  )
}

args <- commandArgs(trailingOnly = TRUE)
flags <- c("--no-deps", "--no-build", "--preview")
deps <- !"--no-deps" %in% args
run <- !"--no-build" %in% args
preview <- "--preview" %in% args
args <- setdiff(args, flags)
if (run && length(args) == 0) {
  args <- names(packages)
}
repos <- ifelse(args %in% names(packages), packages[args], args)
pkg_names <- ifelse(args %in% names(packages), args, basename(args))
unknown <- !grepl("/", repos)
if (any(unknown)) {
  stop(
    "Unknown package ",
    paste(args[unknown], collapse = ", "),
    "; give its GitHub repo as owner/repo.",
    call. = FALSE
  )
}

dir.create(smoke_dir, recursive = TRUE, showWarnings = FALSE)
summary_file <- file.path(smoke_dir, "summary.csv")
pkgload::load_all(".", quiet = TRUE)
# For check_site()'s rd_topics(), which reads the packages' Rd macros.
.libPaths(c(lib, .libPaths()))

if (run) {
  results <- do.call(
    rbind,
    Map(
      function(name, repo) {
        tryCatch(
          smoke(name, repo, deps),
          error = function(e) {
            message("   FAILED: ", conditionMessage(e))
            empty_row(name, conditionMessage(e))
          }
        )
      },
      pkg_names,
      repos
    )
  )
  rownames(results) <- NULL
  all <- results
  previous <- if (file.exists(summary_file)) utils::read.csv(summary_file)
  # Unless written by a version of this script with other checks.
  if (all(names(results) %in% names(previous))) {
    previous <- previous[!previous$package %in% results$package, names(results)]
    all <- rbind(previous, results)
  }
  all <- all[order(all$package), ]
  utils::write.csv(all, summary_file, row.names = FALSE)
  write_index(all)
  print(results[names(results) != "error"], row.names = FALSE)
  message("Logs and issues in ", smoke_dir)
}

if (preview) {
  if (!file.exists(file.path(smoke_dir, "index.html"))) {
    stop("No results to preview in ", smoke_dir, call. = FALSE)
  }
  message("Serving ", smoke_dir, "; press Ctrl+C (Esc in RStudio) to stop.")
  servr::httd(smoke_dir, daemon = FALSE, browser = TRUE)
} else if (run && !all(results$ok)) {
  quit(status = 1)
}
