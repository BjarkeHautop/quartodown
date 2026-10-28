# Runs the code of reference pages in this one R process, so Quarto doesn't
# start one per page (see run_examples() in R/examples.R). The argument is
# an RDS file with `pages` (paths of .qmd files), `opts_chunk` and
# `opts_knit`. Each page that knits is overwritten with the Markdown Quarto's
# knitr engine would have made of it; its name and "done" are then appended
# to `status`, else its name and why it was left for Quarto to run.
#
# The hooks follow those of Quarto's knitr engine (share/rmd/hooks.R) for the
# options reference pages use, so the HTML comes out the same.
#
# Everything is kept in local() so the examples, run in the global
# environment as Quarto does, see none of it.

local({
  job <- readRDS(commandArgs(TRUE)[[1]])
  report <- function(page, status) {
    cat(
      basename(page),
      "\t",
      status,
      "\n",
      sep = "",
      file = job$status,
      append = TRUE
    )
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    for (page in job$pages) {
      report(page, "knitr isn't installed")
    }
    return(invisible())
  }

  md_hooks <- knitr::hooks_markdown()

  # Enough backticks to fence `x`.
  fence <- function(x) {
    runs <- regmatches(x, gregexpr("`{3,}", x))[[1]]
    strrep("`", max(3L, nchar(runs) + 1L))
  }
  output_div <- function(x, classes) {
    paste0(
      "::: {",
      paste(paste0(".", classes), collapse = " "),
      "}\n",
      x,
      "\n:::\n\n"
    )
  }
  output_hook <- function(type, classes) {
    function(x, options) {
      x <- md_hooks[[type]](x, options)
      if (identical(options$results, "asis") || isTRUE(options$collapse)) {
        x
      } else {
        output_div(x, c("cell-output", paste0("cell-output-", classes)))
      }
    }
  }
  hooks <- list(
    chunk = function(x, options) {
      x <- md_hooks$chunk(x, options)
      if (identical(options$results, "asis")) {
        return(x)
      }
      indent <- if (is.null(options$indent)) "" else options$indent
      paste0("\n", indent, "::: {.cell}\n", x, "\n", indent, ":::\n")
    },
    source = function(x, options) {
      x <- paste(c("", x), collapse = "\n")
      ticks <- fence(x)
      paste0("\n\n", ticks, "{.r .cell-code}", x, "\n", ticks, "\n\n")
    },
    output = output_hook("output", "stdout"),
    warning = output_hook("warning", "stderr"),
    message = output_hook("message", "stderr"),
    error = output_hook("error", "error"),
    plot = function(x, options) {
      attr <- character()
      if (
        !is.null(options$fig.align) && !identical(options$fig.align, "default")
      ) {
        attr <- c(attr, sprintf("fig-align='%s'", options$fig.align))
      }
      if (!is.null(options$fig.alt) && nzchar(options$fig.alt)) {
        attr <- c(
          attr,
          sprintf(
            "fig-alt='%s'",
            gsub("'", "\\'", options$fig.alt, fixed = TRUE)
          )
        )
      }
      attr <- paste(
        c(
          attr,
          sprintf("width=%s", options$out.width),
          sprintf("height=%s", options$out.height),
          options$out.extra
        ),
        collapse = " "
      )
      attr <- if (nzchar(attr)) paste0("{", trimws(attr), "}") else ""
      cap <- if (length(options$fig.cap)) options$fig.cap else ""
      md <- sprintf("![%s](%s)%s", cap, x, attr)
      if (identical(options$results, "asis")) {
        md
      } else {
        output_div(md, "cell-output-display")
      }
    }
  )

  # What each page is reset to, more than `R CMD check` does between topics.
  # Options a page adds stay, as loading a namespace adds them too, and the
  # package would break without them. Environment variables aren't reset, as
  # packages may change them when loading too (e.g. PATH). Loaded namespaces,
  # and state kept inside packages, can't be reset either.
  base <- list(
    options = options(),
    search = search(),
    wd = getwd(),
    rng = RNGkind()
  )
  reset <- function() {
    rm(list = ls(globalenv(), all.names = TRUE), envir = globalenv())
    grDevices::graphics.off()
    for (item in setdiff(search(), base$search)) {
      try(detach(item, character.only = TRUE), silent = TRUE)
    }
    current <- options()
    for (name in names(base$options)) {
      if (!identical(current[[name]], base$options[[name]])) {
        try(
          options(stats::setNames(list(base$options[[name]]), name)),
          silent = TRUE
        )
      }
    }
    setwd(base$wd)
    do.call(RNGkind, as.list(base$rng))
  }

  # "done", or why the page is left for Quarto.
  knit_page <- function(page) {
    setwd(dirname(page))
    stem <- tools::file_path_sans_ext(basename(page))
    # The names rmarkdown::render() gives them for Quarto.
    input <- paste0(stem, ".rmarkdown")
    output <- paste0(stem, ".knit.md")
    figures <- paste0(stem, "_files")
    on.exit(unlink(c(input, output)), add = TRUE)
    file.copy(basename(page), input, overwrite = TRUE)

    knitr::opts_knit$restore()
    knitr::opts_chunk$restore()
    knitr::knit_hooks$restore()
    knitr::render_markdown()
    knitr::opts_knit$set(job$opts_knit)
    knitr::opts_chunk$set(c(
      list(fig.path = paste0(figures, "/figure-html/")),
      job$opts_chunk
    ))
    knitr::knit_hooks$set(hooks)
    invisible(knitr::knit_meta(clean = TRUE))

    result <- tryCatch(
      {
        knitr::knit(
          input,
          output,
          quiet = TRUE,
          envir = globalenv(),
          encoding = "UTF-8"
        )
        NULL
      },
      error = function(e) paste("error:", conditionMessage(e))
    )
    # HTML dependencies (htmlwidgets) and preserved raw HTML need Quarto's
    # handling, as does an error, which Quarto then reports.
    if (is.null(result) && length(knitr::knit_meta(clean = TRUE)) > 0) {
      result <- "HTML dependencies"
    }
    lines <- if (is.null(result)) {
      readLines(output, warn = FALSE, encoding = "UTF-8")
    }
    if (
      is.null(result) && any(grepl("<!--html_preserve-->", lines, fixed = TRUE))
    ) {
      result <- "preserved HTML"
    }
    if (!is.null(result)) {
      unlink(figures, recursive = TRUE)
      return(result)
    }
    writeLines(enc2utf8(lines), basename(page), useBytes = TRUE)
    "done"
  }

  for (page in job$pages) {
    status <- tryCatch(knit_page(page), error = function(e) {
      paste("error:", conditionMessage(e))
    })
    report(page, gsub("[\r\n\t]+", " ", status))
    reset()
  }
})
