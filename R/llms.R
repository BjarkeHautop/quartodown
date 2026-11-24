# ---- docs for LLMs ------------------------------------------------------------
#
# With Quarto's `website: llms-txt:` (on by default where Quarto has it, see
# quarto_config()), Quarto writes a Markdown copy of each page next to it,
# `<page>.llms.md`. quartodown cleans those (clean_llms_md()) and writes the
# site's `llms.txt` itself (write_llms_txt()): Quarto's lists every page as
# rendered, in no order, and with `workers > 1` each shard writes its own.

# Quarto 1.9 added `llms-txt`; older versions don't know the key.
quarto_has_llms_txt <- function() {
  version <- tryCatch(quarto::quarto_version(), error = function(e) NULL)
  !is.null(version) && version >= "1.9"
}

# The `.llms.md` pages in `docs`, relative to it.
llms_pages <- function(docs) {
  as.character(fs::path_rel(
    fs::dir_ls(docs, regexp = "\\.llms\\.md$", recurse = TRUE),
    docs
  ))
}

# Quarto converts a page to Markdown before postprocess_pages() turns its
# ANSI codes to HTML (see R/ansi.R), so they're stripped here. A
# `.light-content`/`.dark-content` image pair (theme_pictures_to_images())
# becomes two images with the same alt text; the first is kept.
clean_llms_md <- function(lines) {
  lines <- gsub("\u001b\\[[0-9;]*m", "", lines)
  gsub("(!\\[([^]]*)\\]\\([^)]*\\))!\\[\\2\\]\\([^)]*\\)", "\\1", lines)
}

# `llms.txt` (https://llmstxt.org/) for the `.llms.md` pages in `docs`: the
# home page's text, then links to the vignettes, the reference topics grouped
# as on the index, and the rest under "Optional". Nothing without `.llms.md`
# pages, i.e. with `llms-txt: false` or an older Quarto.
write_llms_txt <- function(
  path,
  config = list(),
  settings = list(),
  topics = rd_topics(path),
  docs = site_dir(path, config)
) {
  pages <- setdiff(llms_pages(docs), "404.llms.md")
  if (length(pages) == 0) {
    return(invisible())
  }
  for (page in pages) {
    file <- file.path(docs, page)
    write_utf8(clean_llms_md(read_utf8(file)), file)
  }

  url <- settings$website$`site-url`
  href <- function(page) {
    if (is.null(url)) page else paste0(sub("/+$", "", url), "/", page)
  }
  item <- function(page, text, notes = NULL) {
    sprintf(
      "- [%s](%s)%s",
      text,
      href(page),
      if (length(notes) == 1 && nzchar(notes)) paste0(": ", notes) else ""
    )
  }
  section <- function(title, lines) {
    if (length(lines) > 0) c(paste("##", title), "", lines, "")
  }
  listed <- c("index.llms.md", "reference/index.llms.md")

  home <- if ("index.llms.md" %in% pages) {
    llms_home(read_utf8(file.path(docs, "index.llms.md")))
  }

  vignettes <- find_vignettes(path)
  intro <- find_getting_started_vignette(path, vignettes)
  vignettes <- c(intro, setdiff(vignettes, intro))
  vignette_pages <- paste0("vignettes/", strip_ext(vignettes), ".llms.md")
  keep <- vignette_pages %in% pages
  article_items <- unlist(Map(
    function(page, vignette) {
      item(page, vignette_title(file.path(path, "vignettes", vignette)))
    },
    vignette_pages[keep],
    vignettes[keep]
  ))
  listed <- c(listed, vignette_pages)

  # As on the reference index (build_reference_qmd()), a heading level down.
  blocks <- reference_blocks(config, topics)
  reference <- unlist(Map(
    function(block, idx) {
      pages_idx <- paste0(
        "reference/",
        topic_basenames(topics[idx]),
        ".llms.md"
      )
      idx <- idx[pages_idx %in% pages]
      if (
        length(idx) == 0 && length(block$contents) > 0 || hidden_block(block)
      ) {
        return(NULL)
      }
      items <- vapply(
        unname(topics[idx]),
        function(topic) {
          names <- html_unescape(gsub("<[^>]+>", "", topic$names))
          item(
            paste0("reference/", topic$basename, ".llms.md"),
            paste0("`", names, "`", collapse = ", "),
            squish(topic$title)
          )
        },
        character(1)
      )
      c(
        if (!is.null(block$title)) c(paste("###", block$title), ""),
        if (!is.null(block$subtitle)) c(paste("####", block$subtitle), ""),
        if (!is.null(block$desc)) c(trimws(block$desc), ""),
        if (length(items) > 0) c(items, "")
      )
    },
    blocks,
    reference_block_topics(blocks, topics)
  ))
  # Headings alone, e.g. "All functions" without any topics, aren't listed.
  reference <- if (any(startsWith(reference, "- ["))) {
    utils::head(reference, -1)
  }
  listed <- c(
    listed,
    paste0("reference/", topic_basenames(topics), ".llms.md")
  )

  other_items <- vapply(
    sort(setdiff(pages, listed)),
    function(page) item(page, llms_page_title(file.path(docs, page), page)),
    character(1)
  )

  # The README usually says what DESCRIPTION's Description does, and more.
  description <- if (is.null(home)) {
    desc::desc_get_field("Description", default = NA, file = path)
  }
  lines <- c(
    paste("#", pkg_name(path)),
    "",
    if (is_string(settings$website$description)) {
      c(paste(">", squish(settings$website$description)), "")
    },
    if (is_string(description) && !is.na(description)) {
      c(squish(description), "")
    },
    section("Home", home),
    section("Articles", article_items),
    section("Reference", reference),
    section("Optional", unname(other_items))
  )
  write_utf8(utils::head(lines, -1), file.path(docs, "llms.txt"))
}

topic_basenames <- function(topics) {
  vapply(topics, `[[`, character(1), "basename", USE.NAMES = FALSE)
}

# The home page's Markdown `lines` for a section of llms.txt: without its
# title, which llms.txt has, and with its headings a level down.
llms_home <- function(lines) {
  text <- code_block_lines(lines) == ""
  title <- which(text & grepl("^# ", lines))
  if (length(title) > 0) {
    lines <- lines[-title[[1]]]
    text <- text[-title[[1]]]
  }
  lines[text] <- sub("^(#+ )", "#\\1", lines[text])
  while (length(lines) > 0 && !nzchar(trimws(lines[[1]]))) {
    lines <- lines[-1]
  }
  while (length(lines) > 0 && !nzchar(trimws(lines[[length(lines)]]))) {
    lines <- lines[-length(lines)]
  }
  lines
}

# A `.llms.md` page's first heading, without images, else its file name.
llms_page_title <- function(file, page) {
  lines <- read_utf8(file)
  heading <- grep("^# ", lines, value = TRUE)
  title <- if (length(heading) > 0) {
    squish(gsub("!\\[[^]]*\\]\\([^)]*\\)", "", sub("^# ", "", heading[[1]])))
  }
  if (is_string(title) && nzchar(title)) {
    title
  } else {
    sub("\\.llms\\.md$", "", page)
  }
}
