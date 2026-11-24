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

# `llms.txt` (https://llmstxt.org/), linking the `.llms.md` pages in `docs`:
# the home page and the function reference, then the vignettes, the reference
# topics in the index's order, and the rest under "Optional". Nothing without
# `.llms.md` pages, i.e. with `llms-txt: false` or an older Quarto.
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
  listed <- character()
  section <- function(title, items) {
    if (length(items) > 0) c(paste("##", title), "", items, "")
  }

  docs_items <- character()
  for (page in intersect(
    c("index.llms.md", "reference/index.llms.md"),
    pages
  )) {
    text <- if (page == "index.llms.md") "Home" else "Function reference"
    docs_items <- c(docs_items, item(page, text))
  }
  listed <- c(listed, "index.llms.md", "reference/index.llms.md")

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

  blocks <- reference_blocks(config, topics)
  idx <- unique(unlist(reference_block_topics(blocks, topics)[
    !vapply(blocks, hidden_block, logical(1))
  ]))
  reference_items <- character()
  for (topic in unname(topics[idx])) {
    page <- paste0("reference/", topic$basename, ".llms.md")
    if (page %in% pages) {
      names <- html_unescape(gsub("<[^>]+>", "", topic$names))
      text <- paste0("`", names, "`", collapse = ", ")
      reference_items <- c(
        reference_items,
        item(page, text, squish(topic$title))
      )
      listed <- c(listed, page)
    }
  }

  other_items <- vapply(
    sort(setdiff(pages, listed)),
    function(page) item(page, llms_page_title(file.path(docs, page), page)),
    character(1)
  )

  description <- desc::desc_get_field("Description", default = NA, file = path)
  lines <- c(
    paste("#", pkg_name(path)),
    "",
    if (is_string(settings$website$description)) {
      c(paste(">", squish(settings$website$description)), "")
    },
    if (!is.na(description)) c(squish(description), ""),
    section("Docs", docs_items),
    section("Articles", article_items),
    section("Reference", reference_items),
    section("Optional", unname(other_items))
  )
  write_utf8(utils::head(lines, -1), file.path(docs, "llms.txt"))
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
