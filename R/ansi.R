# ---- colored output -------------------------------------------------------------
#
# The pages' R processes run with `R_CLI_NUM_COLORS=256`, so cli (and crayon)
# color their output with ANSI escape codes, as in a terminal. Quarto passes
# them through, but xml2 drops their ESC character (not allowed in XML), so
# link_code_html() would lose them: protect_ansi() swaps it for a private-use
# character first, and ansi_to_html() turns the codes into `<span>`s
# afterwards (see postprocess_pages()), colored by ansi_scss_rules(). fansi,
# not cli::ansi_html(), which drops the color of text styled inside it (as in
# `col_red("a {.strong b}")`).

ansi_esc <- "\u001b"
ansi_placeholder <- "\uE01B"

protect_ansi <- function(html) {
  gsub(ansi_esc, ansi_placeholder, html, fixed = TRUE)
}

# The page's `lines`, with each one's protected codes made `<span>`s.
ansi_to_html <- function(lines) {
  has <- grepl(ansi_placeholder, lines, fixed = TRUE)
  codes <- gsub(ansi_placeholder, ansi_esc, lines[has], fixed = TRUE)
  lines[has] <- fansi::sgr_to_html(
    codes,
    warn = FALSE,
    term.cap = c("bright", "256"),
    classes = TRUE,
    carry = FALSE
  )
  lines
}

# The search index keeps the codes, JSON-escaped.
strip_search_ansi <- function(docs) {
  search <- file.path(docs, "search.json")
  if (file.exists(search)) {
    json <- read_utf8(search)
    json <- gsub("\\\\u001b\\[[0-9;]*m", "", json)
    write_utf8(json, search)
  }
  invisible()
}

# SCSS rules coloring ansi_to_html()'s `fansi-color-NNN` and `fansi-bgcol-NNN`
# classes, added to quartodown.scss. The colors are cli's 256: the 16 basic
# ones from a palette that suits the background, darker on the light theme
# and lighter on the dark one.
ansi_scss_rules <- function() {
  rules <- function(palette, n) {
    style <- unlist(cli::ansi_html_style(colors = 256, palette = palette))
    c(
      sprintf(".fansi-color-%03d %s", n, style[paste0(".ansi-color-", n)]),
      sprintf(".fansi-bgcol-%03d %s", n, style[paste0(".ansi-bg-color-", n)])
    )
  }
  c(
    "",
    "// Colored output, from ansi_scss_rules().",
    # Uncolored (e.g. only bold) text keeps the output's color, not Quarto's
    # `code span` one.
    "code span[style]:not([class]) { color: inherit; }",
    rules("vga", 16:255),
    "@if lightness($body-bg) > 50% {",
    paste0("  ", rules("vga", 0:15)),
    "} @else {",
    paste0("  ", rules("vscode", 0:15)),
    "}"
  )
}
