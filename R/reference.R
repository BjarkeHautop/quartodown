# ---- reference/index.qmd generation ------------------------------------------
#
# `_quartodown.yml`'s `reference:` groups the index into sections, each
# listing topics by name or selector. An internal topic is listed only when
# named, or selected with `internal = TRUE`, and then gets a page too.

# Every man/*.Rd as a topic: the parsed `rd`, its file's basename, `\name`,
# title, aliases, concepts and keywords. build_site() parses them once, and
# passes them to everything else that needs them.
rd_topics <- function(path) {
  man_dir <- file.path(path, "man")
  if (!dir.exists(man_dir)) {
    return(list())
  }
  rd_files <- fs::dir_ls(man_dir, regexp = "\\.Rd$")
  macros <- rd_macros(path)

  lapply(rd_files, function(rd_file) {
    rd <- tools::parse_Rd(rd_file, macros = macros)
    tags <- rd_tags(rd)
    tag_text <- function(tag) {
      trimws(vapply(
        rd[tags == tag],
        function(x) as.character(x[[1]]),
        character(1)
      ))
    }

    alias <- tag_text("\\alias")
    names <- usage_names(rd[tags == "\\usage"])
    keywords <- tag_text("\\keyword")
    list(
      rd = rd,
      basename = fs::path_ext_remove(basename(rd_file)),
      name = tag_text("\\name"),
      title = trimws(paste(unlist(rd[tags == "\\title"]), collapse = "")),
      alias = alias,
      names = if (length(names) > 0) names else alias,
      concepts = tag_text("\\concept"),
      keywords = keywords,
      internal = "internal" %in% keywords
    )
  })
}

# The tag of each top-level element of the parsed Rd `rd`, e.g. "\\alias".
rd_tags <- function(rd) {
  vapply(rd, function(x) attr(x, "Rd_tag"), character(1))
}

topic_internal <- function(topics) {
  vapply(topics, `[[`, logical(1), "internal")
}

# The name each of `topics` is listed under in `reference:`.
topic_first_alias <- function(topics) {
  vapply(topics, function(t) t$alias[[1]], character(1))
}

# ---- index names from \usage ------------------------------------------------
#
# A topic's index row lists every function in its \usage: `f()`, `name` for
# data or an infix operator, `generic(<class>)` for a method of a generic
# documented elsewhere. Without a usage, it lists the aliases.

# Turns the Rd markup of a \usage section into parseable R code, writing
# \method{generic}{class} as S3method(`generic`, `class`).
usage_code <- function(x) {
  if (is.character(x) && is.null(attr(x, "Rd_tag"))) {
    return(paste(x, collapse = ""))
  }
  collapse <- function(x) {
    paste(vapply(x, usage_code, character(1)), collapse = "")
  }
  tag <- attr(x, "Rd_tag")
  switch(
    if (is.null(tag)) "" else tag,
    TEXT = ,
    RCODE = ,
    VERB = paste(as.character(x), collapse = ""),
    COMMENT = "",
    "\\dots" = ,
    "\\ldots" = "...",
    "\\method" = ,
    # Names may be backticked already, as in \method{print}{`NULL`}.
    "\\S3method" = sprintf(
      "S3method(`%s`, `%s`)",
      unbacktick(usage_code(x[[1]])),
      unbacktick(usage_code(x[[2]]))
    ),
    "\\S4method" = sprintf(
      "S4method(`%s`, list(%s))",
      unbacktick(usage_code(x[[1]])),
      paste0(
        "`",
        unbacktick(strsplit(usage_code(x[[2]]), ",")[[1]]),
        "`",
        collapse = ","
      )
    ),
    collapse(x)
  )
}

unbacktick <- function(x) {
  sub("^`(.*)`$", "\\1", x)
}

is_infix <- function(name) {
  grepl("^%.*%$", name) ||
    name %in%
      c(
        "+",
        "-",
        "*",
        "^",
        "/",
        "==",
        ">",
        "<",
        "!=",
        "<=",
        ">=",
        "&",
        "|",
        "[[",
        "[",
        "$"
      )
}

# Returns list(type, name, signature) for one usage expression, with type
# "data", "fun", "s3" or "s4", or NULL for anything else.
usage_type <- function(x) {
  if (is.symbol(x)) {
    return(list(type = "data", name = as.character(x)))
  }
  if (!is.call(x)) {
    return(NULL)
  }
  if (identical(x[[1]], quote(data))) {
    return(list(type = "data", name = as.character(x[[2]])))
  }
  replacement <- identical(x[[1]], quote(`<-`))
  if (replacement) {
    x <- x[[2]]
    if (!is.call(x)) return(NULL)
  }

  fun <- x[[1]]
  out <- if (is.symbol(fun)) {
    list(type = "fun", name = as.character(fun))
  } else if (identical(fun[[1]], quote(S3method))) {
    list(
      type = "s3",
      name = as.character(fun[[2]]),
      signature = as.character(fun[[3]])
    )
  } else if (identical(fun[[1]], quote(S4method))) {
    sig <- vapply(as.list(fun[[3]])[-1], as.character, character(1))
    list(type = "s4", name = as.character(fun[[2]]), signature = sig)
  } else if (
    identical(fun[[1]], quote(`::`)) || identical(fun[[1]], quote(`:::`))
  ) {
    list(type = "fun", name = as.character(fun[[3]]))
  } else {
    return(NULL)
  }
  if (replacement) {
    out$name <- paste0(out$name, "<-")
  }
  out
}

short_name <- function(u) {
  name <- html_escape(u$name)
  if (make.names(u$name) != u$name) {
    # As an entity: Pandoc would take a literal backtick for a code span,
    # even inside the raw HTML `<code>`, and show "&lt;" in `units<-` as is.
    name <- paste0("&#96;", name, "&#96;")
  }
  switch(
    u$type,
    data = name,
    fun = if (is_infix(u$name)) name else paste0(name, "()"),
    paste0(
      name,
      "(",
      paste0("<i>&lt;", html_escape(u$signature), "&gt;</i>", collapse = ","),
      ")"
    )
  )
}

# `usage` is the (possibly empty) list of \usage sections of one Rd file.
usage_names <- function(usage) {
  if (length(usage) == 0) {
    return(character())
  }
  code <- paste(vapply(usage, usage_code, character(1)), collapse = "\n")
  exprs <- tryCatch(
    as.list(parse(text = code, keep.source = FALSE)),
    error = function(e) {
      cli::cli_warn("Failed to parse usage: {.code {code}}")
      list()
    }
  )
  funs <- Filter(Negate(is.null), lapply(exprs, usage_type))
  if (length(funs) == 0) {
    return(character())
  }

  # Drop methods for generics documented in the same topic.
  name <- vapply(funs, `[[`, character(1), "name")
  type <- vapply(funs, `[[`, character(1), "type")
  self_meth <- name %in% name[type == "fun"] & type %in% c("s3", "s4")
  unique(vapply(funs[!self_meth], short_name, character(1)))
}

# The environment `contents:` entries are evaluated in: each alias and Rd file name bound to its topic's position,
# plus the selectors. All but has_keyword() skip internal topics unless
# `internal = TRUE`.
match_env <- function(topics) {
  env <- new.env(parent = emptyenv())
  assign("-", function(x) -x, envir = env)
  assign("c", function(...) c(...), envir = env)

  alias_vecs <- lapply(topics, `[[`, "alias")
  for (i in seq_along(topics)) {
    for (alias in alias_vecs[[i]]) {
      assign(alias, i, envir = env)
    }
  }
  for (i in seq_along(topics)) {
    assign(topics[[i]]$basename, i, envir = env)
  }

  internal_topics <- topic_internal(topics)
  selectable <- function(match, internal) {
    which(match & (internal | !internal_topics))
  }
  any_alias <- function(f, internal) {
    selectable(vapply(alias_vecs, function(x) any(f(x)), logical(1)), internal)
  }

  assign(
    "starts_with",
    function(x, internal = FALSE) {
      any_alias(function(a) grepl(paste0("^", x), a), internal)
    },
    envir = env
  )
  assign(
    "ends_with",
    function(x, internal = FALSE) {
      any_alias(function(a) grepl(paste0(x, "$"), a), internal)
    },
    envir = env
  )
  assign(
    "contains",
    function(x, internal = FALSE) {
      any_alias(function(a) grepl(x, a, fixed = TRUE), internal)
    },
    envir = env
  )
  assign(
    "matches",
    function(x, internal = FALSE) any_alias(function(a) grepl(x, a), internal),
    envir = env
  )
  assign(
    "has_concept",
    function(x, internal = FALSE) {
      match <- vapply(topics, function(t) any(t$concepts == x), logical(1))
      selectable(match, internal)
    },
    envir = env
  )
  assign(
    "has_keyword",
    function(x) {
      which(vapply(topics, function(t) any(t$keywords %in% x), logical(1)))
    },
    envir = env
  )

  env
}

match_eval <- function(string, env) {
  literal <- mget(string, envir = env, ifnotfound = list(NULL))[[1]]
  if (is.numeric(literal)) {
    return(as.integer(literal))
  }

  expr <- tryCatch(str2lang(string), error = function(e) NULL)
  if (is.null(expr)) {
    config_abort(
      "reference",
      "entry {.val {string}} must be valid R code."
    )
  }

  if (is.symbol(expr)) {
    val <- mget(as.character(expr), envir = env, ifnotfound = list(NULL))[[1]]
    if (!is.numeric(val)) {
      config_abort(
        "reference",
        "entry {.val {string}} must be a known \\alias{{}} or selector."
      )
    }
    as.integer(val)
  } else {
    tryCatch(
      as.integer(eval(expr, envir = env)),
      error = function(e) {
        config_abort(
          "reference",
          "entry {.val {string}} failed to evaluate.",
          parent = e
        )
      }
    )
  }
}

select_topics <- function(match_strings, topics) {
  if (length(match_strings) == 0) {
    return(integer())
  }

  env <- match_env(topics)
  indexes <- lapply(match_strings, match_eval, env = env)

  all_sign <- function(x, text) {
    if (all(x > 0)) {
      return("+")
    }
    if (all(x < 0)) {
      return("-")
    }
    config_abort(
      "reference",
      "entry {.val {text}} must be all positive or all negative."
    )
  }

  sign1 <- all_sign(indexes[[1]], match_strings[[1]])
  public <- which(!topic_internal(topics))
  sel <- switch(sign1, "+" = integer(), "-" = public)

  for (i in seq_along(indexes)) {
    sign <- all_sign(indexes[[i]], match_strings[[i]])
    sel <- switch(
      sign,
      "+" = union(sel, indexes[[i]]),
      "-" = setdiff(sel, -indexes[[i]])
    )
  }
  sel
}

build_reference_qmd <- function(
  path,
  config = list(),
  topics = rd_topics(path)
) {
  out_dir <- file.path(project_dir(path), "reference")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, "index.qmd")
  internal <- topic_internal(topics)
  blocks <- reference_blocks(config, topics)
  block_indexes <- reference_block_topics(blocks, topics)

  # Internal topics needn't be listed.
  unlisted <- setdiff(which(!internal), unique(unlist(block_indexes)))
  if (length(unlisted) > 0) {
    unlisted_names <- topic_first_alias(topics[unlisted])
    cli::cli_warn(
      "man/*.Rd defines {.val {unlisted_names}}, which {?is/are} not listed under `reference:` in _quartodown.yml and won't appear on the reference page."
    )
  }

  sections <- Map(
    function(block, idx) {
      # Drop a section whose contents matched nothing, e.g. a selector that
      # matched only internal topics.
      if (length(idx) == 0 && length(block$contents) > 0) {
        return(NULL)
      }
      entries <- vapply(
        idx,
        function(i) {
          topic <- topics[[i]]
          links <- sprintf(
            '<code><a href="%s.qmd">%s</a></code>',
            topic$basename,
            topic$names
          )
          sprintf(
            "<dt>%s</dt>\n<dd>%s</dd>",
            paste(links, collapse = " "),
            topic$title
          )
        },
        character(1)
      )
      # `title:` is a section heading and `subtitle:` a
      # heading one level down; either may stand alone with no contents.
      c(
        if (!is.null(block$title)) c(paste0("## ", block$title), ""),
        if (!is.null(block$subtitle)) c(paste0("### ", block$subtitle), ""),
        if (!is.null(block$desc)) c(trimws(block$desc), ""),
        if (length(entries) > 0) {
          c(
            '<dl class="ref-index">',
            paste(entries, collapse = "\n\n"),
            "</dl>",
            ""
          )
        }
      )
    },
    blocks,
    block_indexes
  )

  lines <- c('---', 'title: "Function reference"', '---', "", unlist(sections))
  lines <- lines[-length(lines)] # drop trailing blank line
  write_utf8(lines, out_path)
}

# The sections of the reference index: `reference:`, or without it, one
# section listing every public topic.
reference_blocks <- function(config, topics) {
  if (!is.null(config$reference)) {
    return(config$reference)
  }
  public <- !topic_internal(topics)
  list(list(
    title = "All functions",
    contents = topic_first_alias(topics[public])
  ))
}

# The topics each of `blocks` lists, as positions in `topics`.
reference_block_topics <- function(blocks, topics) {
  lapply(blocks, function(block) {
    select_topics(as.character(block$contents), topics)
  })
}

# Whether each of `topics` gets a reference page: every public topic does,
# and an internal one only if the reference index lists it.
has_page <- function(topics, config = list()) {
  blocks <- reference_blocks(config, topics)
  listed <- unlist(reference_block_topics(blocks, topics))
  !topic_internal(topics) | seq_along(topics) %in% listed
}
