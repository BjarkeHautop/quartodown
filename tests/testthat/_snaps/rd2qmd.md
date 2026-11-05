# a \link{} to an internal topic without a page is plain text and warns

    Code
      check_internal_links(list(), topics, "examplepkg")
    Condition
      Warning:
      Some links go to internal topics, which have no page:
      * 'man/foo.Rd' links "helper", "helper_alias"
      i List them under `reference:`, in a section with `title: internal` to keep them off the index.

# navbar links to reference pages that won't exist warn

    Code
      check_internal_links(config, topics, "examplepkg")
    Condition
      Warning:
      Some links go to internal topics, which have no page:
      * _quartodown.yml `navbar:` links "topic-guide"
      i List them under `reference:`, in a section with `title: internal` to keep them off the index.
      Warning:
      _quartodown.yml `navbar:` links "fooo", which is not a topic in 'man/'.

