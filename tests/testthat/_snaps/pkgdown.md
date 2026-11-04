# translate_pkgdown() translates a _pkgdown.yml

    Code
      writeLines(pkgdown_config_yml(translation$config, "_pkgdown.yml"))
    Output
      # Configuration for quartodown::build_site(), translated from _pkgdown.yml by
      # use_quartodown(). See vignette("customizing", package = "quartodown").
      
      url: https://me.github.io/examplepkg
      
      # Pages for internal topics too, which some links go to, as pkgdown builds.
      internal-pages: true
      
      development:
        mode: auto
      
      navbar:
        left:
          - intro
          - reference
          - text: Articles
            menu:
              - basics
              - extras
              - text: '---'
              - text: Deeper
              - articles/advanced
          - text: Guides
            menu:
              - text: Concepts
              - basics
              - text: The Foo topic
                href: reference/foo.qmd
              - text: '---'
              - text: Old index
                href: articles/index.html
          - news
        tools:
          - repo
          - icon: mastodon
            aria-label: Mastodon
            href: https://fosstodon.org/@me
      
      home:
        links:
          - text: Blog
            href: https://example.com
      
      theme:
        light: flatly
      
      reference:
        - title: Functions
          contents: foo
      
      # pkgdown's addresses of the vignettes and changelog, now under vignettes/
      # and NEWS.html.
      redirects:
        - [old.html, vignettes/basics.html]
        - [articles/basics.html, vignettes/basics.html]
        - [articles/examplepkg.html, vignettes/examplepkg.html]
        - [articles/extras.html, vignettes/extras.html]
        - [articles/advanced.html, vignettes/articles/advanced.html]
        - [news/index.html, NEWS.html]
      
      quarto:
        website:
          description: Does examples.
          navbar:
            title: Example
          open-graph:
            image: man/figures/card.png
            image-alt: A card
          twitter-card:
            creator: '@me'
            card-style: summary_large_image
            image: man/figures/card.png
          google-analytics: G-123
        format:
          html:
            include-in-header: _quartodown/in-header.html
            html-math-method: katex
            toc-depth: 3
            fig-width: 8
        lang: da
    Code
      writeLines(translation$skipped)
    Output
      `development: version_label`
      navbar link to `articles/index.html`: no such page
      `home: title`: the home page's title is the package's
      `template: includes: before_navbar`
      `template: includes`: copied to _quartodown/in-header.html; check what they load, such as analytics
      `template: package`: quartodown has its own theme
      `figures: dev`
      `footer`: set the footer's roles with `authors: footer:`, or `quarto: website: page-footer:`
      `news`: the changelog is NEWS.md as one page
      `search`
    Code
      translation$files
    Output
      $`in-header.html`
      [1] "<script src='analytics.js'></script>"
      
      $extra.scss
      [1] "/*-- scss:defaults --*/"                                                      
      [2] "@import url(\"https://fonts.googleapis.com/css2?family=Inter&display=swap\");"
      [3] "$primary: #0054AD;"                                                           
      [4] "$font-family-sans-serif: \"Inter\", sans-serif;"                              
      [5] "/*-- scss:rules --*/"                                                         
      [6] ".navbar { font-weight: bold; }"                                               
      

