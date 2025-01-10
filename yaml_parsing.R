
# the whole deal
yaml_string <- "---\r\ntitle: \"your_title_goes_here\"\r\nauthor: \"your_name_goes_here\"\r\ndate: last-modified\r\nformat:\r\n  html:\r\n    embed-resources: true\r\n    theme:\r\n      - default\r\n      - custom.scss\r\nknitr:\r\n  opts_chunk:        ############ set global options ############\r\n    collapse: true   # keep code from blocks together (if shown)\r\n    echo: false      # don't show code\r\n    message: true    # show messages\r\n    warning: true    # show warnings\r\n    error: true      # show error messages\r\n    comment: \"\"      # don't show ## with printed output\r\n    dpi: 100         # image resolution (typically 300 for publication)\r\n    fig-width: 6.5   # figure width\r\n    fig-height: 4.0  # figure height\r\n    R.options:    \r\n      digits: 3    # round to three digits\r\neditor: source\r\nbibliography: [references.bib, packages.bib]\r\ncsl: the-new-england-journal-of-medicine.csl\r\n---"
 
yaml_less <- "format:\r\n  html:\r\n    embed-resources: true\r\n    theme:\r\n      - default\r\n      - custom.scss"
 
# what we're looking for (non-Windows)
qmd_pattern <- "format:\n  html:\n    embed-resources: true\n    theme:\n      - default\n      - custom.scss"

# Replace that with this:
qmd_replacement <- "output: rmarkdown::html_vignette\nvignette: >\n  %\\\\VignetteIndexEntry{your_title_goes_here}\n  %\\\\VignetteEngine{knitr::rmarkdown}\n  %\\\\VignetteEncoding{UTF-8}\n"
 
 
# what is read in Windows
pattern_win <- "format:\r\n  html:\r\n    embed-resources: true\r\n    theme:\r\n      - default\r\n      - custom.scss"

# stringr::str_replace_all(pattern, '\n', '\r\n')

if (.Platform$OS.type == 'windows') {
  qmd_pattern <- stringr::str_replace_all(qmd_pattern, '\n', '\r\n')
  qmd_replacement <- stringr::str_replace_all(qmd_replacement, '\n', '\r\n')
}

stringr::str_replace(yaml_string, qmd_pattern, qmd_replacement)
stringr::str_replace(yaml_less, qmd_pattern, qmd_replacement)

#################################################################################

# what Windows sees
yaml_rmd <- "---\r\ntitle: \"your_title_goes_here\"\r\nauthor: \"your_name_goes_here\"\r\ndate: \"`r Sys.Date()`\"\r\noutput:\r\n  bookdown::html_document2:\r\n    number_sections: false\r\nbibliography: [references.bib, packages.bib]\r\ncsl: the-new-england-journal-of-medicine.csl\r\n---"

# what Windows sees reduced
yaml_rmd_less <- "output:\r\n  bookdown::html_document2:\r\n    number_sections: false\r\n"

# what will be replaced
rmd_pattern <- "output:\n  bookdown::html_document2:\n    number_sections: false\n"

# what it'll be replaced with
rmd_replacement <- "output: rmarkdown::html_vignette\nvignette: >\n  %\\\\VignetteIndexEntry{your_title_goes_here}\n  %\\\\VignetteEngine{knitr::rmarkdown}\n  %\\\\VignetteEncoding{UTF-8}\n"

if (.Platform$OS.type == 'windows') {
  rmd_pattern <- stringr::str_replace_all(rmd_pattern, '\n', '\r\n')
  rmd_replacement <- stringr::str_replace_all(rmd_replacement, '\n', '\r\n')
}

stringr::str_replace(yaml_rmd, rmd_pattern, rmd_replacement)
stringr::str_replace(yaml_rmd_less, rmd_pattern, rmd_replacement)
