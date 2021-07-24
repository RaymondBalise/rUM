#' Make an Analysis Project
#' 
#' @description This function makes an R project that includes an analysis.Rmd 
#' file with conflicted and tidyverse and an aggressive .gitignore.  The 
#' .gitignore is designed to help protect against leaking data (with protected 
#' health information). This function is used by the research_project.dcf file 
#' to make the files.
#'
#' @param path Path automatically set by research_project.dcf (see
#'    \code{./rstudio/templates/project/})
#'
#' @import tidyverse 
#' @import conflicted
#' @importFrom utils download.file
#'
#' @return Creates a project directory with the following contents: a template
#'    \code{.Rmd} file called "analysis", a subdirectory for data, a template
#'    \code{.gitignore} with aggressive protections against publishing potential
#'    protected health information, a starter bibliography file called 
#'    "references" (in standard \code{.bib} format), and a stock Citation Style
#'    Language (\code{.csl}) file for the New England Journal of Medicine.
#'    
#' @export

make_project <- function(path) {
  
  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  
  # generate header
  header <- c(
    '---',
    'title: "your_title_goes_here"',
    'author: "your_name_goes_here"',
    'date: "`r Sys.Date()`"',
    'output:', 
    '  bookdown::html_document2:',
    '    number_sections: false',
    'bibliography: [references.bib, packages.bib]',
    'csl: the-new-england-journal-of-medicine.csl',
    '---',
    '',
    '```{r setup, echo=FALSE}',
    'knitr::opts_chunk$set(echo = FALSE)',
    '```',
    '',
    '```{r tidyverse, echo=FALSE}',
    'library(conflicted)',
    'conflict_prefer("filter", "dplyr", quiet = TRUE)',
    'conflict_prefer("lag", "dplyr", quiet = TRUE)',
    'suppressPackageStartupMessages(library(tidyverse))',
    '',
    '# suppress "`summarise()` has grouped output by " messages',
    'options(dplyr.summarise.inform=F) ', 
    '```',
    '',
    '# Introduction',
    '',
    '# Method',
    'Analyses were conducted with `r stringr::word(R.Version()$version.string, 1, 3)` with the `tidyverse` (`r packageVersion("tidyverse")`), `rUM` (`r packageVersion("rum")`), `table1` (`r packageVersion("tidyverse")`) packages used to preprocess and summarize data.[@R-base; @R-tidyverse; @tidyverse2019; @R-rUM; @R-table1]',
    '',
    '# Results',
    '',
    '# Conclusion',
    '',
    '# References {-}',
    '',
    '```{r include=FALSE}',
    '# automatically create a bib database for loaded R packages & rUM',
    'knitr::write_bib(',
    '  c(',
    '    .packages(),',
    '    "rUM",',
    '    "table1"',
    '  ),', 
    '  "packages.bib"',
    ')',
    '```'
)
  
  # collect into single text string
  contents <- paste(
    paste(header, collapse = "\n"),
    sep = "\n"
  )
  
  # write to index file
  writeLines(contents, con = file.path(path, "analysis.Rmd"))
  dir.create(paste0(path, "/data"), recursive = TRUE, showWarnings = FALSE)
  
  gist_path_ignore <- paste0(
    "https://gist.githubusercontent.com/RaymondBalise/",
    "300d99c2b6450feda3ed5a816f396191/raw/",
    "a38f77aab743a2670dbb80ab0278b30745527243/.gitignore"
  )
  download.file(gist_path_ignore, paste0(path, "/.gitignore"))

  # write an empty packages bibliography file - needed to knit the first time
  writeLines("", con = file.path(path, "packages.bib"))
    
  # write an empty user bibliography file
  writeLines("", con = file.path(path, "references.bib"))
  
  download.file(
    "https://www.zotero.org/styles/the-new-england-journal-of-medicine", 
    paste0(path, "/the-new-england-journal-of-medicine.csl")
  )
}
