#' Make an Analysis Project
#' 
#' @description This function makes an R project that includes an analysis.Rmd 
#' file with conflicted and tidyverse and an aggressive .gitignore.  The 
#' .gitignore is designed to help protect against leaking data (with protected 
#' health information). This function is used by the research_project.dcf file 
#' to make the files.
#'
#' @param path Path automatically sent by research_project.dcf
#'
#' @import tidyverse 
#' @import conflicted
#' @importFrom utils download.file
#'
#' @return Creates a project directory with stuff in it.
#' @export

make_project <- function(path) {
  
  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  
  # generate header
  header <- c(
    '---',
    'title: "title_goes_here"',
    'author: "me"',
    'date: "`r Sys.Date()`"',
    'output:', 
    '  bookdown::html_document2:',
    '  df_print: kable',
    'bibliography: references.bib',
    '---',
    '',
    '```{r tidyverse}',
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
    'Analyses were conducted with `r stringr::word(R.Version()$version.string, 1, 3)` with the `tidyverse` (`r packageVersion("tidyverse")`) and `table1` (`r packageVersion("tidyverse")`) packages used to preprocess and summarize data.[@wickham2019; @table1_2021]',
    '',
    '# Results',
    '',
    '# Conclusion'
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
  
  gist_path_bib <- paste(
    "https://gist.githubusercontent.com/RaymondBalise/",
    "10abfed28ea343e4e7ce7752e39a5195/raw/",
    "0fd86d2aad871c1501ab887f93290be269f453a0/references.bib"
  )
  download.file(gist_path_bib, paste0(path, "/references.bib"))
}
