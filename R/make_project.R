#' Make an Analysis Project
#' 
#' This function makes a R project that includes an analysis.Rmd file with 
#' conflicted and tidyverse.  This is used by the research_project.dcf file.
#'
#' @param path Path automatically sent by research_project.dcf
#'
#' @return a project
#' @export

make_project <- function(path) {
  
  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  
  # generate header
  header <- c(
    '---',
    'title: "html"',
    'author: ""',
    'date: "`r Sys.Date()`"',
    'output:', 
    '  bookdown::html_document2:',
    '  df_print: kable',
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
    '```'
)
  
  
  # collect into single text string
  contents <- paste(
    paste(header, collapse = "\n"),
    sep = "\n"
  )
  
  # write to index file
  writeLines(contents, con = file.path(path, "analysis.Rmd"))
  dir.create(paste0(path, "/data"),recursive = TRUE, showWarnings = FALSE)
}
