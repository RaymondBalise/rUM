# Package Initialization Script
# rUM Package Authors
# Initial Date: 2024-04-23
# Last updated: 2024-04-23

# This script sets options for you to develop a new package which contains a 
#   manuscript vignette. To run this file, click the "Source" button at the top
#   right of the RStudio script windowpane. Upon successful completion of the
#   code in this script, RStudio will prompt you to delete two files: this
#   script (RUN_ME_FIRST.R) and a vignette placeholder file called "x" (either
#   a .qmd or .Rmd file). Click "Yes" when prompted to delete both of these
#   files.



######  List packages used in the vignette  ###################################

usethis::use_package("here", type = "suggests")
usethis::use_package("knitr", type = "suggests")
usethis::use_package("rmarkdown", type = "suggests")
usethis::use_package("roxygen2", type = "suggests")

usethis::use_package("conflicted", type = "suggests")
usethis::use_package("glue", type = "suggests")
usethis::use_package("gtsummary", type = "suggests")
usethis::use_package("quarto", type = "suggests", min_version = "1.3.12")
usethis::use_package("rUM", type = "suggests")
usethis::use_package("rio", type = "suggests")
usethis::use_package("table1", type = "suggests")
usethis::use_package("tidymodels", type = "suggests")
usethis::use_package("tidyverse", type = "suggests")

##### Make a bogus vignette to get benefits (.gitignore(s), VignetteBuilder)
usethis::use_vignette("x","x")
invisible(file.remove(paste0(here::here(), "/vignettes/x.Rmd")))

##### Fix vignette header

library(stringr)

# Read the file content
file_content <- readr::read_file(paste0(here::here(), "/vignettes/analysis.qmd"))

# Replace the pattern with the new sentence
modified_contentX <- 
  str_replace_all(
    file_content, 
    fixed("format:\n  html:\n    self-contained: true\n"), 
    fixed("output: rmarkdown::html_vignette\nvignette: >\n  %\\VignetteIndexEntry{your_title_goes_here}\n  %\\VignetteEngine{quarto::html}\n  %\\VignetteEncoding{UTF-8}\n")
  )

readr::write_file(modified_contentX, paste0(here::here(), "/vignettes/analysis.qmd"))


##### Change vignette engine to quarto
file_content <- readr::read_file(paste0(here::here(), "/DESCRIPTION"))
modified_contentX <- 
  str_replace_all(
    file_content, 
    fixed("VignetteBuilder: knitr\n"), 
    fixed("VignetteBuilder: \n    quarto\n")
  )
readr::write_file(modified_contentX, paste0(here::here(), "/DESCRIPTION"))

rm(list = c("modified_contentX", "file_content"))

##### delete this file
invisible(file.remove(paste0(here::here(), "/RUN_ME_FIRST.R")))

