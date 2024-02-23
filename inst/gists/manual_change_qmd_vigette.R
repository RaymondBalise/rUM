##### List packages used in the vignette

usethis::use_package("knitr", type = "suggests")
usethis::use_package("rmarkdown", type = "suggests")

usethis::use_package("conflicted", type = "imports")
usethis::use_package("glue", type = "imports")
usethis::use_package("gtsummary", type = "imports")
usethis::use_package("quarto", type = "imports", min_version = "1.3.12")
usethis::use_package("rUM", type = "imports")
usethis::use_package("rio", type = "imports")
usethis::use_package("table1", type = "imports")
usethis::use_package("tidymodels", type = "imports")
usethis::use_package("tidyverse", type = "imports")

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

