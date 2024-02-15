usethis::use_package("knitr", type = "suggests")
usethis::use_package("rmarkdown", type = "suggests")

usethis::use_package("conflicted", type = "imports")
usethis::use_package("glue", type = "imports")
usethis::use_package("gtsummary", type = "imports")
usethis::use_package("rUM", type = "imports")
usethis::use_package("rio", type = "imports")
usethis::use_package("table1", type = "imports")
usethis::use_package("tidymodels", type = "imports")
usethis::use_package("tidyverse", type = "imports")

usethis::use_vignette("x","x")
invisible(file.remove(paste0(here::here(), "/vignettes/x.Rmd")))

# cut me later
usethis::use_mit_license("me")


library(stringr)

# Read the file content
file_content <- readr::read_file(paste0(here::here(), "/vignettes/analysis.Rmd"))

# Replace the pattern with the new sentence
modified_contentX <- 
  str_replace_all(
    file_content, 
    fixed("output:\n  bookdown::html_document2:\n    number_sections: false\n"), 
    fixed("output: rmarkdown::html_vignette\nvignette: >\n  %\\VignetteIndexEntry{your_title_goes_here}\n  %\\VignetteEngine{knitr::rmarkdown}\n  %\\VignetteEncoding{UTF-8}\n")
  )

readr::write_file(modified_contentX, paste0(here::here(), "/vignettes/analysis.Rmd"))

library(stringr)

# Read the file content
file_content <- readr::read_file(paste0(here::here(), "/vignettes/analysis.Rmd"))

# Replace the pattern with the new sentence
modified_contentX <- 
  str_replace_all(
    file_content, 
    fixed("output:\n  bookdown::html_document2:\n    number_sections: false\n"), 
    fixed("output: rmarkdown::html_vignette\nvignette: >\n  %\\VignetteIndexEntry{your_title_goes_here}\n  %\\VignetteEngine{knitr::rmarkdown}\n  %\\VignetteEncoding{UTF-8}\n")
  )

readr::write_file(modified_contentX, paste0(here::here(), "/vignettes/analysis.Rmd"))

rm(list = c("modified_contentX", "file_content"))


invisible(file.remove(paste0(here::here(), "/run_me.R")))

