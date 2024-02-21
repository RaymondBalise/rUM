#' Make an Analysis Project
#'
#' @description This function makes an R project that includes an analysis.Rmd
#' or analysis.qmd file using the conflicted and tidyverse packages.  This
#' project automatically includes an aggressive .gitignore which is designed to
#' help protect against leaking data (with protected  health information), a
#' starter bibliography file called "references" (in standard .bib format),
#' and a stock Citation Style Language (.csl) file for the New England Journal
#' of Medicine.
#'
#' @param path Path automatically set by research_project.dcf (see
#'    \code{./rstudio/templates/project/})
#' @param type Choose between "Quarto (analysis.qmd)" or
#'    "R Markdown (analysis.Rmd)"
#' @param example Will the analysis file include an example table/figure?"
#'
#' @details Behind the scenes, this function used by research_project.dcf when
#' a user selects New project... > New Directory > rUM Research Project Template
#' within the RSutdio IDE. See \code{./rstudio/templates/project/}.
#'
#' @importFrom tidyverse tidyverse_logo
#' @importFrom conflicted conflict_prefer
#' @importFrom bookdown html_document2
#' @importFrom rmarkdown html_document
#' @importFrom table1 t1kable
#' @importFrom rlang abort
#' @importFrom utils download.file
#' @importFrom usethis create_project
#' @importFrom glue glue
#' @importFrom gtsummary tbl_summary
#' @importFrom rio import
#' @importFrom tidymodels tidymodels_prefer
#'
#' @return Returns nothing.  See description above.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   make_project(path = "~/test_project", type = "Quarto (analysis.qmd)", example = TRUE)
#' }
#' \dontrun{
#'   make_project(path = "~/test_project", type = "R Markdown (analysis.Rmd), example = TRUE")
#' }

make_project <- function(
    path,
    type = c("Quarto (analysis.qmd)", "R Markdown (analysis.Rmd)"),
    example = FALSE,
    vignette = FALSE
  ) {

  type <- match.arg(type)
  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  if (vignette == FALSE){ # make paper project w/o package infrastructure
    # If the project object does not exist add it.
    if (length(list.files(path = path, pattern = "\\.Rproj$")) == 0) {
      usethis::create_project(path = path, open = TRUE, rstudio = TRUE)
    }
  } else { # make paper project with package infrastructure
    if (length(list.files(path = path, pattern = "\\.Rproj$")) == 0) {
      usethis::create_package(path = path, open = TRUE, rstudio = TRUE)
    }
  }

  # Paths to gist files for analysis - these need to update of the gist changes.
  
  # path to analysis.Rmd without example
  gist_path_rmd <- paste0(
    "https://gist.github.com/RaymondBalise/ef56efda4a9260d8415a2cde94cbad1b/",
    "raw/c159912e48a2d93efb4d501d5a1d9d5a288ef38d/analysis.Rmd"
  )
  
  # path to analysis.qmd without example
  gist_path_qmd <- paste0(
    "https://gist.githubusercontent.com/RaymondBalise/",
    "224f0b7b107a6b800c610d46c8b6f236/raw/",
    "51f8e2cde1026fa2b376f99aa06244382c3a1bcb/analysis.qmd"
  )

  # path to analysis.Rmd with example
  gist_w_ex_path_rmd <- paste0(
    "https://gist.githubusercontent.com/RaymondBalise/",
    "c8399e7b3474a6022eeae373d059a042/",
    "raw/cf9080a2d2096ddde5f5550abbb6c790126f7d10/analysis_with_example.Rmd"
  )
  
  # path to analysis.qmd with example
  gist_w_ex_path_qmd <- paste0(
    "https://gist.githubusercontent.com/RaymondBalise/",
    "40e8e1cc0dec94b225b7cb307f4fa959/raw/",
    "d4883d98163f565915e514d96d3e4c9fcdda1405/analysis_with_example.qmd"
  )
  
 
  # Prevent user from overwriting an analysis file
  if (file.exists(paste0(path, "/analysis.Rmd")) ||
      file.exists(paste0(path, "/analysis.qmd"))
  ) {
    abort(
      "The directory you choose already has an analysis file. Stopping."
    )
  }

  if (vignette == TRUE) {
    vig_path = "/vignettes"
    dir.create(paste0(path, vig_path), recursive = TRUE, showWarnings = FALSE)
  } else {
    vig_path = NULL
  }
  
  
  if (example == FALSE){ # use old templates w/o an example
    if (type == "R Markdown (analysis.Rmd)") {
      download.file(gist_path_rmd, paste0(path, vig_path, "/analysis.Rmd"))
    } else if (type == "Quarto (analysis.qmd)") {
      download.file(gist_path_qmd, paste0(path, vig_path, "/analysis.qmd"))
    } else {
      abort(
        "The type must be 'R Markdown (analysis.Rmd)' or 'Quarto (analysis.qmd)'"
      )
    }
  } else { # use newer templates w an example
    if (type == "R Markdown (analysis.Rmd)") {
      download.file(gist_w_ex_path_rmd, paste0(path, vig_path, "/analysis.Rmd"))
    } else if (type == "Quarto (analysis.qmd)") {
      download.file(gist_w_ex_path_qmd, paste0(path, vig_path, "/analysis.qmd"))
    } else {
      abort(
        "The type must be 'R Markdown (analysis.Rmd)' or 'Quarto (analysis.qmd)'"
      )
    }
  }
  
  dir.create(paste0(path, "/data"), recursive = TRUE, showWarnings = FALSE)

  # Path to gitignore this must be updated if the gist changes.
  gist_path_ignore <- paste0(
    "https://gist.githubusercontent.com/RaymondBalise/",
    "300d99c2b6450feda3ed5a816f396191/raw/",
    "c959571e2618ba6baa14d91972f84de20a08b63f/.gitignore"
  )
  download.file(gist_path_ignore, paste0(path, "/.gitignore"))

  # write an empty packages bibliography file - needed to knit the first time
  writeLines("", con = file.path(paste0(path, vig_path, "/packages.bib")))

  # write an empty user bibliography file
  writeLines("", con = file.path(paste0(path, vig_path, "/references.bib")))

  download.file(
    "https://www.zotero.org/styles/the-new-england-journal-of-medicine",
    paste0(path, vig_path, "/the-new-england-journal-of-medicine.csl")
  )

  download.file(
    "https://www.zotero.org/styles/apa",
    paste0(path, vig_path, "/apa.csl")
  )
  
   if (vignette == TRUE){ 
    if (type == "R Markdown (analysis.Rmd)") {
      file.copy(
        system.file(
          "gists/manual_change_rmd_vigette.R", 
          package = "rUM"
        ), 
        paste0(path, "/run_me.R")
      )
    } else if (type == "Quarto (analysis.qmd)") {
      file.copy(
        system.file(
          "gists/manual_change_qmd_vigette.R", 
          package = "rUM"
        ), 
        paste0(path, "/run_me.R")
      )
    }
  } 
  
  

}
