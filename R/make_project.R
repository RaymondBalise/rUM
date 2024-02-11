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
#'
#' @return Returns nothing.  See description above.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   make_project(path = "~/test_project", type = "Quarto (analysis.qmd)")
#' }
#' \dontrun{
#'   make_project(path = "~/test_project", type = "R Markdown (analysis.Rmd)")
#' }

make_project <- function(
    path,
    type = c("Quarto (analysis.qmd)", "R Markdown (analysis.Rmd)"),
    example = FALSE
  ) {

  type <- match.arg(type)
  # ensure path exists
  dir.create(path, recursive = TRUE, showWarnings = FALSE)


  # If the project object does not exist add it.
  if (length(list.files(path = path, pattern = "\\.Rproj$")) == 0) {
    usethis::create_project(path = path, open = TRUE, rstudio = TRUE)
  }

  # Paths to gist files for analysis - these need to update of the gist changes.
  gist_path_rmd <- paste0(
    "https://gist.github.com/RaymondBalise/ef56efda4a9260d8415a2cde94cbad1b/",
    "raw/b394a5ad7e90e54161e1105ecd770e60e4592456/analysis.Rmd"
  )
  
  gist_path_qmd <- paste0(
    "https://gist.githubusercontent.com/RaymondBalise/",
    "224f0b7b107a6b800c610d46c8b6f236/raw/",
    "ee3369492aafd001b59d4390177e8b44cd0ea088/analysis.qmd"
  )

  gist_w_ex_path_rmd <- paste0(
    "https://gist.githubusercontent.com/RaymondBalise/",
    "c8399e7b3474a6022eeae373d059a042/",
    "raw/094e854be27554e4e296c48e4323c4f0d1b5ba9c/analysis_with_example.Rmd"
  )
  
  gist_w_ex_path_qmd <- paste0(
    "https://gist.githubusercontent.com/RaymondBalise/",
    "40e8e1cc0dec94b225b7cb307f4fa959/raw/",
    "f365198b41abe5b35ec7d7501d6f286f668a14d7/analysis_with_example.qmd"
  )
  
 
  # Prevent user from overwriting an analysis file
  if (file.exists(paste0(path, "/analysis.Rmd")) ||
      file.exists(paste0(path, "/analysis.qmd"))
  ) {
    abort(
      "The directory you choose already has an analysis file. Stopping."
    )
  }

  
  if (example == FALSE){ # use old templates w/o an example
    if (type == "R Markdown (analysis.Rmd)") {
      download.file(gist_path_rmd, paste0(path, "/analysis.Rmd"))
    } else if (type == "Quarto (analysis.qmd)") {
      download.file(gist_path_qmd, paste0(path, "/analysis.qmd"))
    } else {
      abort(
        "The type must be 'R Markdown (analysis.Rmd)' or 'Quarto (analysis.qmd)'"
      )
    }
  } else { # use newer templates w an example
    if (type == "R Markdown (analysis.Rmd)") {
      download.file(gist_w_ex_path_rmd, paste0(path, "/analysis.Rmd"))
    } else if (type == "Quarto (analysis.qmd)") {
      download.file(gist_w_ex_path_qmd, paste0(path, "/analysis.qmd"))
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
    "b9ab7d1ecf503bad68a3f9cca98db397b7afd2b9/.gitignore"
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
