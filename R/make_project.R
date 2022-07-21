#' Make an Analysis Project
#' 
#' @description This function makes an R project that includes an analysis.Rmd 
#' or analysis.qmd file with conflicted and tidyverse and an aggressive 
#' .gitignore.  The  .gitignore is designed to help protect against leaking data
#' (with protected  health information). This function is used by the RStudio
#' research_project.dcf file to make the files.
#'
#' @param path Path automatically set by research_project.dcf (see
#'    \code{./rstudio/templates/project/})
#' @param type Choose between "Quarto (analysis.qmd)") or 
#'    "R Markdown (analysis.Rmd)"(see \code{./rstudio/templates/project/})
#'
#' @import tidyverse 
#' @import conflicted
#' @importFrom rlang abort
#' @importFrom utils download.file
#' @importFrom usethis create_project
#'
#' @return Creates a project directory with the following contents: a template
#'    \code{.Rmd} file called "analysis", a subdirectory for data, a template
#'    \code{.gitignore} with aggressive protections against publishing potential
#'    protected health information, a starter bibliography file called 
#'    "references" (in standard \code{.bib} format), and a stock Citation Style
#'    Language (\code{.csl}) file for the New England Journal of Medicine.
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
    type = c("Quarto (analysis.qmd)", "R Markdown (analysis.Rmd)")
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
    "raw/a94aef9bd34e31218aeebeea06513db08cdb3ee6/analysis.Rmd"
  )
  gist_path_qmd <- paste0(
    "https://gist.githubusercontent.com/RaymondBalise/",
    "224f0b7b107a6b800c610d46c8b6f236/raw/",
    "b220bf3004822ac30632fa7cce4ab9acec05a3d2/analysis.qmd"
  )
  
  # Prevent user from overwriting an analysis file
  if (file.exists(paste0(path, "/analysis.Rmd")) | 
      file.exists(paste0(path, "/analysis.qmd"))   
  ) {
    abort(
      "The directory you choose already has an analysis file. Stopping."
    )
  } 
  
  if (type == "R Markdown (analysis.Rmd)") {
    download.file(gist_path_rmd, paste0(path, "/analysis.Rmd"))
  } else if (type == "Quarto (analysis.qmd)") {
    download.file(gist_path_qmd, paste0(path, "/analysis.qmd"))
  } else {
    abort(
      "The type must be 'R Markdown (analysis.Rmd)' or 'Quarto (analysis.qmd)'"
    )
  }
  
  dir.create(paste0(path, "/data"), recursive = TRUE, showWarnings = FALSE)
  
  # Path to gitignore this must be updated if the gist changes.
  gist_path_ignore <- paste0(
    "https://gist.githubusercontent.com/RaymondBalise/",
    "300d99c2b6450feda3ed5a816f396191/raw/",
    "34298c16ff3827e3b455a5ef9a69ca7b198e1f9e/.gitignore"
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
