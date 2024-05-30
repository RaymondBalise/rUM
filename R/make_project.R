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
#' @param example Will the analysis file include an example table/figure?
#' @param vignette Will the analysis file be saved as a package vignette?
#' @param overwrite Will an existing RStudio project be overwritten?  This is 
#' needed for for Posit.Cloud.  You will be prompted to confirm this option.
#' @param openInteractive Should this new project be opened in a new RStudio
#'   window? Defaults to \code{TRUE}. NOTE: this option exists to prevent 
#'   RStudio from opening two duplicate versions of the new project when
#'   this function is executed from RStudio menus. MODIFY WITH CAUTION.
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
#'   # This makes a project with an example Quarto paper in the project's folder.
#'   make_project(path = "~/test", type = "Quarto (analysis.qmd)", example = TRUE, vignette = TRUE)
#'   # make_project() allows abbreviations on the project type: "Q" for Quarto or "R" for R Markdown
#'   make_project(path = "~/test_project", "Q", TRUE, TRUE)
#'   
#'   # This makes a project with an example R Markdown paper in the project's folder.
#'   make_project(path = "~/test", type = "R Markdown (analysis.Rmd)", example = TRUE, vignette = TRUE)
#'   # This makes a project with an example paper in the project's folder.
#'   make_project(path = "~/test_project", "R", example = TRUE)
#' }

make_project <- function(
    path,
    type = c("Quarto (analysis.qmd)", "R Markdown (analysis.Rmd)"),
    example = FALSE,
    vignette = FALSE,
    overwrite = FALSE,
    openInteractive = TRUE
) {
  
  type <- match.arg(type)
  # ensure path exists
  
  # check directory name (for packages only); we don't want to create an empty
  #   directory if the package name is invalid.
  # code from: https://github.com/r-lib/usethis/blob/main/R/description.R
  # The name will be checked only for packages; it's "valid" otherwise
  if (vignette == TRUE) {
    dir_name_char <- stringr::word(path, -1, sep = "[\\|\\/]")
    valid_name_lgl <- .valid_package_name(dir_name_char)
  } else {
    valid_name_lgl <- TRUE
  }
  if (vignette != TRUE && valid_name_lgl) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  
  
  # get version of Quarto on the machine and save it as a version
  the_version <- quarto::quarto_version()
  if (vignette == FALSE){ # make paper project w/o package infrastructure
    # If the project object does not exist add it.
    if (length(list.files(path = path, pattern = "\\.Rproj$")) == 0) {
      # Prior to version 2.0.0, the option `open` was set to TRUE, which worked
      #   as intended interactively, but spawned duplicate RStudio project 
      #   windows when called from RStudio menus via the `research_project.dcf`
      #   file. This is now TRUE by default for interactive calls to this 
      #   function, but set to FALSE via the .dcf file for menu-driven calls.
      usethis::create_project(
        path = path, open = openInteractive, rstudio = TRUE
      )
    }
  } else { # make paper project with package infrastructure
    # Quarto version 1.4.549 was the first to allow the building of vignettes
    if (type == "Quarto (analysis.qmd)" & the_version < "1.4.549"){
      message(
        paste0(
          "STOPPING: You need a modern version of Quarto from ", 
          "https://quarto.org/docs/download/ in order to make the package ",
          "with a Quarto vignette."
        )
      )
      return(invisible(NULL))
    }
    
    if (length(list.files(path = path, pattern = "\\.Rproj$")) > 0 & 
        !(overwrite == TRUE)
      ){
      message(
        paste0(
          "STOPPING: The folder/directory you chose to hold the package ",
          "already has a RStudio project file (.Rproj) in it. If you want to ",
          "create a package in a directory/folder that already has an RStudio ",
          "project in it, add the overwrite = TRUE option when you use ",
          "make_project()."
        )
      )
      return(invisible(NULL))
    }
    
    if (
      length(list.files(path = path, pattern = "\\.Rproj$")) == 0 | 
      overwrite == TRUE
    ) {
      usethis::create_package(path = path, open = TRUE, rstudio = TRUE)
    }
  }
  
  # Paths to gist files for analysis - these need to update of the gist changes.
  
  # path to analysis.Rmd without example
  gist_path_rmd <- paste0(
    "https://gist.github.com/RaymondBalise/ef56efda4a9260d8415a2cde94cbad1b/",
    "raw/83194521c2475033458f2ea6b45b20f6f292a0d0/analysis.Rmd"
  )
  
  # path to analysis.qmd without example
  gist_path_qmd <- paste0(
    "https://gist.githubusercontent.com/RaymondBalise/",
    "224f0b7b107a6b800c610d46c8b6f236/raw/",
    "b417b2fe260af6377a2fb1cbc06b10fd07da29ee/analysis.qmd"
  )
  
  # path to analysis.Rmd with example
  gist_w_ex_path_rmd <- paste0(
    "https://gist.githubusercontent.com/RaymondBalise/",
    "c8399e7b3474a6022eeae373d059a042/",
    "raw/54059a3c9109efe79bf31010f32d4e99cf6469ea/analysis_with_example.Rmd"
  )
  
  # path to analysis.qmd with example
  gist_w_ex_path_qmd <- paste0(
    "https://gist.githubusercontent.com/RaymondBalise/",
    "40e8e1cc0dec94b225b7cb307f4fa959/raw/",
    "2add2f029b640e31c3d4a755c690f6dd62e84a5e/analysis_with_example.qmd"
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
        paste0(path, "/RUN_ME_FIRST.R")
      )
    } else if (type == "Quarto (analysis.qmd)") {
      file.copy(
        system.file(
          "gists/manual_change_qmd_vigette.R", 
          package = "rUM"
        ), 
        paste0(path, "/RUN_ME_FIRST.R")
      )
    }
  } 
  
  
  
}

.valid_package_name <- function(x) {
  grepl("^[a-zA-Z][a-zA-Z0-9.]+$", x) && !grepl("\\.$", x)
}

