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
#' @return Returns nothing.  See description above.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # This makes a project with an example Quarto paper in the project's folder.
#'   make_project(path = "~/test", type = "Quarto (analysis.qmd)", 
#'               example = TRUE, vignette = TRUE)
#'   
#'   # make_project() allows abbreviations on the project type: "Q" for Quarto or "R" for R Markdown
#'   make_project(path = "~/test_project", "Q", TRUE, TRUE)
#'   
#'   # This makes a project with an example R Markdown paper in the project's folder.
#'   make_project(path = "~/test", type = "R Markdown (analysis.Rmd)", 
#'               example = TRUE, vignette = TRUE)
#'               
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

  # Input validation:--------------------------------------------------------------
  
  # 1) Type matches available options
  type <- match.arg(type)
  is_quarto_project <- type == "Quarto (analysis.qmd)"
  is_markdown_project <- type == "R Markdown (analysis.Rmd)"
  if (!is_quarto_project && !is_markdown_project) {
    abort("The type must be 'R Markdown (analysis.Rmd)' or 'Quarto (analysis.qmd)'")
  }

  # 2) Check logical inputs
  if (!is.logical(example))   stop('Parameter `example` must be TRUE or FALSE')
  if (!is.logical(vignette))  stop('Parameter `vignette` must be TRUE or FALSE')
  if (!is.logical(overwrite)) stop('Parameter `overwrite` must be TRUE or FALSE')
  if (!is.logical(openInteractive)) {
    stop('Parameter `openInteractive` must be TRUE or FALSE')
  }

  # 3) Check directory name (for packages only); we don't want to create an empty
  #    directory if the package name is invalid.
  #    code from: https://github.com/r-lib/usethis/blob/main/R/description.R
  #    The name will be checked only for packages; it's "valid" otherwise
  if (vignette) {
    dir_name_char <- stringr::word(path, -1, sep = "[\\|\\/]")
    valid_name_lgl <- .valid_package_name(path = dir_name_char)
  } else {
    valid_name_lgl <- TRUE
  }
  if (!vignette && valid_name_lgl) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  
  
  # get version of Quarto on the machine and save it as a version
  the_version <- quarto::quarto_version()
  # Is there a .Rproj in the provided path?
  has_rproj <- length(list.files(path = path, pattern = "\\.Rproj$")) > 0

  if (!vignette){ # make paper project w/o package infrastructure
    # If the project object does not exist add it.
    if (!has_rproj) {
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
    if (is_quarto_project & the_version < "1.4.549"){
      message(
        paste0(
          "STOPPING: You need a modern version of Quarto from ", 
          "https://quarto.org/docs/download/ in order to make the package ",
          "with a Quarto vignette."
        )
      )
      return(invisible(NULL))
    }

    # browser()
    if (has_rproj & !overwrite){
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
    
    if (!has_rproj | overwrite) {
      usethis::create_package(path = path, open = openInteractive, rstudio = TRUE)
    }
  }

  # Now that the project directory has been successfully created, normalize the
  # path to work across OS and prevent path issues ahead.
  path <- normalizePath(path, mustWork = TRUE)
  
  #############################################################################
  # This section creates the project directory and adds the appropriate files #
  #############################################################################
  
  # Prevent user from overwriting an analysis file
  if (file.exists(file.path(path, "analysis.Rmd")) ||
      file.exists(file.path(path, "analysis.qmd"))
  ) {
    abort("The directory you choose already has an analysis file. Stopping.")
  }
  
  
  # Create vignette folder. If the project will NOT be using vignettes (ie., making
  # a R package), the `updated_path` created below will resolve correctly.
  # Packages require that the "analysis.*md" file is located in the vignettes directory.
  # However, non-packages can store all files in the main project root.
  # When `vignette == TRUE` the `updated_path` will look like: path/vignettes
  # When `vignette == FALSE` the `updated_path` will be equal to path
  if (vignette) {
    updated_path <- file.path(path, "vignettes")
    dir.create(updated_path, recursive = TRUE, showWarnings = FALSE)
  } else {
    updated_path <- path
  }
  
  #############################################################################
  #          Add template with or without included example?                   #
  #############################################################################
  if (is_quarto_project) {
    .add_quarto_doc(example = example, path = updated_path)
  } else {
    .add_rmd_doc(example = example, path = updated_path)
  }

  #############################################################################
  #            Add project directories and supplemental files                 #
  #############################################################################
  # Add custom.scss to Quarto non-package projects only. 
  #  "The minimal default format is a deliberate limitation of the current 
  #  implementaton of the vignette engine. It ensures that the HTML vignettes
  #  produced are reasonable size and can be published on CRAN without problems".
  # source: https://cran.r-project.org/web/packages/quarto/vignettes/hello.html
  
  # 1. Create the data directory in the project's root
  dir.create(file.path(path, "data"), recursive = TRUE, showWarnings = FALSE)
  
  # 2. Create custom.scss
  if (is_quarto_project && !vignette) rUM::write_scss(name = "custom", path = path) 

  # 3. Add enhanced .gitignore from inst/gists
  .add_gitignore(path = path)

  # 4. Add project documentation files
  rUM::write_readme(path = path)
  rUM::write_notes(path = path)

  # 5. Add .bib & .csl files
  .add_citation_files(path = updated_path)

  # 6. Modify DESCRIPTION, main vignette template, and other package files
  if (vignette) .rUM_package_builder(path, is_quarto_project)
  
}
