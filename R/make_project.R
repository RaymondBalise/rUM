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
#' @importFrom lifecycle deprecate_warn
#' @importFrom quarto quarto_version
#' @importFrom rlang abort
#' @importFrom usethis create_project
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
  if (!is.logical(vignette))  stop('Parameter `vignette` must be TRUE or FALSE')
  is_quarto_project <- .arg_validation(path, type, example, overwrite, openInteractive)

  # If the user chooses make_project() to create a R package, warn & continue:
  if (vignette == TRUE) {
    # Deprecation warning for version >= 2.2.0
    deprecate_warn(
      when = "2.2.0", 
      what = "make_project(vignette)", 
      with = "make_package()"
    )
    # Using make_package() here to continue initializing their package project. This
    # section will be deprecated in the future.
    make_package(
      path = path,
      type = type,
      example = example,
      overwrite = overwrite,
      openInteractive = openInteractive
    )
    
  # Once the vignette argument is phased out, remove the make_package() section above
  # and keep everything contained within this "else" section. Also, fix indentation:
  } else {
    
    # 3) Create directory and perform initial project setup
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  
    # get version of Quarto on the machine and save it as a version
    the_version <- quarto_version()
    # Is there a .Rproj in the provided path?
    has_rproj <- length(list.files(path = path, pattern = "\\.Rproj$")) > 0

    # If the project object does not exist add it.
    if (!has_rproj) {
      # Prior to version 2.0.0, the option `open` was set to TRUE, which worked
      #   as intended interactively, but spawned duplicate RStudio project 
      #   windows when called from RStudio menus via the `research_project.dcf`
      #   file. This is now TRUE by default for interactive calls to this 
      #   function, but set to FALSE via the .dcf file for menu-driven calls.
      create_project(
        path = path, open = openInteractive, rstudio = TRUE
      )
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
  
    #############################################################################
    #          Add template with or without included example?                   #
    #############################################################################
    if (is_quarto_project) {
      .add_quarto_doc(example = example, path = path)
    } else {
      .add_rmd_doc(example = example, path = path)
    }

    #############################################################################
    #            Add project directories and supplemental files                 #
    #############################################################################
    
    # 1. Create the data directory in the project's root
    dir.create(file.path(path, "data"), recursive = TRUE, showWarnings = FALSE)
    
    # 2. Create custom.scss
    if (is_quarto_project) write_scss(name = "custom", path = path) 

    # 3. Add enhanced .gitignore from inst/gists
    .add_gitignore(path = path)

    # 4. Add project documentation files
    write_readme(path = path)
    write_notes(path = path)

    # 5. Add .bib & .csl files
    .add_citation_files(path = path)

  }

}
