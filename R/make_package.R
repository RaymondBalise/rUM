#' Make an Analysis Project
#'
#' @description This function creates the structure for an R package. An analysis.Qmd (or
#' analysis.Rmd) is created in the vignettes directory. This
#' project includes an aggressive .gitignore which is designed to
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
#' @note If you need to create multiple packages from the same session, restart
#'   RStudio/Positron after making each one. See [open issue](https://github.com/RaymondBalise/rUM/issues/84).
#'
#' @return Returns nothing.  See description above.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # This makes a package with an example Quarto paper in the vignettes folder.
#'   make_package(path = "~/test", type = "Quarto (analysis.qmd)", example = TRUE)
#'   
#'   # make_package() allows abbreviations on the project type: "Q" for Quarto or "R" for R Markdown
#'   make_package(path = "~/test_project", "Q", TRUE)
#'   
#'   # This makes a package with an example R Markdown paper in the vignettes folder.
#'   make_package(path = "~/test", type = "R Markdown (analysis.Rmd)", example = TRUE)
#'               
#'   # This makes a project with an example paper in the vignettes folder.
#'   make_package(path = "~/test_project", "R", example = TRUE)
#' }

make_package <- function(
    path,
    type = c("Quarto (analysis.qmd)", "R Markdown (analysis.Rmd)"),
    example = FALSE,
    overwrite = FALSE,
    openInteractive = TRUE
) {

  ######################################################################################
  # rUM::make_project(
  #   path = path,
  #   type = type,
  #   example = example,
  #   vignette = TRUE,
  #   overwrite = overwrite,
  #   openInteractive = TRUE
  # )
  ######################################################################################

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
  if (!is.logical(overwrite)) stop('Parameter `overwrite` must be TRUE or FALSE')
  if (!is.logical(openInteractive)) {
    stop('Parameter `openInteractive` must be TRUE or FALSE')
  }

  # 3) Check directory name; we don't want to create an empty
  #    directory if the package name is invalid.
  #    code from: https://github.com/r-lib/usethis/blob/main/R/description.R
  #    The name will be checked only for packages; it's "valid" otherwise
  dir_name_char <- stringr::word(path, -1, sep = "[\\|\\/]")
  valid_name_lgl <- .valid_package_name(path = dir_name_char)
  
  
  # 4) Get version of Quarto on the machine and save it as a version
  the_version <- quarto::quarto_version()
  
  # 5) Is there a .Rproj in the provided path?
  has_rproj <- length(list.files(path = path, pattern = "\\.Rproj$")) > 0

  # 6) Make paper project with package infrastructure
  #    Quarto version 1.4.549 was the first to allow the building of vignettes
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

  # 7) Validation checks before creating directories & files
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
  
  
  # Create vignette folder.
  # Packages require that the "analysis.*md" file is located in the vignettes directory.
  # `updated_path` will look like: path/vignettes
  updated_path <- file.path(path, "vignettes")
  dir.create(updated_path, recursive = TRUE, showWarnings = FALSE)
  
  
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
  # Why we are NOT including write_scss() in packages:
  # https://cran.r-project.org/web/packages/quarto/vignettes/hello.html
  
  # 1) Create the data directory in the project's root
  dir.create(file.path(path, "data"), recursive = TRUE, showWarnings = FALSE)
  
  # 2) Add enhanced .gitignore from inst/gists
  .add_gitignore(path = path)

  # 3) Add project documentation files
  rUM::write_readme(path = path)
  rUM::write_notes(path = path)

  # 4) Add .bib & .csl files
  .add_citation_files(path = updated_path)

  # 5) Modify DESCRIPTION, main vignette template, and other package files
  .rUM_package_builder(path, is_quarto_project)
  
}
