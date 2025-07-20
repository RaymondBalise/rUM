# Collection of rUM helper functions: ---------------------------------------------------

#' Argument validation
#' @description
#' Argument validation function for rUM functions
#' 
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
#' @return Logical. Evaluation of `is_quarto_project`.
#' 
#' @noRd
.arg_validation <- function(type, example, overwrite, openInteractive) {

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

  return(is_quarto_project)
}


#----------------------------------------------------------------------------------------
#' Path validation
#' 
#' @description
#' Validate path to output file(s) specified by user
#' 
#' @param path Character string.
#' 
#' @return A `stop` message if errors exist.
#' 
#' @noRd
.validate_path <- function(path) {
  if (is.null(path) || !dir.exists(path)) {
    stop("Invalid `path`. Please enter a valid project directory.")
  }
}


#----------------------------------------------------------------------------------------
#' Filename validation
#' 
#' @description
#' Validate path to output file(s) specified by user
#' 
#' @param name Character string.
#' 
#' @return A `stop` message if errors exist.
#' 
#' @noRd
.validate_filename <- function(name) {
  if (!is.character(name)) stop('Invalid filename: must be character.')
  
  if (str_detect(name, "/")) {
    stop(
      'Invalid filename. You included a forward slash in the file name. If you are trying to give a folder/directory location, use the `path =` argument.'
    )
  }
  if (str_detect(name, "\\\\")) {
    stop(
      'Invalid filename. You included a backslash in the file name. If you are trying to give a folder/directory location, use the `path =` argument.'
    )
  }
  if (!str_detect(name, '^[a-zA-Z0-9_-]+$')) {
    stop(
      'Invalid filename. Use only letters, numbers, hyphens, and underscores.'
    )
  }
}


#----------------------------------------------------------------------------------------
#' Add Quarto document
#' @description
#' This helper function adds the Quarto document to the supplied path. It is added to the
#' project root directory for non-package projects, or is created in the vignettes 
#' directory.
#' 
#' @param example Logical. Include an example of how to link table & plot from within the 
#' document.
#' @param path Character. Path where to store the document template, either project root
#' or vignettes folder for package projects.
#' 
#' @importFrom usethis ui_done
#' 
#' @noRd
.add_quarto_doc <- function(example, path) {
  if (example) {
    invisible(file.copy(
      from = system.file("gists/analysis_qmd_with_example.qmd", package = "rUM"),
      to = file.path(path, "analysis.qmd")
    ))
  } else {
    invisible(file.copy(
      from = system.file("gists/analysis_qmd_wo_example.qmd", package = "rUM"),
      to = file.path(path, "analysis.qmd")
    ))
  }

  # Provide user feedback
  ui_done("analysis.qmd has been created.")
}


#----------------------------------------------------------------------------------------
#' Add Rmarkdown document
#' @description
#' This helper function adds the Rmarkdown document to the supplied path. It is added to 
#' the project root directory for non-package projects, or is created in the vignettes 
#' directory.
#' 
#' @inheritParams .add_quarto_doc
#' 
#' @importFrom usethis ui_done
#' 
#' @noRd
.add_rmd_doc <- function(example, path) {
  if (example) {
    invisible(file.copy(
      from = system.file("gists/analysis_rmd_with_example.Rmd", package = "rUM"),
      to = file.path(path, "analysis.Rmd")
    ))
  } else {
    invisible(file.copy(
      from = system.file("gists/analysis_rmd_wo_example.Rmd", package = "rUM"),
      to = file.path(path, "analysis.Rmd")
    ))
  }

  # Provide user feedback
  ui_done("analysis.Rmd has been created.")
}


#----------------------------------------------------------------------------------------
#' Add .gitignore file
#' @description
#' This helper functon will add the improved .gitignore file from rUM inst/gists
#' 
#' @param path Character. Path to project root directory.
#' 
#' @importFrom usethis ui_done
#' 
#' @noRd
.add_gitignore <- function(path) {
  ign_path <- system.file("gists/aggressive_gitignore.md", package = "rUM")
  if (ign_path == "") stop("Could not find .gitignore in package installation")
  
  the_gitignore_path <- file.path(path, ".gitignore")
  invisible({
    # Remove default .gitingore provided by `usethis::create_package()`
    if (file.exists(the_gitignore_path)) file.remove(the_gitignore_path)
    # Replace .gitignore
    file.copy(from = ign_path, to = the_gitignore_path)
  })
  ui_done("An enhanced .gitignore has been created.")
}


#----------------------------------------------------------------------------------------
#' Add citation and referencess file
#' @description
#' This helper functon will add .bib and .csl files to the supplied path. It is added to 
#' the project root directory for non-package projects, or is created in the vignettes 
#' directory.
#' 
#' @param path Character. Path where to store the document template, either project root
#' or vignettes folder for package projects.
#' 
#' @importFrom utils download.file
#' 
#' @noRd
.add_citation_files <- function(path) {

  writeLines("", con = file.path(path, "packages.bib"))
  
  # 7. Write an empty user bibliography file
  writeLines("", con = file.path(path, "references.bib"))
  
  # 8. Add .csl files
  download.file(
    "https://www.zotero.org/styles/the-new-england-journal-of-medicine",
    file.path(path, "the-new-england-journal-of-medicine.csl"),
    quiet = TRUE
  )
  download.file(
    "https://www.zotero.org/styles/apa",
    file.path(path, "apa.csl"),
    quiet = TRUE
  )

}
