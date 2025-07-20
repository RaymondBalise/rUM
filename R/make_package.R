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
#' @importFrom quarto quarto_version
#' @importFrom rlang abort
#' @importFrom stringr word
#' @importFrom usethis create_package
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
  is_quarto_project <- .arg_validation(path, type, example, overwrite, openInteractive)

  # 1) Check directory name; we don't want to create an empty
  #    directory if the package name is invalid.
  #    code from: https://github.com/r-lib/usethis/blob/main/R/description.R
  #    The name will be checked only for packages; it's "valid" otherwise
  dir_name_char <- word(path, -1, sep = "[\\|\\/]")
  valid_name_lgl <- .valid_package_name(path = dir_name_char)
  
  
  # 2) Get version of Quarto on the machine and save it as a version
  the_version <- quarto_version()
  
  # 3) Is there a .Rproj in the provided path?
  has_rproj <- length(list.files(path = path, pattern = "\\.Rproj$")) > 0

  # 4) Make paper project with package infrastructure
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

  # 5) Validation checks before creating directories & files
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
    create_package(path = path, open = openInteractive, rstudio = TRUE)
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
  write_readme(path = path)
  write_notes(path = path)

  # 4) Add .bib & .csl files
  .add_citation_files(path = updated_path)

  # 5) Modify DESCRIPTION, main vignette template, and other package files
  .rUM_package_builder(path, is_quarto_project)
  
}


#----------------------------------------------------------------------------------------
# Helper functions for make_package()
#----------------------------------------------------------------------------------------
#' Check for valid project name
#' @description
#' This helper function parses the provided rUM::make_project or rUM::make_package path
#' argument.
#' 
#' @param path Character. Path supplied by user for location of new rUM project.
#' 
#' @importFrom stringr str_detect
#' 
#' @noRd
.valid_package_name <- function(path) {
  str_detect(path, "^[a-zA-Z][a-zA-Z0-9.]+$") && !str_detect(path, "\\.$")
}


#----------------------------------------------------------------------------------------
#' Build and convert to package
#' @description
#' This helper functon will convert an ordinary rUM project into a rUM package. It 
#' modifies the vignettes/analysis.* YAML header with content appropriate to build 
#' the respective vignette using the correct engine.
#' 
#' This was the previously-named internal function \code{.run_me_first()}.
#' 
#' @param path Character. Path where to store the document template, either project root
#' or vignettes folder for package projects.
#' @param is_quarto_project Logical. Determines Quarto versus RMarkdown project.
#' 
#' @noRd
.rUM_package_builder <- function(path, is_quarto_project) {

  .add_notes_to_Rbuildignore(path = path)

  # We need to save the current working directory path & then move to the new project
  # location in order to add & modify files THERE. Once those tasks are complete, we
  # will then return to the `current_wd` (where the user is initiating the project or 
  # package creation).
  # Capture current directory and return to it at the end of this function
  current_wd <- getwd()
  # Move to new project location
  setwd(path)
  
  .rUM_build_description(is_quarto_project = is_quarto_project)

  # Create vignettes/.gitignore & write "*.html" & "*.R"
  writeLines("*.html\n*.R", con = "vignettes/.gitignore")

  # Add inst folder & .gitkeep file
  dir.create("inst", recursive = TRUE)
  writeLines("# Do not edit by hand", con = "inst/.gitkeep")
  # Needed to prevent devtools::check() warning:
  cat("inst/.gitkeep\n", file = ".Rbuildignore", append = TRUE)

  # Alter document template YAML for vignette builder
  .rUM_modify_for_vignette(is_quarto_project = is_quarto_project)

  # Return to original location where rUM::make_project() was executed. See above comment.
  setwd(current_wd)

}


#----------------------------------------------------------------------------------------
#' Add dated_progress_notes file
#' @description
#' This helper functon will add the dated progress notes template
#' 
#' @param path Character. Path to project root directory.
#' 
#' @importFrom usethis ui_done
#' 
#' @noRd
.add_notes_to_Rbuildignore <- function(path) {
  cat(
    "dated_progress_notes.md\n", 
    file = file.path(path, ".Rbuildignore"),
    append = TRUE # add, don't overwrite current file
  )
  ui_done("dated_progress_notes.md has been added to the .Rbuildignore.")
}


#----------------------------------------------------------------------------------------
#' Add content to the DESCRIPTION file
#' @description
#' This helper functon will add appropriate package content to the DESCRIPTION file. It
#' uses \code{usethis::use_package()} syntax to build the DESCRIPTION file and adds the
#' VignetteBuilder to the DESCRIPTION file.
#' 
#' @param path Character. Path where to store the document template, either project root
#' or vignettes folder for package projects.
#' @param is_quarto_project Logical. Determines Quarto versus RMarkdown project.
#' 
#' @importFrom usethis use_package
#' 
#' @noRd
.rUM_build_description <- function(is_quarto_project) {
  # Add quietly to DESCRIPTION:
  suppressMessages({
    use_package("here", type = "suggests")
    use_package("knitr", type = "suggests")
    use_package("rmarkdown", type = "suggests")
    use_package("roxygen2", type = "suggests")
    
    use_package("conflicted", type = "suggests")
    use_package("glue", type = "suggests")
    use_package("gtsummary", type = "suggests", min_version = "2.0.3")
    use_package("rUM", type = "suggests")
    use_package("rio", type = "suggests")
    use_package("table1", type = "suggests")
    use_package("tidymodels", type = "suggests")
    use_package("tidyverse", type = "suggests")
  })

  # Add Vignette builder to DESCRIPTION:
  if (is_quarto_project) {
    cat(
      "VignetteBuilder: quarto\n", 
      file = file.path("DESCRIPTION"),
      append = TRUE # add, don't overwrite current file
    )
    # Add minimal Quarto version:
    suppressMessages({
      use_package("quarto", type = "suggests", min_version = "1.3.12")
    })
  } else {  # Rmd project
    cat(
      "VignetteBuilder: knitr\n", 
      file = file.path("DESCRIPTION"),
      append = TRUE # add, don't overwrite current file
    )
  }
}


#----------------------------------------------------------------------------------------
#' Modify the YAML header to write a vignette
#' @description
#' This helper functon will modify the YAML structure for the document template. It will
#' evaluate the operating system and apply the appropriate new line syntax. Due to the 
#' differences in Unix-based systems using \code{"\n"} versus Windows syntax to use
#' \code{"\r\n"}, the search & replace must be carefully applied.
#' 
#' @param is_quarto_project Logical. Determines Quarto versus RMarkdown project.
#' 
#' @importFrom readr read_file write_file
#' @importFrom stringr str_replace str_replace_all
#' 
#' @noRd
.rUM_modify_for_vignette <- function(is_quarto_project) {
  
  # Setup OS-specific string parsing--------------------------------------------------
  # Unix-based OS's use a end of line return like "\n"
  # Windows uses a carriage return and line break like "\r\n"
  # This section will adjust the YAML pattern to be read and replaced by inserting
  # the appropriate line return items if on Windows:


  # Original YAML content to be replaced
  qmd_pattern <- "format:\n  html:\n    embed-resources: true   # true = a single file, false = multiple files\n    theme:\n      - default\n      - custom.scss"

  rmd_pattern <- "output:\n  bookdown::html_document2:\n    number_sections: false\n"

  # rUM will replace the pattern with this to be able write & create package vignettes
  qmd_replacement <- "output: rmarkdown::html_vignette\nvignette: >\n  %\\\\VignetteIndexEntry{your_title_goes_here}\n  %\\\\VignetteEngine{quarto::html}\n  %\\\\VignetteEncoding{UTF-8}"

  rmd_replacement <- "output: rmarkdown::html_vignette\nvignette: >\n  %\\\\VignetteIndexEntry{your_title_goes_here}\n  %\\\\VignetteEngine{knitr::rmarkdown}\n  %\\\\VignetteEncoding{UTF-8}\n"

  # Apply the OS-conditional change
  if (.Platform$OS.type == 'windows') {
    qmd_pattern <- str_replace_all(qmd_pattern, '\n', '\r\n')
    qmd_replacement <- str_replace_all(qmd_replacement, '\n', '\r\n')
    
    rmd_pattern <- str_replace_all(rmd_pattern, '\n', '\r\n')
    rmd_replacement <- str_replace_all(rmd_replacement, '\n', '\r\n')
  }
  #-----------------------------------------------------------------------------------

  
  # Append Vignette builder to DESCRIPTION file & modify YAML content
  if (is_quarto_project) { # Quarto project
    
    # Replace the YAML pattern with the new structure for Quarto vignette:
    write_file(
      x = str_replace(
        string = read_file("vignettes/analysis.qmd"),
        pattern = qmd_pattern,
        replacement = qmd_replacement
      ), 
      file = "vignettes/analysis.qmd"
    )

  } else { # Rmd project
    
    # Replace the YAML pattern with the new structure for Rmd vignette:
    write_file(
      x = str_replace(
        string = read_file("vignettes/analysis.Rmd"),
        pattern = rmd_pattern,
        replacement = rmd_replacement
        ), 
      file = "vignettes/analysis.Rmd"
    )
  }
}
