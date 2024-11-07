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
  
  #############################################################################
  # This section creates the project directory and adds the appropriate files #
  #############################################################################
  
  # Prevent user from overwriting an analysis file
  if (file.exists(paste0(path, "/analysis.Rmd")) ||
      file.exists(paste0(path, "/analysis.qmd"))
  ) {
    abort(
      "The directory you choose already has an analysis file. Stopping."
    )
  }
  
  # Create vignette folder
  if (vignette == TRUE) {
    vig_path = "/vignettes"
    dir.create(paste0(path, vig_path), recursive = TRUE, showWarnings = FALSE)
  } else {
    vig_path = NULL
  }
  
  #############################################################################
  #          Write appropriate Quarto or Rmarkdown analysis file              #
  #############################################################################
  # use old templates w/o an example
  if (example == FALSE){
    if (type == "R Markdown (analysis.Rmd)") {

      invisible(file.copy(
        from = system.file(
          "gists/analysis_rmd_wo_example.Rmd", 
          package = "rUM"
        ),
        to = paste0(path, vig_path, "/analysis.Rmd")
      ))

      # Adding console feedback
      ui_done("analysis.Rmd has been created.")


    } else if (type == "Quarto (analysis.qmd)") {

      invisible(file.copy(
        from = system.file(
          "gists/analysis_qmd_wo_example.qmd", 
          package = "rUM"
        ),
        to = paste0(path, vig_path, "/analysis.qmd")
      ))

      # Adding console feedback
      ui_done("analysis.qmd has been created.")

      # Add custom.scss to project
      write_scss(name = "custom", path = path)

    } else {
      abort(
        "The type must be 'R Markdown (analysis.Rmd)' or 'Quarto (analysis.qmd)'"
      )
    }

  # use newer templates w an example
  } else { 
    if (type == "R Markdown (analysis.Rmd)") {
      
      invisible(file.copy(
        from = system.file(
          "gists/analysis_rmd_with_example.Rmd", 
          package = "rUM"
        ),
        to = paste0(path, vig_path, "/analysis.Rmd")
      ))

      # Adding console feedback
      ui_done("analysis.Rmd has been created.")


    } else if (type == "Quarto (analysis.qmd)") {
      
      invisible(file.copy(
        from = system.file(
          "gists/analysis_qmd_with_example.qmd", 
          package = "rUM"
        ),
        to = paste0(path, vig_path, "/analysis.qmd")
      ))

      # Adding console feedback
      ui_done("analysis.qmd has been created.")

      # Add custom.scss to project
      write_scss(name = "custom", path = path)

    # User did not provide correct argument type:  
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
    "1978fb42fc520ca57f670908e111585e/raw/",
    "e0b0ac8c7726f488fcc52b3b8269e449cbf33c15/.gitignore"
  )
  download.file(
    gist_path_ignore, 
    paste0(path, "/.gitignore"),
    # silence console output: "trying URL..., Content type..., downloaded..."
    quiet = TRUE
  )
  # Adding console feedback
  ui_done("An enhanced .gitignore has been created.")

  ############################################################################
  # Add a README template from inst/gists 
  readme_path <- system.file("gists/README.md", package = "rUM")
  if (readme_path == "") {
    stop("Could not find README template in package installation")
  }
  invisible(file.copy(
    from = readme_path,
    to = paste0(path, "/README.md")
  ))

  # Adding console feedback
  ui_done("A README.md template has been created.")

  # Add dated_progress_notes.md template
  writeLines(
    paste0(
      "# Add project updates here\n", 
      format(Sys.Date(), "%b %d, %Y"),
      ": project started"
    ),
    con = file.path(paste0(path, "/dated_progress_notes.md"))
  )
  # Adding console feedback
  ui_done("A dated_progress_notes.md template has been created.")
  if (vignette == TRUE) {
    cat(
      "dated_progress_notes.md", 
      file = file.path(paste0(path, "/.Rbuildignore")),
      append = TRUE # add, don't overwrite current file
    )
    # Adding console feedback
    ui_done("dated_progress_notes.md has been added to the .Rbuildignore.")  
  }

  # write an empty packages bibliography file - needed to knit the first time
  writeLines("", con = file.path(paste0(path, vig_path, "/packages.bib")))
  
  # write an empty user bibliography file
  writeLines("", con = file.path(paste0(path, vig_path, "/references.bib")))
  
  download.file(
    "https://www.zotero.org/styles/the-new-england-journal-of-medicine",
    paste0(path, vig_path, "/the-new-england-journal-of-medicine.csl"),
    # silence console output: "trying URL..., Content type..., downloaded..."
    quiet = TRUE
  )
  
  download.file(
    "https://www.zotero.org/styles/apa",
    paste0(path, vig_path, "/apa.csl"),
    # silence console output: "trying URL..., Content type..., downloaded..."
    quiet = TRUE
  )
  
  if (vignette == TRUE){ 
    if (type == "R Markdown (analysis.Rmd)") {
      invisible(file.copy(
        system.file(
          "gists/manual_change_rmd_vigette.R", 
          package = "rUM"
        ), 
        paste0(path, "/RUN_ME_FIRST.R")
      ))
    } else if (type == "Quarto (analysis.qmd)") {
      invisible(file.copy(
        system.file(
          "gists/manual_change_qmd_vigette.R", 
          package = "rUM"
        ), 
        paste0(path, "/RUN_ME_FIRST.R")
      ))
    }
  } 
  
  ### EXPERIMENTAL ###
  # source(paste0(path, "/RUN_ME_FIRST.R"))
  
}

.valid_package_name <- function(x) {
  grepl("^[a-zA-Z][a-zA-Z0-9.]+$", x) && !grepl("\\.$", x)
}

