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

    # browser()
    
    
    has_rproj <- length(list.files(path = path, pattern = "\\.Rproj$")) > 0
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
    abort(
      "The directory you choose already has an analysis file. Stopping."
    )
  }
  
  
  # Create vignette folder. If the project will NOT be using vignettes (ie., making
  # a R package), the `vig_path` created below will resolve correctly.
  # Packages require that the "analysis.*md" file is located in the vignettes directory.
  # However, non-packages can store all files in the main project root.
  # When `vignette == TRUE` the `vig_path` will look like: path/vignettes
  # When `vignette == FALSE` the `vig_path` will be equal to path
  if (vignette == TRUE) {
    vig_path <- file.path(path, "vignettes")
    dir.create(vig_path, recursive = TRUE, showWarnings = FALSE)
  } else {
    vig_path <- path
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
        # to = paste0(path, vig_path, "/analysis.Rmd")
        to = file.path(vig_path, "analysis.Rmd")
      ))

      # Adding console feedback
      ui_done("analysis.Rmd has been created.")


    } else if (type == "Quarto (analysis.qmd)") {

      invisible(file.copy(
        from = system.file(
          "gists/analysis_qmd_wo_example.qmd", 
          package = "rUM"
        ),
        # to = paste0(path, vig_path, "/analysis.qmd")
        to = file.path(vig_path, "analysis.qmd")
      ))

      # Adding console feedback
      ui_done("analysis.qmd has been created.")

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
        # to = paste0(path, vig_path, "/analysis.Rmd")
        to = file.path(vig_path, "analysis.Rmd")
      ))

      # Adding console feedback
      ui_done("analysis.Rmd has been created.")


    } else if (type == "Quarto (analysis.qmd)") {
      
      invisible(file.copy(
        from = system.file(
          "gists/analysis_qmd_with_example.qmd", 
          package = "rUM"
        ),
        # to = paste0(path, vig_path, "/analysis.qmd")
        to = file.path(vig_path, "analysis.qmd")
      ))

      # Adding console feedback
      ui_done("analysis.qmd has been created.")

    # User did not provide correct argument type:  
    } else {
      abort(
        "The type must be 'R Markdown (analysis.Rmd)' or 'Quarto (analysis.qmd)'"
      )
    }
  }

  #############################################################################
  #            Add project directories and supplemental files                 #
  #############################################################################
  # Add custom.scss to Quarto non-package projects only. 
  #  "The minimal default format is a deliberate limitation of the current 
  #  implementaton of the vignette engine. It ensures that the HTML vignettes
  #  produced are reasonable size and can be published on CRAN without problems".
  # source: https://cran.r-project.org/web/packages/quarto/vignettes/hello.html
  if (vignette == FALSE & type == "Quarto (analysis.qmd)") {
    rUM::write_scss(name = "custom", path = path) 
  }
  
  dir.create(file.path(path, "data"), recursive = TRUE, showWarnings = FALSE)

  # Add enhanced .gitignore from inst/gists
  ign_path <- system.file("gists/aggressive_gitignore.md", package = "rUM")
  if (ign_path == "") {
    stop("Could not find .gitignore in package installation")
  }
  invisible({
    # Remove default .gitingore
    # file.remove(paste0(path, "/.gitignore"))
    if (file.exists(file.path(path, ".gitignore"))) {
      file.remove(file.path(path, ".gitignore"))
    }
    # Replace .gitignore
    file.copy(
      from = ign_path,
      # to = paste0(path, "/.gitignore")
      to = file.path(path, ".gitignore")
    )
  })
  # Adding console feedback
  ui_done("An enhanced .gitignore has been created.")

  # Add a README & progress notes templates from inst/gists
  rUM::write_readme(path = path)
  rUM::write_notes(path = path)

  # Add dated_progress_notes.md to .Rbuildignore for packages
  if (vignette == TRUE) {
    cat(
      "dated_progress_notes.md", 
      # file = file.path(paste0(path, "/.Rbuildignore")),
      file = file.path(path, ".Rbuildignore"),
      append = TRUE # add, don't overwrite current file
    )
    # Adding console feedback
    ui_done("dated_progress_notes.md has been added to the .Rbuildignore.")  
  }

  # write an empty packages bibliography file - needed to knit the first time
  # writeLines("", con = file.path(paste0(path, vig_path, "/packages.bib")))
  writeLines("", con = file.path(vig_path, "packages.bib"))
  
  # write an empty user bibliography file
  # writeLines("", con = file.path(paste0(path, vig_path, "/references.bib")))
  writeLines("", con = file.path(vig_path, "references.bib"))
  
  # Add .csl files
  download.file(
    "https://www.zotero.org/styles/the-new-england-journal-of-medicine",
    # paste0(path, vig_path, "/the-new-england-journal-of-medicine.csl"),
    file.path(vig_path, "the-new-england-journal-of-medicine.csl"),
    quiet = TRUE
  )
  download.file(
    "https://www.zotero.org/styles/apa",
    # paste0(path, vig_path, "/apa.csl"),
    file.path(vig_path, "apa.csl"),
    quiet = TRUE
  )
  
  if (vignette) .run_me_first(path, type)
}



#############################################################################
#                   make_project() helper functions                         #
#############################################################################
.valid_package_name <- function(x) {
  grepl("^[a-zA-Z][a-zA-Z0-9.]+$", x) && !grepl("\\.$", x)
}

#############################################################################
#               Automate package building functionality                     #
#############################################################################
# This function adds to DESCRIPTION file, .gitignore (package & vignettes),
#   adds the specific VignetteBuilder, and modifies the vignettes/analysis.*
#   YAML header with content appropriate to build the respective vignette
#   using the correct engine.
#############################################################################
.run_me_first <- function(path, type) {
  # browser()
  # Capture current directory and return to it at the end of this function
  current_wd <- getwd()
  # Move to new project location
  setwd(path)
  # Add quietly to DESCRIPTION:
  suppressMessages({
    usethis::use_package("here", type = "suggests")
    usethis::use_package("knitr", type = "suggests")
    usethis::use_package("rmarkdown", type = "suggests")
    usethis::use_package("roxygen2", type = "suggests")
    
    usethis::use_package("conflicted", type = "suggests")
    usethis::use_package("glue", type = "suggests")
    usethis::use_package("gtsummary", type = "suggests", min_version = "2.0.3")
    usethis::use_package("rUM", type = "suggests")
    usethis::use_package("rio", type = "suggests")
    usethis::use_package("table1", type = "suggests")
    usethis::use_package("tidymodels", type = "suggests")
    usethis::use_package("tidyverse", type = "suggests")
  })

  # browser()

  # Create vignettes/.gitignore & write "*.html" & "*.R"
  writeLines("*.html\n*.R", con = "vignettes/.gitignore")

  # Setup OS-specific string parsing--------------------------------------------------
  # Unix-based OS's use a end of liine return like "\n"
  # Windows uses a carriage return and line break like "\r\n"
  # This section will adjust the YAML pattern to be read and replaced by inserting
  # the appropriate line return items if on Windows:


  # Original YAML content to be replaced
  qmd_pattern <- "format:\n  html:\n    embed-resources: true\n    theme:\n      - default\n      - custom.scss"

  rmd_pattern <- "output:\n  bookdown::html_document2:\n    number_sections: false\n"

  # rUM will replace the pattern with this to be able write & create package vignettes
  qmd_replacement <- "output: rmarkdown::html_vignette\nvignette: >\n  %\\\\VignetteIndexEntry{your_title_goes_here}\n  %\\\\VignetteEngine{quarto::html}\n  %\\\\VignetteEncoding{UTF-8}"

  rmd_replacement <- "output: rmarkdown::html_vignette\nvignette: >\n  %\\\\VignetteIndexEntry{your_title_goes_here}\n  %\\\\VignetteEngine{knitr::rmarkdown}\n  %\\\\VignetteEncoding{UTF-8}\n"

  # Apply the OS-conditional change
  if (.Platform$OS.type == 'windows') {
    qmd_pattern <- stringr::str_replace_all(qmd_pattern, '\n', '\r\n')
    qmd_replacement <- stringr::str_replace_all(qmd_replacement, '\n', '\r\n')
    
    rmd_pattern <- stringr::str_replace_all(rmd_pattern, '\n', '\r\n')
    rmd_replacement <- stringr::str_replace_all(rmd_replacement, '\n', '\r\n')
  }
  #-----------------------------------------------------------------------------------

  # Append Vignette builder to DESCRIPTION file & modify YAML content
  if (type == "Quarto (analysis.qmd)") { # Quarto project
    # Add quietly to DESCRIPTION for Quarto:
    suppressMessages({
      usethis::use_package("quarto", type = "suggests", min_version = "1.3.12")
    })
    # Add Vignette builder to DESCRIPTION:
    cat(
      "VignetteBuilder: quarto\n", 
      file = file.path("DESCRIPTION"),
      append = TRUE # add, don't overwrite current file
    )
    # Replace the YAML pattern with the new structure for Quarto vignette:
    readr::write_file(
      x = stringr::str_replace(
        string = readr::read_file("vignettes/analysis.qmd"),
        pattern = qmd_pattern,
        replacement = qmd_replacement
      ), 
      file = "vignettes/analysis.qmd"
    )

  } else { # Rmd project
    # Add Vignette builder to DESCRIPTION:
    cat(
      "VignetteBuilder: knitr\n", 
      file = file.path("DESCRIPTION"),
      append = TRUE # add, don't overwrite current file
    )
    # Replace the YAML pattern with the new structure for Rmd vignette:
    readr::write_file(
      x = stringr::str_replace(
        string = readr::read_file("vignettes/analysis.Rmd"),
        pattern = rmd_pattern,
        replacement = rmd_replacement
        ), 
      file = "vignettes/analysis.Rmd"
    )
  }

  # Return to original location where rUM::make_project() was executed
  setwd(current_wd)
}
