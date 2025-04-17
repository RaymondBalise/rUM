# Collection of rUM helper functions: ---------------------------------------------------

#' Check for valid project name
#' @description
#' This helper function parses the provided rUM::make_project or rUM::make_package path
#' argument.
#' 
#' @param path Character. Path supplied by user for location of new rUM project.
#' 
#' @noRd
.valid_package_name <- function(path) {
  grepl("^[a-zA-Z][a-zA-Z0-9.]+$", path) && !grepl("\\.$", path)
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
#' Add dated_progress_notes file
#' @description
#' This helper functon will add the dated progress notes template
#' 
#' @param path Character. Path to project root directory.
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
#' Add citation and referencess file
#' @description
#' This helper functon will add .bib and .csl files to the supplied path. It is added to 
#' the project root directory for non-package projects, or is created in the vignettes 
#' directory.
#' 
#' @param path Character. Path where to store the document template, either project root
#' or vignettes folder for package projects.
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
#' @noRd
.rUM_build_description <- function(is_quarto_project) {
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

  # Add Vignette builder to DESCRIPTION:
  if (is_quarto_project) {
    cat(
      "VignetteBuilder: quarto\n", 
      file = file.path("DESCRIPTION"),
      append = TRUE # add, don't overwrite current file
    )
    # Add minimal Quarto version:
    suppressMessages({
      usethis::use_package("quarto", type = "suggests", min_version = "1.3.12")
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
#' @noRd
.rUM_modify_for_vignette <- function(is_quarto_project) {
  
  # Setup OS-specific string parsing--------------------------------------------------
  # Unix-based OS's use a end of line return like "\n"
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
  if (is_quarto_project) { # Quarto project
    
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

  # Return to original location where rUM::make_project() was executed
  setwd(current_wd)

}


#----------------------------------------------------------------------------------------
#' Helper for write_man (1)
#' @description Function needed to clean labels for manual variable descriptions. Remove
#' {text} typically used for {other}
#' 
#' @param text Character. The text to parse.
#' @noRd
.remove_braces <- function(text) {
  gsub("\\{[^\\}]*\\}", "", text)
}

#' Helper for write_man (2)
#' @description Function needed to clean labels for manual variable descriptions. Used
#' to replace square brackets (\code{[]}) with backticks \code{``} for the info piped
#' into variable labels.
#' 
#' @param text Character. The text to parse.
#' @noRd
.replace_brackets_with_backticks <- function(text) {
  gsub("\\[(.*?)\\]", "`\\1`", text)
}