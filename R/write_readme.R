#' Create a project README file
#'
#' This function streamlines project documentation by creating and managing both README.md
#' and dated_progress_notes.md files. It provides interactive prompts for existing files
#' and maintains consistent project documentation structure.
#'
#' @param path The path to the main project level. Defaults to the current
#' working directory.
#' @return Creates or updates two files:
#' \itemize{
#'   \item README.md: A comprehensive template for project documentation
#'   \item dated_progress_notes.md: A chronological project progress tracker
#' }
#'
#' @details
#' The README.md template includes structured sections for:
#' \itemize{
#'   \item Project description (study name, principal investigator, author)
#'   \item Project setup steps for reproducibility
#'   \item File and directory descriptions
#'   \item Miscellaneous project notes
#' }
#'
#' The dated_progress_notes.md file is initialized with the current date and is designed
#' to help track project milestones chronologically.
#'
#' For both files, if they already exist, the user will be prompted before any
#' overwriting occurs. The templates include example documentation that can be
#' modified to suit your specific project needs.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create new documentation files
#' write_readme(path = "path/to/project")
#'
#' # Update existing documentation (will prompt for confirmation)
#' write_readme()  # uses current working directory
#' }
 
write_readme <- function(path = getwd()) {
  # Check if directory exists
  if (!dir.exists(path)) {
    stop("Directory does not exist")
  }
  
  # Normalize the path for consistency
  path <- normalizePath(path, mustWork = TRUE)
  
  # Get README template path first (fail fast)
  readme_path <- system.file("gists/README.md", package = "rUM")
  if (readme_path == "") {
    stop("Could not find README template in package installation")
  }

  # Handle dated_progress_notes.md creation/overwrite
  progress_notes_content <- paste0(
    "# Add project updates here\n",
    format(Sys.Date(), "%b %d, %Y"),
    ": project started"
  )
  
  # Handle README creation/overwrite
  if (file.exists(file.path(path, 'README.md'))) {
    ui_info('**CAUTION!!**')
    if (ui_yeah('README.md found in project level directory! Overwrite?')) {

      # Yes, overwrite existing README
      invisible(file.copy(
        from = readme_path,
        to = file.path(path, "README.md"),
        overwrite = TRUE
      ))
      ui_done("README.md has been overwritten with the template.")

    } else {
      ui_info("Keeping existing README.md")
    }
  } else {
    invisible(file.copy(
      from = readme_path,
      to = file.path(path, "README.md")
    ))
    ui_done("A README.md template has been created.")

    # Now check if the progress notes exists & ask to overwrite that too
    if (!file.exists(file.path(path, 'dated_progress_notes.md'))) {
      writeLines(
        progress_notes_content,
        con = file.path(path, "dated_progress_notes.md")
      )
      ui_done("A dated_progress_notes.md template has been created.")
    }
  }
  
  return(invisible(NULL))
}
