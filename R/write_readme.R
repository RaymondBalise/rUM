#' Create a project README file
#'
#' This function streamlines project documentation by creating and managing a README.md
#' file. It provides interactive prompts for existing files and maintains consistent
#' project documentation structure.
#'
#' @param path The destination directory for the README file. Defaults to \code{
#' here::here()}.
#' 
#' @importFrom here here
#' @importFrom usethis ui_done ui_nope
#' 
#' @return Creates a comprehensive README template for project documentation.
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
#' If the README file already exists, the function will stop and warn the user. The
#' templates include example documentation that can be modified to suit project needs.
#'
#' @export
#' @examples
#' # Create new README in temporary directory
#' tmp <- tempdir()
#' write_readme(path = tmp)

write_readme <- function(path = here()) {
  # Validate path
  .validate_path(path)

  # Normalize the path for consistency
  path <- normalizePath(path, mustWork = TRUE)

  # Get README template path first (fail fast)
  readme_path <- system.file("gists/README.md", package = "rUM")
  if (readme_path == "") {
    stop("Could not find README template in package installation")
  }

  # Handle README creation/overwrite
  if (file.exists(file.path(path, 'README.md'))) {
    # stop(
    #   'A README.md has been found in the specified directory! If you would like to ',
    #   'proceed, remove the existing README and rerun this function.'
    # )
    if (
      ui_nope(
        "A README.md has been found in the specified directory! Would you like to overwite with rUM's README?"
      )
    ) {
      return(invisible(NULL))
    }
  }

  invisible(file.copy(
    from = readme_path,
    to = file.path(path, "README.md"),
    overwrite = TRUE
  ))
  ui_done("A README.md template has been created.")

  return(invisible(NULL))
}
