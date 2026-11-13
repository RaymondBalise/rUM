#' Create a project README file
#'
#' Creates a structured `README.md` template for project documentation. If
#' `README.md` already exists, interactive sessions prompt for confirmation
#' before overwriting.
#'
#' @param path Destination directory for `README.md`. Defaults to
#'   `here::here()`.
#'
#' @importFrom here here
#' @importFrom rlang is_interactive
#' @importFrom usethis ui_done ui_info
#' @importFrom utils menu
#'
#' @return Invisibly returns `NULL`.
#'
#' @details
#' The `README.md` template includes structured sections for:
#' \itemize{
#'   \item Project description (study name, principal investigator, author)
#'   \item Project setup steps for reproducibility
#'   \item File and directory descriptions
#'   \item Ordered analysis pipeline and data provenance notes
#' }
#'
#' If `README.md` already exists, interactive sessions ask whether to overwrite
#' it; non-interactive sessions stop with an error to avoid clobbering existing
#' documentation.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   tmp <- tempdir()
#'   write_readme(path = tmp)
#' }
write_readme <- function(path = here()) {
  .validate_path(path)
  path <- normalizePath(path, mustWork = TRUE)

  scaffold_path <- .get_rum_scaffold()
  readme_path <- file.path(scaffold_path, "supporting/README.md")
  if (!file.exists(readme_path)) {
    stop("Could not find README template in downloaded templates")
  }

  readme_dst <- file.path(path, "README.md")
  if (file.exists(readme_dst)) {
    if (!is_interactive()) {
      stop(paste(
        "A README.md already exists in the specified directory and session is",
        "not interactive. Remove it first or run interactively."
      ))
    }
    choice <- menu(
      choices = c("No", "Yes"),
      title = paste(
        "A README.md has been found in the specified directory!",
        "Would you like to overwrite it with rUM's README?"
      )
    )
    if (choice != 2L) {
      ui_info("Operation cancelled; existing README.md was kept.")
      return(invisible(NULL))
    }
  }

  invisible(file.copy(
    from = readme_path,
    to = readme_dst,
    overwrite = TRUE
  ))
  ui_done("A README.md template has been created.")

  invisible(NULL)
}
