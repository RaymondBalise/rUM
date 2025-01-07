#' Create a New Quarto Document
#'
#' This function creates a new Quarto document (.qmd file) complete with a useful header.
#'
#' @param filename Character string. The name of the file without the '.qmd' extension.
#'   Only letters, numbers, hyphens, and underscores are allowed.
#' @param path Character string. Directory where the file will be created. Defaults to
#'   the current project's base directory.
#'
#' @return Invisibly returns NULL after creating the Quarto document.
#'
#' @examples
#' \donttest{
#' # Create a new Quarto document with custom YAML
#' write_quarto(filename = "frog_analysis", path = tempdir())
#'
#' # Create a basic Quarto document with standard YAML
#' write_quarto(filename = "frog_analysis_basic", path = tempdir(), custom_yaml = FALSE)
#' }
#' @export
write_quarto <- function(
  filename = NULL, 
  path = NULL
) {
  # Validate path
  if (is.null(path) || !dir.exists(path)) {
    stop("Invalid `path`. Please enter a valid project directory.")
  }

  # Validate filename
  if (!is.character(filename)) stop('Invalid filename: must be character.')
  if (!grepl('^[a-zA-Z0-9_-]+$', filename)) {
    stop('Invalid filename. Use only letters, numbers, hyphens, and underscores.')
  }

  # Normalize the path for consistency
  path <- normalizePath(path, mustWork = TRUE)

  # Set up full file path
  the_quarto_file <- file.path(path, paste0(filename, '.qmd'))

  # Check for existing Quarto doc
  if (file.exists(the_quarto_file)) {
    stop(sprintf("%s.qmd already exists in the specified path.", filename))
  }

  # Write the Quarto file based on template
  template_path <- system.file('gists/default_quarto.qmd', package = 'rUM')

  if (template_path == "") {
    stop("Could not find Quarto template in package installation")
  }

  file.copy(from = template_path, to = the_quarto_file, overwrite = FALSE)
  ui_done(sprintf("Created %s.qmd %s", filename))

  invisible(NULL)
}
