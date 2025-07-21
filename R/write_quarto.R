#' Create a New Quarto Document
#'
#' This function creates a new Quarto document (.qmd file) complete with a useful header.
#'
#' @param filename Character string. The name of the file without the '.qmd' extension.
#'   Only letters, numbers, hyphens, and underscores are allowed.
#' @param path Character string. Directory where the file will be created. Defaults to
#'   the current project's base directory.
#' 
#' @importFrom glue glue
#' @importFrom here here
#' @importFrom stringr str_detect str_replace_all
#' @importFrom usethis edit_file
#'
#' @return Opens file after creating the Quarto document.
#'
#' @examples
#' \dontrun{
#' # Create a new Quarto document
#' write_quarto(filename = "data_cleaning", path = tempdir())
#' }
#' @export
#'
write_quarto <- function(filename = NULL, path = here()) {
  # Validate path
  if (!dir.exists(path)) dir.create(path)
  .validate_path(path)

  # Validate filename: part 1
  if (is.null(filename)) stop('Invalid filename. Please input a value.')

  # Remove .qmd if accidentally typed
  filename <- str_replace_all(filename, '.qmd$', '')

  # Validate filename: part 2
  .validate_filename(filename)

  # Normalize the path for consistency
  path <- normalizePath(path, mustWork = TRUE)

  if (file.access(path, mode = 2) != 0) {
    stop(glue(
      'You do not have permission to write to the path location: {path}\nTry `rUM::write_quarto(filename = "", path = "")`'
    ))
  }

  # Set up full file path
  the_quarto_file <- file.path(path, paste0(filename, '.qmd'))

  # Check for existing Quarto doc
  if (file.exists(the_quarto_file)) {
    stop(glue("{filename}.qmd already exists in the specified path."))
  }

  # Write the Quarto file based on template
  template_path <- system.file('gists/default_quarto.qmd', package = 'rUM')

  if (template_path == "") {
    stop("Could not find Quarto template in package installation")
  }

  file.copy(from = template_path, to = the_quarto_file, overwrite = FALSE)

  # Open the new template upon successful copy
  if (file.exists(the_quarto_file)) {
    edit_file(the_quarto_file)
  } else {
    stop("The file does not exist.")
  }

  invisible(NULL)
}
