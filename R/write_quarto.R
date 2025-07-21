#' Create a New Quarto Document
#'
#' This function creates a new Quarto document (.qmd file) complete with a useful header.
#'
#' @param filename Character string. The name of the file without the '.qmd' extension.
#'   Only letters, numbers, hyphens, and underscores are allowed.
#' @param path Character string. Directory where the file will be created. Defaults to
#'   the current project's base directory.
#' @param example Logical. Will the analysis file include a paper example with table/
#'   figure? Default is `NULL` and will use a default, non-paper template.
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
write_quarto <- function(filename = NULL, path = here(), example = NULL) {
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

  # Check if the user would like to use a paper or default template:
  if (is.null(example)) {
    # The user wants a generic non-paper template
    template_path <- system.file('gists/default_quarto.qmd', package = 'rUM')

    if (template_path == "") {
      stop("Could not find Quarto template in package installation")
    }
    
  } else if (!example) {
    # The user wants our non-example paper template
    template_path <- system.file('gists/analysis_qmd_wo_example.qmd', package = 'rUM')

  } else {
    # The user wants our example paper template
    template_path <- system.file('gists/analysis_qmd_with_example.qmd', package = 'rUM')
  }

  # Write the selected template to the specified path
  file.copy(from = template_path, to = the_quarto_file, overwrite = FALSE)

  # Add the citation and bibliography files to the project path if they do not exist.
  if (!is.null(example)) {
    .add_citation_files(path)
  }

  # Open the new template upon successful copy
  if (file.exists(the_quarto_file)) {
    edit_file(the_quarto_file)
  } else {
    stop("The file does not exist.")
  }

  invisible(NULL)
}
