#' Add 'Quarto' slides template
#' 
#' @description
#' Add more details later...
#' 
#' @inheritParams write_quarto
#' @param example Will the slide deck include an example slides?
#' @param format What Quarto slide format to use
#' 
#' @return Opened Quarto template for reveal.js slides
#' 
#' @examples
#' tmp <- tempdir()
#' write_slides(filename = "reveal_project", path = tmp, format = 'revealjs')
#' 
#' @export
write_slides <- function(
  filename, 
  path = here::here(), 
  example = FALSE, 
  format = 'revealjs'
){
  # Validate path
  if (is.null(path) || !dir.exists(path)) {
    stop("Invalid `path`. Please enter a valid project directory.")
  }

  # Validate filename: part 1
  if (is.null(filename)) stop('Invalid filename. Please input a value.')

  # Remove .qmd if accidentally typed
  filename <- str_replace_all(filename, '.qmd$', '')

  # Validate filename: part 2
  if (!is.character(filename)) stop('Invalid filename: must be character.')
  if (!grepl('^[a-zA-Z0-9_-]+$', filename)) {
    stop('Invalid filename. Use only letters, numbers, hyphens, and underscores.')
  }

  # Normalize the path for consistency
  path <- normalizePath(path, mustWork = TRUE)

  if (file.access(path, mode = 2) != 0) {
    stop(sprintf(
      'You do not have permission to write to the path location: %s\nTry `rUM::write_quarto(filename = "", path = "")`', 
      path
    ))
  }

  # Set up full file path
  the_quarto_file <- file.path(path, paste0(filename, '.qmd'))

  # Check for existing Quarto doc
  if (file.exists(the_quarto_file)) {
    stop(sprintf("%s.qmd already exists in the specified path.", filename))
  }

  # Write the Quarto file based on template
  if (example) {
    template_path <- system.file('gists/quarto_slides.qmd', package = 'rUM')
  } else {
    template_path <- system.file('gists/quarto_slides_example.qmd', package = 'rUM')
  }
  
  if (template_path == "") {
    stop("Could not find Quarto template in package installation")
  }

  file.copy(from = template_path, to = the_quarto_file, overwrite = FALSE)


  # Add SCSS file for slides
  scss_path <- system.file('gists/slides.scss', package = 'rUM')
  the_scss_file <- file.path(path, 'slides.scss')

  if (scss_path == "") {
    stop("Could not find slides.scss file in package installation")
  }

  file.copy(from = scss_path, to = the_scss_file, overwrite = FALSE)

  # Add RStudio theme file
  rs_theme_path <- system.file('gists/rstudio_default-light.theme', package = 'rUM')
  the_rstheme_file <- file.path(path, 'rstudio_default-light.theme')

  if (rs_theme_path == "") {
    stop("Could not find rstudio_default-light.theme file in package installation")
  }

  file.copy(from = rs_theme_path, to = the_rstheme_file, overwrite = FALSE)

  # Open the new template upon successful copy
  if (file.exists(the_quarto_file)) {
    usethis::edit_file(the_quarto_file)
  } else {
    stop("The file does not exist.")
  }

  invisible(NULL)
}