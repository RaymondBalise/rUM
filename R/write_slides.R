#' Create a Quarto slide deck template
#' 
#' @description
#' Creates a pre-formatted .qmd file for presentation slides along with necessary 
#' supporting files (SCSS styling and RStudio theme). The generated template 
#' includes optimized YAML configuration and slide structure to quickly start 
#' building academic & professional presentations.
#' 
#' @param filenames Character vector with minimal length of 1. This allows for the ability
#'   to batch create multiple slide decks in one function call.
#' @param path Character string. Directory where the file will be created. Defaults to
#'   the current project's base directory.
#' @param new_folder Character. Default folder is \code{"slides"}. Options are:  
#' 
#'   * \code{"none"}: No folder is created. All files (.qmd & other) are created 
#'   in the working directory (path of \code{here::here()}).
#' 
#'   * \code{"all"}: One folder is created for each value in the length of 
#'   \code{filenames}.  
#' 
#'   * \code{"slide_"}: One folder is created by appending "slides_" to the first file
#'   in \code{filenames} argument. If \code{filenames = "day_1"}, then the folder will
#'   be named \code{"slides_day_1"}.  
#' 
#'   * A character value.  
#' 
#' @param example Whether to include example slides with demonstrations
#'   of common slide layouts and formatting (default: FALSE)
#' 
#'   * optional: \code{"rmed2025"} for a R/Med 2025 theme.
#' 
#' @param format Character string. Slide format to use. Currently supports 'revealjs',
#'   with planned support for PowerPoint and Beamer in future releases.
#' 
#' @return Invisibly returns NULL. The created .qmd file is automatically opened
#'   in the RStudio editor upon successful creation.
#' 
#' @details
#' The function creates three files:
#'   * A .qmd file with the specified filename containing the slide template
#'   * A slides.scss file for custom styling 
#'   * An RStudio theme file for consistent code highlighting
#'
#' All filenames must contain only letters, numbers, hyphens, and underscores.
#' 
#' @examples
#' # Create basic slides template in current directory
#' write_slides(filenames = "my_presentation")
#' 
#' # Create slides with example content in a specific directory
#' tmp <- tempdir()
#' write_slides(filenames = "tutorial_slides", path = tmp, example = TRUE)
#' 
#' @export
write_slides <- function(
  filenames, 
  path = here::here(), 
  new_folder = "slides",
  example = "FALSE", 
  format = 'revealjs'
) {
  # Validate path
  if (is.null(path) || !dir.exists(path)) {
    stop("Invalid `path`. Please enter a valid project directory.")
  }

  # Validate filenames:
  for (i in filenames) {
    # Validate filenames: part 1
    if (is.null(i)) stop('Invalid filename. Please input a value.')
      if (!grepl('^[a-zA-Z0-9_-]+$', i)) {
        stop('Invalid filename. Use only letters, numbers, hyphens, and underscores.')
      }

    # Remove .qmd if accidentally typed
    i <- str_replace_all(i, '.qmd$', '')

    # Validate filename: part 2
    if (!is.character(i)) stop('Invalid filename: must be character.')
    if (!grepl('^[a-zA-Z0-9_-]+$', i)) {
      stop('Invalid filename. Use only letters, numbers, hyphens, and underscores.')
    }
  }

  # Validation check for new_folder:
  if (!is.character(new_folder)) stop('Invalid filename: must be character.')
  if (!grepl('^[a-zA-Z0-9_-]+$', new_folder)) {
    stop('Invalid "new_folder" argument. Use only letters, numbers, hyphens, and underscores.')
  }
  
  # Check the new_folder argument:
  if (new_folder == "none") {
    path <- path
  } else if (new_folder == "slides") {
    path <- glue::glue("{path}/slides")
  } else if (new_folder %in% c("slides_", "slide_")) {
    path <- glue::glue("{path}/slides_{filenames[1]}")
  } else {
    path <- glue::glue("{path}/{new_folder}")
  }

  # Create the slide deck directory:
  if (!dir.exists(path)) dir.create(path)

  # Normalize the path for consistency
  path <- normalizePath(path, mustWork = TRUE)

  if (file.access(path, mode = 2) != 0) {
    stop(sprintf(
      'You do not have permission to write to the path location: %s\nTry `rUM::write_quarto(filename = "", path = "")`', 
      path
    ))
  }

  # Create each slide .qmd file
  for (slide_file in filenames) {
    # Set up full file path
    the_quarto_file <- file.path(path, paste0(slide_file, '.qmd'))

    # Check for existing Quarto doc
    if (file.exists(the_quarto_file)) {
      stop(sprintf("%s.qmd already exists in the specified path.", the_quarto_file))
    }

    # Write the Quarto file based on template
    if (example != "rmed2025") {
      template_path <- system.file('gists/quarto_slides.qmd', package = 'rUM')
    } else {
      template_path <- system.file('gists/quarto_slides_rmed2025.qmd', package = 'rUM')
    }
    
    if (template_path == "") {
      stop("Could not find Quarto template in package installation")
    }

    file.copy(from = template_path, to = the_quarto_file, overwrite = FALSE)
  }

  # Check for SCSS file in slide folder:
  if (!file.exists(file.path(path, "slides.scss"))) {
    
    # Add SCSS file for slides
    if (example == 'rmed2025') {
      # For R/Med 2025:
      # SCSS file
      rum_scss_path <- system.file('gists/slides_example_rmed.scss', package = 'rUM')
      # Add background:
      file.copy(
        from = system.file("img/rmed_background.png", package = "rUM"),
        to = file.path(path, "rmed_background.png")
      )
      # Add header strip
      file.copy(
        from = system.file("img/rmed_narrow.png", package = "rUM"),
        to = file.path(path, "rmed_narrow.png")
      )
      # Add logo:
      file.copy(
        from = system.file("img/rmed.png", package = "rUM"),
        to = file.path(path, "rmed.png")
      )
      # Add favicon:
      file.copy(
        from = system.file("img/rmed.ico", package = "rUM"),
        to = file.path(path, "rmed.ico")
      )
      # Add JavaScript HTML file that cleans up title page and adds slide content:
      file.copy(
        from = system.file("gists/clean_title_page.html", package = "rUM"),
        to = file.path(path, "clean_title_page.html")
      )
    } else {
      # Normal styling
      rum_scss_path <- system.file('gists/slides.scss', package = 'rUM')
    }
    
    if (rum_scss_path == "") {
      stop("Could not find slides.scss file in package installation")
    }
    
    # Where to write the scss file:
    the_project_scss_file <- file.path(path, 'slides.scss')
    file.copy(from = rum_scss_path, to = the_project_scss_file, overwrite = FALSE)
  }

  # Check for RStudio light theme file:
  if (!file.exists(file.path(path, "rstudio_default-light.theme"))) {
    # Add RStudio theme file
    rs_theme_path <- system.file('gists/rstudio_default-light.theme', package = 'rUM')
    the_rstheme_file <- file.path(path, 'rstudio_default-light.theme')

    if (rs_theme_path == "") {
      stop("Could not find rstudio_default-light.theme file in package installation")
    }

    file.copy(from = rs_theme_path, to = the_rstheme_file, overwrite = FALSE)
  }

  # Open the first slide file template upon successful copy
  the_first_slide_file <- file.path(path, paste0(filenames[1], ".qmd"))
  if (file.exists(the_first_slide_file)) {
    usethis::edit_file(the_first_slide_file)
  } else {
    stop("The file does not exist.")
  }

  invisible(NULL)
}