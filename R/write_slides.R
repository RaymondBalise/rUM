#' Create a Quarto slide deck template
#'
#' @description
#' Creates a pre-formatted .qmd file for presentation slides using reveal.js 
#' along with necessary supporting files (SCSS styling and RStudio theme). The 
#' generated template includes optimized YAML configuration and slide structure 
#' to quickly start building academic & professional presentations. For more 
#' information look in the 
#' [Creating Slides with write_slides()](../doc/ah_write_slides.html) vignette.
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
#' @param example Logical. Whether to include example slides with demonstrations of
#'   including content.
#'
#' @param template Character. Whether to include a slide template for common slide
#'   layouts and formatting (default: "none")
#'
#'   * optional: \code{"miami"} for a University of Miami theme.
#'   * optional: \code{"rmed2025"} for a R/Med 2025 theme.
#'
#' @param format Character string. Slide format to use. Currently supports 'reveal.js',
#'   with planned support for PowerPoint and Beamer in future releases.
#' 
#' @importFrom glue glue
#' @importFrom here here
#' @importFrom stringr str_detect str_replace_all
#' @importFrom usethis edit_file
#' 
#' @note Be sure to specify \code{path = "inst"} if you are adding slides to a package.
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
#' if (interactive()) {
#'   # Create basic slides template in current directory
#'   write_slides(filenames = "my_presentation")
#'
#'   # Create slides with example content in a specific directory
#'   tmp <- tempdir()
#'   write_slides(filenames = "tutorial_slides", path = tmp, example = TRUE)
#' 
#'   # Create a slidedeck for a package in the inst directory
#'   tmp <- tempdir()
#'   write_slides(filenames = "tutorial_slides", path = 'inst', example = TRUE)
#' }
#'
#' @export
write_slides <- function(
  filenames,
  path = here(),
  new_folder = "slides",
  example = FALSE,
  template = "none",
  format = "revealjs"
) {
  # Validate path
  if (is.null(path) || !dir.exists(path)) {
    stop("Invalid `path`. Please enter a valid project directory.")
  }

  # Validate filenames:
  for (i in filenames) {
    # Validate filenames: part 1
    if (is.null(i)) stop('Invalid filename. Please input a value.')
    if (str_detect(i, "/")) {
      stop(
        'Invalid filename. You included a forward slash in the file name. If you are trying to give a folder/directory location, use the `path =` argument.'
      )
    }
    if (str_detect(i, "\\\\")) {
      stop(
        'Invalid filename. You included a backslash in the file name. If you are trying to give a folder/directory location, use the `path =` argument.'
      )
    }
    if (!str_detect(i, '^[a-zA-Z0-9_-]+$')) {
      stop(
        'Invalid filename. Use only letters, numbers, hyphens, and underscores.'
      )
    }

    # Remove .qmd if accidentally typed
    i <- str_replace_all(i, '.qmd$', '')

    # Validate filename: part 2
    if (!is.character(i)) stop('Invalid filename: must be character.')
    if (!str_detect(i, '^[a-zA-Z0-9_-]+$')) {
      stop(
        'Invalid filename. Use only letters, numbers, hyphens, and underscores.'
      )
    }
  }

  # Validation check for new_folder:
  if (!is.character(new_folder)) stop('Invalid filename: must be character.')
  if (!str_detect(new_folder, '^[a-zA-Z0-9_-]+$')) {
    stop(
      'Invalid "new_folder" argument. Use only letters, numbers, hyphens, and underscores.'
    )
  }

  # Check the new_folder argument:
  if (new_folder == "none") {
    path <- path
  } else if (new_folder == "slides") {
    path <- glue("{path}/slides")
  } else if (new_folder %in% c("slides_", "slide_")) {
    path <- glue("{path}/slides_{filenames[1]}")
  } else {
    path <- glue("{path}/{new_folder}")
  }

  # Create the slide deck directory:
  if (!dir.exists(path)) dir.create(path)

  # Normalize the path for consistency
  path <- normalizePath(path, mustWork = TRUE)

  if (file.access(path, mode = 2) != 0) {
    stop(glue(
      'You do not have permission to write to the path location: {path}\nTry `rUM::write_quarto(filename = "", path = "")`'
    ))
  }

  # Part 1: Determine the slides template type:
  # list of valid slide templates
  valid_templates <- c("miami", "rmed2025")

  # 1a. Check if chosen template is an available template
  if (template %in% valid_templates) {
    # 1b. Check if using example themed template
    if (example) {
      slide_path <- glue("gists/quarto_slides_example_{template}.qmd")
    } else {
      slide_path <- glue("gists/quarto_slides_{template}.qmd")
    }

    # 2a. Check if template == "none"
  } else if (template == "none") {
    # 2b. Check if using example themed template
    if (example) {
      slide_path <- "slides/rUM_the_package.qmd"
    } else {
      slide_path <- "gists/quarto_slides.qmd"
    }
    
    # 3. Supplied named template is not valid, so use generic template
  } else {
    slide_path <- "slides/rUM_the_word.qmd"
  }

  # Complete template_path:
  template_path <- system.file(slide_path, package = "rUM")

  # Part 2: Create each slide .qmd file:

  for (slide_file in filenames) {
    # Set up full file path
    the_quarto_file <- file.path(path, paste0(slide_file, '.qmd'))

    # Check for existing Quarto doc
    if (file.exists(the_quarto_file)) {
      stop(glue(
        "{the_quarto_file} already exists in the specified path."        
      ))
    }

    file.copy(from = template_path, to = the_quarto_file, overwrite = FALSE)
  }

  # Part 3: Add SCSS file for slides:

  # Check for SCSS file in slide folder
  if (!file.exists(file.path(path, "slides.scss"))) {
    if (template == "rmed2025") {
      # For R/Med 2025:
      # SCSS file
      rum_scss_path <- system.file(
        "gists/slides_example_rmed.scss",
        package = "rUM"
      )
      # Create the img directory as referenced in the slide's YAML:
      img_path <- glue("{path}/img")
      if (!dir.exists(img_path)) dir.create(img_path)
      # Add background:
      file.copy(
        from = system.file("img/rmed_background.png", package = "rUM"),
        to = file.path(img_path, "rmed_background.png")
      )
      # Add header strip
      file.copy(
        from = system.file("img/rmed_narrow.png", package = "rUM"),
        to = file.path(img_path, "rmed_narrow.png")
      )
      # Add logos:
      file.copy(
        from = system.file("img/rmed.png", package = "rUM"),
        to = file.path(img_path, "rmed.png")
      )
      file.copy(
        from = system.file("img/R-Med-25-Hex-Logo.png", package = "rUM"),
        to = file.path(img_path, "R-Med-25-Hex-Logo.png")
      )
      # Add favicon:
      file.copy(
        from = system.file("img/rmed.ico", package = "rUM"),
        to = file.path(img_path, "rmed.ico")
      )
      # Add JavaScript HTML file that cleans up title page and adds slide content:
      file.copy(
        from = system.file("gists/clean_title_page.html", package = "rUM"),
        to = file.path(img_path, "clean_title_page.html")
      )

    } else if (template == 'miami') {
      # SCSS file
      rum_scss_path <- system.file(
        "gists/custom_miami.scss",
        package = "rUM"
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
    file.copy(
      from = rum_scss_path,
      to = the_project_scss_file,
      overwrite = FALSE
    )
  } else {
    # Output when a SCSS file exists, but won't be overwritten
    message(glue(
      "A SCSS file has been found in the slides project location & will not be modified. Please create the slides in a different folder if you'd like to use the SCSS file from the {template} template."
    ))
  }

  # Part 4: Check for RStudio light theme file:
  if (!file.exists(file.path(path, "rstudio_default-light.theme"))) {
    # Add RStudio theme file
    rs_theme_path <- system.file(
      'gists/rstudio_default-light.theme',
      package = 'rUM'
    )
    the_rstheme_file <- file.path(path, 'rstudio_default-light.theme')

    if (rs_theme_path == "") {
      stop(
        "Could not find rstudio_default-light.theme file in package installation"
      )
    }

    file.copy(from = rs_theme_path, to = the_rstheme_file, overwrite = FALSE)
  }

  has_description_file <- list.files(
    path = dirname(path),  # search one level up, essentially the package/project root 
    pattern = "DESCRIPTION", 
    full.names = TRUE
  )
  going_to_inst <- str_detect(new_folder, 'inst')

  if (length(has_description_file) > 0 && !going_to_inst) {
    warning('rUM has created your slide deck.\nHowever, we have detected that you are currently in a package environment. If your project does not have an "inst" directory/folder, make one. Move your slide\'s directory to the "inst" directory for find_slides() and show_slides() to work and to pass CRAN checks.')
  }

  # Open the first slide file template upon successful copy
  the_first_slide_file <- file.path(path, paste0(filenames[1], ".qmd"))
  if (file.exists(the_first_slide_file)) {
    edit_file(the_first_slide_file)
  } else {
    stop("The file does not exist.")
  }

  invisible(NULL)
}
