#' Preview slide deck from a package
#' 
#' @description
#' Render a Quarto slide deck from the supplied package in the Viewer or browser. 
#' For more information look in the 
#' [Creating Slides with write_slides()](../doc/ah_write_slides.html) vignette.
#' 
#' @param package Character. Provide the package containing one or more slide decks. 
#' @param deck Character. The name of the slide deck to render without ".qmd".
#' @param ... Optional arguments passed to \code{quarto::quarto_preview()}.
#' 
#' @importFrom fs path_ext_remove
#' @importFrom glue glue
#' @importFrom quarto quarto_preview
#' @importFrom utils menu
#' 
#' @note Uses \code{quarto::quarto_preview()} to display the slide deck in the 
#' Viewer pane or browser.
#' 
#' @examples
#' if (interactive()) {
#'   # Preview a known slide deck name from a package:
#'   show_slides(package = "rUM", deck = "rUM_the_package")
#' 
#'   # Use find_slides to pipe the output:
#'   find_slides(package = "rUM") |> show_slides()
#' }
#' 
#' @export
show_slides <- function(package = NULL, deck = NULL, ...) {

  # Ensure interactive environment
  if (!interactive()) stop("This must be used in an interactive session.")
  
  # Handle piped input from find_slides()
  if (inherits(package, 'slide_finder')) {
    slide_obj <- package
    package <- slide_obj$package
    available_slides <- slide_obj$slides
  } else {
    # User called show_slides directly. Use find_slides functionality under the hood
    # Validate package argument
    if (is.null(package)) stop("Invalid NULL package argument.")
    if (!is.character(package)) stop("Argument 'package' must be a character.")
    
    # Get the available slides using find_slides logic
    # suppressMessages for cleaner output
    available_slides <- suppressMessages(find_slides(package))$slides
  }
  
  # If the deck is NULL and multiple slides are available, prompt for selection
  if (is.null(deck) && length(available_slides) > 1) {
    choice <- menu(
      choices = available_slides,
      title = paste0('Select a slide deck from "', package, '" package:')
    )

    if (choice == 0) {
      message('No slide selected.')
      return(invisible(NULL))
    }

    deck <- available_slides[choice]
  }

  # If deck is NULL but only one slide, use it
  if (is.null(deck) && length(available_slides) == 1) {
    deck <- available_slides[1]
  }

  # Validate we have a deck at this point
  if (is.null(deck)) stop('No reveal.js Quarto slide deck specified or found.')
    
  # Remove any file extension if added by user
  deck <- path_ext_remove(deck)
  
  # Returns the path to the package slide deck & appends the deck argument with extension
  file_path <- glue("{find.package(package)}/slides/{deck}.qmd")

  message('Rendering your reveal.js Quarto slideshow...')
  quarto_preview(file_path, ...)

  }
