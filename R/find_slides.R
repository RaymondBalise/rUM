#' List all slide decks in a package
#' 
#' @description
#' Returns the names of all Quarto slide decks in a package. This is designed to work
#' with \code{rUM::show_slides()} to preview the slide deck. For more 
#' information look in the 
#' [Creating Slides with write_slides()](../doc/ah_write_slides.html) vignette.
#' 
#' @param package Character. Provide the package containing one or more slide decks.
#' 
#' @importFrom dplyr filter mutate pull rowwise tibble
#' @importFrom fs path_ext_remove
#' @importFrom glue glue
#' @importFrom stringr str_detect
#' 
#' @return A list of class "slide_finder" containing the name of the package and the
#' name of the slides.
#' 
#' @examples
#' if (interactive()) {
#'   find_slides("rUM")
#' }
#' 
#' @export
find_slides <- function(package = NULL) {

  # Validate package argument
  if (is.null(package)) stop("Invalid NULL package argument.")
  if (!is.character(package)) stop("Argument 'package' must be a character.")
  
  # Returns the path to the package slide deck
  folder_path <- glue("{find.package(package)}/slides")

  # Keep this for testing within this package only
  # folder_path <- glue("{find.package(package)}/inst/slides")

  # Get a list of all file paths
  file_paths <- list.files(
    path = folder_path, 
    pattern = "\\.qmd$", 
    full.names = TRUE, 
    recursive = TRUE
  )
  
  # Create a tibble of the filepaths
  the_slides <- tibble(filepath = file_paths)
  
  # Only proceed if files are found, but must be same structure as if having an
  # available slide
  if (nrow(the_slides) == 0) {
    # return(structure(
    #   list(package = package, slides = character(0)),
    #   class = 'slide_finder'
    # ))
    stop(paste('No slides found for package:', package))
  }
  
  # Process each file
  result <- the_slides |> 
    rowwise() |> 
    mutate(
      content = readLines(.data$filepath, n = 30) |> paste(collapse = "\n"), 
      is_slide = str_detect(.data$content, "revealjs")
    ) |> 
    filter(.data$is_slide) |> 
    pull(.data$filepath) |> 
    basename() |> 
    path_ext_remove()

  # Only show message if called interactively at top level (not in a pipe/assignment)
  show_message <- interactive() && sys.nframe() == 1
  if (show_message) {
    message(
      'Available reveal.js slides in package "', package, '":\n', 
      paste('  -', result, collapse = '\n')
    )
  }

  # Return an invisible structured object to be piped into show_slides()
  invisible(
    structure(
      list(
        package = package,
        slides = result
      ),
      class = 'slide_finder'
    )
  )

}
