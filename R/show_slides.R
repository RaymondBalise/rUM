#' Preview slide deck from a package
#' 
#' @description
#' Render a Quarto slide deck from the supplied package in the Viewer or browser. 
#' 
#' @param package Character. Provide the package containing one or more slide decks. 
#' @param deck Character. The name of the slide deck to render without ".qmd".
#' @param ... Optional arguments passed to \code{quarto::quarto_preview()}.
#' 
#' @return Uses \code{quarto::quarto_preview()} to display the slide deck in the 
#' Viewer pane or browser.
#' 
#' @examples
#' if (interactive()) {
#'   # Preview a known slide deck name from a package:
#'   show_slides(package = "rUM", deck = "hit_the_deck")
#' 
#'   # Use find_slides to pipe the output:
#'   find_slides(package = "rUM") |> show_slides(package = "rUM")
#' 
#'   # Show second slide deck when find_slides returns more than one file:
#'   find_slides(package = "rUM")[1] |> show_slides(package = "rUM")
#' }
#' 
#' @export
show_slides <- function(package = NULL, deck = NULL, ...) {

  # Ensure interactive environment
  if (!interactive()) stop("This must be used in an interactive session.")
  
  # Validate package argument
  if (!is.character(package)) stop("Argument 'package' must be a character.")
  if (is.null(package) | is.null(deck)) stop("Invalid NULL argument(s).")
  
  # Remove any file extension if added by user
  deck <- fs::path_ext_remove(deck)
  
  # Returns the path to the package slide deck & appends the deck argument with extension
  file_path <- glue::glue("{find.package(package)}/slides/{deck}.qmd")

  quarto::quarto_preview(file_path, ...)

}