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
#' @return A character vector of slide deck names within the provided package.
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
  folder_path <- glue::glue("{find.package(package)}/slides")

  # Get a list of all file paths
  file_paths <- list.files(
    path = folder_path, 
    pattern = "\\.qmd$", 
    full.names = TRUE, 
    recursive = TRUE
  )
  
  # Create a tibble of the filepaths
  the_slides <- tibble(filepath = file_paths)
  
  # Only proceed if files are found
  if (nrow(the_slides) == 0) return(character(0))
  
  # Process each file
  result <- the_slides |> 
    rowwise() |> 
    mutate(
      content = readLines(.data$filepath, n = 30) |> paste(collapse = "\n"), 
      is_slide = grepl("(revealjs|beamer|pptx)", .data$content)
    ) |> 
    dplyr::filter(.data$is_slide) |> 
    dplyr::pull(.data$filepath) |> 
    basename() |> 
    fs::path_ext_remove()

  return(result)

}
