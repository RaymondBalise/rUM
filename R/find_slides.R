#' List all slide decks in a package
#' 
#' @description
#' Returns the names of all Quarto slide decks in a package. This is designed to work
#' with \code{rUM::show_slides()} to preview the slide deck.
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

  the_slides <- list.files(
    path = folder_path, 
    pattern = "\\.qmd$", 
    full.names = TRUE, 
    recursive = TRUE
  ) |> 
    tibble(filepath = _) |> 
    rowwise() |> 
    mutate(
      content = readLines(.data$filepath, n = 30) |> paste(collapse = "\n"), 
      is_slide = grepl("(revealjs|beamer|pptx)", .data$content)
    ) |> 
    dplyr::filter(.data$is_slide) |> 
    dplyr::pull(.data$filepath) |> 
    basename() |> 
    fs::path_ext_remove()

  return(the_slides)

}
