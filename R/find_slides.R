#' List all slide decks in a package
#'
#' @description
#' Returns the names of all Quarto reveal.js slide decks in a package. Designed
#' to work with `rUM::show_slides()` to preview a slide deck. For more
#' information, see `vignette("ah_write_slides", package = "rUM")`.
#'
#' @param package Character. Package name containing one or more slide decks.
#'
#' @importFrom dplyr filter mutate pull rowwise tibble
#' @importFrom fs path_ext_remove path_rel
#' @importFrom rlang is_interactive
#' @importFrom stringr str_detect
#'
#' @return Invisibly returns a list of class `"slide_finder"` containing
#'   `package` and `slides`.
#'
#' @examples
#' if (interactive()) {
#'   find_slides("rUM")
#' }
#'
#' @export
find_slides <- function(package = NULL) {
  if (is.null(package)) stop("Invalid NULL package argument.")
  if (
    !is.character(package) ||
      length(package) != 1L ||
      is.na(package) ||
      !nzchar(trimws(package))
  ) {
    stop("Argument 'package' must be a character string (non-empty).")
  }

  pkg_root <- find.package(package)
  folder_path <- system.file("slides", package = package)
  if (
    !nzchar(folder_path) &&
      dir.exists(file.path(pkg_root, "inst", "slides"))
  ) {
    folder_path <- file.path(pkg_root, "inst", "slides")
  }

  if (!nzchar(folder_path) || !dir.exists(folder_path)) {
    stop(paste("No slides found for package:", package))
  }

  file_paths <- list.files(
    path = folder_path,
    pattern = "\\.qmd$",
    full.names = TRUE,
    recursive = TRUE
  )

  if (length(file_paths) == 0L) {
    stop(paste("No slides found for package:", package))
  }

  the_slides <- tibble(filepath = file_paths)

  matched_paths <- the_slides |>
    rowwise() |>
    mutate(
      content = paste(
        readLines(.data$filepath, n = 100, warn = FALSE),
        collapse = "\n"
      ),
      is_slide = str_detect(.data$content, "revealjs")
    ) |>
    filter(.data$is_slide) |>
    pull(.data$filepath)

  if (length(matched_paths) == 0L) {
    stop(paste("No slides found for package:", package))
  }

  result <- matched_paths |>
    path_rel(start = folder_path) |>
    path_ext_remove() |>
    as.character()

  show_message <- is_interactive() && sys.nframe() == 1
  if (show_message) {
    message(
      'Available reveal.js slides in package "', package, '":\n',
      paste("  -", result, collapse = "\n")
    )
  }

  invisible(
    structure(
      list(
        package = package,
        slides = result
      ),
      class = "slide_finder"
    )
  )
}
