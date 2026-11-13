#' Preview slide deck from a package
#'
#' @description
#' Render and preview a Quarto reveal.js slide deck from an installed or
#' development package in the Viewer pane or browser. Slide assets are staged in
#' a temporary directory so read-only system libraries and package source trees
#' are never modified. For more information, see
#' `vignette("ah_write_slides", package = "rUM")`.
#'
#' @param package Character (or `"slide_finder"` object piped from
#'   `find_slides()`). Package containing one or more slide decks.
#' @param deck Character. Name of the slide deck to render (without `".qmd"`).
#' @param ... Optional arguments passed to `quarto::quarto_preview()`.
#'
#' @importFrom quarto quarto_preview
#' @importFrom rlang is_interactive
#' @importFrom utils menu
#'
#' @note Uses `quarto::quarto_preview()` to display the slide deck in the
#'   Viewer pane or browser.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' if (interactive()) {
#'   show_slides(package = "rUM", deck = "rUM_the_package")
#'   find_slides(package = "rUM") |> show_slides()
#' }
#'
#' @export
show_slides <- function(package = NULL, deck = NULL, ...) {
  if (!is_interactive()) stop("This must be used in an interactive session.")

  if (inherits(package, "slide_finder")) {
    slide_obj <- package
    package <- slide_obj$package
    available_slides <- slide_obj$slides
  } else {
    if (is.null(package)) stop("Invalid NULL package argument.")
    if (
      !is.character(package) ||
        length(package) != 1L ||
        is.na(package) ||
        !nzchar(trimws(package))
    ) {
      stop("Argument 'package' must be a character string (non-empty).")
    }
    available_slides <- suppressMessages(find_slides(package))$slides
  }

  if (is.null(deck) && length(available_slides) > 1L) {
    choice <- menu(
      choices = available_slides,
      title = paste0('Select a slide deck from "', package, '" package:')
    )

    if (choice == 0) {
      message("No slide selected.")
      return(invisible(NULL))
    }

    deck <- available_slides[choice]
  }

  if (is.null(deck) && length(available_slides) == 1L) {
    deck <- available_slides[1L]
  }

  if (is.null(deck)) stop("No reveal.js Quarto slide deck specified or found.")
  if (!is.character(deck) || length(deck) != 1L || is.na(deck)) {
    stop("Argument 'deck' must be a single character string.")
  }

  # Strip optional .qmd or .html extension while preserving dots in deck stems
  deck <- sub("\\.(qmd|html)$", "", deck, ignore.case = TRUE)

  if (!deck %in% available_slides) {
    ci_idx <- which(tolower(available_slides) == tolower(deck))
    if (length(ci_idx) == 1L) {
      deck <- available_slides[ci_idx]
    } else {
      stop(paste0(
        'Slide deck "', deck, '" was not found in package "', package,
        '". Available decks: ', paste(available_slides, collapse = ", ")
      ))
    }
  }

  slides_dir <- system.file("slides", package = package)
  if (!nzchar(slides_dir)) {
    slides_dir <- file.path(find.package(package), "inst", "slides")
  }

  source_qmd <- file.path(slides_dir, paste0(deck, ".qmd"))
  if (!file.exists(source_qmd)) {
    stop(paste0('Slide deck file not found: "', source_qmd, '".'))
  }

  # Stage slide directory to a writable temporary folder so read-only R
  # libraries work and source `inst/slides/` stays clean
  stage_root <- tempfile("rum_slides_")
  staged_slides <- file.path(stage_root, "slides")
  dir.create(staged_slides, recursive = TRUE, showWarnings = FALSE)

  items <- list.files(
    slides_dir,
    all.files = TRUE,
    full.names = TRUE,
    no.. = TRUE
  )
  file.copy(
    from = items,
    to = staged_slides,
    recursive = TRUE,
    copy.mode = FALSE
  )
  staged_dirs <- list.dirs(staged_slides, full.names = TRUE, recursive = TRUE)
  Sys.chmod(staged_dirs, mode = "0755", use_umask = FALSE)

  staged_qmd <- file.path(staged_slides, paste0(deck, ".qmd"))

  message("Rendering your reveal.js Quarto slideshow...")
  quarto_preview(staged_qmd, ...)

  invisible(NULL)
}
