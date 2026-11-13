#' Create a Quarto slide deck template
#'
#' @description
#' Creates a pre-formatted `.qmd` file for presentation slides using reveal.js
#' along with necessary supporting files (SCSS styling and RStudio theme). The
#' generated template includes optimized YAML configuration and slide structure
#' to quickly start building academic and professional presentations. For more
#' information, see `vignette("ah_write_slides", package = "rUM")`.
#'
#' @param filenames Character vector with minimal length of 1. Allows batch
#'   creation of multiple slide decks in one function call.
#' @param path Character string. Directory where the file will be created.
#'   Defaults to the current project's base directory.
#' @param new_folder Character. Default folder is `"slides"`. Options are:
#'
#'   * `"none"`: No subfolder is created. All files (`.qmd` & styling) are
#'   created directly in `path`.
#'
#'   * `"slides_"` (or `"slide_"`): One folder is created by prepending
#'   `"slides_"` to the first entry in `filenames`. If `filenames = "day_1"`,
#'   the folder will be named `"slides_day_1"`.
#'
#'   * A character value naming the subfolder.
#'
#' @param example Logical. Whether to include example slides demonstrating
#'   content layouts.
#'
#' @param template Character. Case-insensitive slide theme template (default:
#'   `"none"`):
#'
#'   * `"none"`: Clean default reveal.js slide template.
#'   * `"miami"`: University of Miami theme.
#'   * `"rmed"`: R/Medicine conference theme.
#'
#' @param format Character string. Slide format to use. Currently supports
#'   `"revealjs"`.
#'
#' @importFrom glue glue
#' @importFrom here here
#' @importFrom purrr walk
#' @importFrom rlang is_interactive
#' @importFrom stringr regex str_detect str_replace_all
#' @importFrom usethis edit_file
#' @importFrom yaml read_yaml
#'
#' @note Specify `path = "inst"` if you are adding slides to an R package (saves
#'   decks into `inst/slides/` so `find_slides()` and `show_slides()` can
#'   discover them).
#'
#' @return Invisibly returns `NULL`. When interactive, the created `.qmd` file
#'   is opened in the IDE editor.
#'
#' @details
#' The function creates:
#'   * A `.qmd` file for each entry in `filenames`
#'   * A `slides.scss` file for custom styling
#'   * An RStudio `.theme` file for code syntax highlighting
#'   * Theme artwork in `img/` when `template = "rmed"`
#'
#' Requires an internet connection on first use to download templates from
#' `rum_templates`; downloaded templates are cached locally for offline use.
#'
#' @examples
#' if (interactive()) {
#'   tmp <- tempdir()
#'   write_slides(filenames = "my_presentation", path = tmp)
#'   write_slides(filenames = "tutorial_slides", path = tmp, example = TRUE)
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
  # 1) Validate scalar flags and strings before touching the filesystem
  if (!.is_bool(example)) {
    stop("Parameter `example` must be TRUE or FALSE.")
  }

  if (
    !is.character(template) ||
      length(template) != 1L ||
      is.na(template)
  ) {
    stop('Parameter `template` must be one of "none", "miami", or "rmed".')
  }
  template <- tolower(trimws(template))
  if (identical(template, "rmed2025")) template <- "rmed"
  valid_templates <- c("none", "miami", "rmed")
  if (!template %in% valid_templates) {
    stop('Parameter `template` must be one of "none", "miami", or "rmed".')
  }

  if (
    !is.character(format) ||
      length(format) != 1L ||
      is.na(format) ||
      tolower(trimws(format)) != "revealjs"
  ) {
    stop('Parameter `format` currently only supports "revealjs".')
  }

  if (is.null(filenames) || length(filenames) == 0L) {
    stop("Invalid filename. Please input a value.")
  }
  if (!is.character(filenames) || anyNA(filenames)) {
    stop("Invalid filename: must be character.")
  }
  filenames <- str_replace_all(
    filenames,
    regex("\\.qmd$", ignore_case = TRUE),
    ""
  )
  walk(filenames, .validate_filename)

  if (
    !is.character(new_folder) ||
      length(new_folder) != 1L ||
      is.na(new_folder)
  ) {
    stop('Invalid "new_folder": must be a single character string.')
  }
  if (!str_detect(new_folder, "^[a-zA-Z0-9_-]+$")) {
    stop(paste(
      'Invalid "new_folder" argument.',
      "Use only letters, numbers, hyphens, and underscores."
    ))
  }

  # 2) Validate path (auto-creating missing package `inst` or `inst/slides`)
  if (
    !is.null(path) &&
      is.character(path) &&
      length(path) == 1L &&
      !is.na(path) &&
      nzchar(trimws(path)) &&
      !dir.exists(path)
  ) {
    base_nm <- basename(path)
    parent_nm <- dirname(path)
    can_make_inst <- base_nm == "inst" && dir.exists(parent_nm)
    can_make_inst_slides <- base_nm == "slides" &&
      basename(parent_nm) == "inst" &&
      (dir.exists(parent_nm) || dir.exists(dirname(parent_nm)))
    if (can_make_inst || can_make_inst_slides) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
  }
  .validate_path(path)
  orig_path <- normalizePath(path, mustWork = TRUE)

  # 3) Resolve target slide directory (avoiding inst/slides/slides nesting)
  slide_dir <- if (new_folder == "none") {
    orig_path
  } else if (new_folder == "slides" && basename(orig_path) == "slides") {
    orig_path
  } else if (new_folder == "slides") {
    file.path(orig_path, "slides")
  } else if (new_folder %in% c("slides_", "slide_")) {
    file.path(orig_path, paste0("slides_", filenames[1]))
  } else {
    file.path(orig_path, new_folder)
  }

  for (slide_file in filenames) {
    candidate <- file.path(slide_dir, paste0(slide_file, ".qmd"))
    if (file.exists(candidate)) {
      stop(glue("{candidate} already exists in the specified path."))
    }
  }

  # 4) Fetch external templates scaffold before mutating the filesystem
  scaffold_path <- .get_rum_scaffold()
  manifest <- read_yaml(file.path(scaffold_path, "manifests", "slides.yml"))

  if (!dir.exists(slide_dir)) dir.create(slide_dir, recursive = TRUE)
  target_dir <- normalizePath(slide_dir, mustWork = TRUE)

  if (file.access(target_dir, mode = 2) != 0) {
    stop(glue(
      "You do not have permission to write to: {target_dir}\n",
      "Specify a writable directory via `rUM::write_slides(..., path = ...)`."
    ))
  }

  scss_already_exists <- file.exists(file.path(target_dir, "slides.scss"))

  if (!is.null(manifest$common)) {
    .execute_kit(
      scaffold_path = scaffold_path,
      kit = manifest$common,
      target_path = target_dir,
      overwrite = FALSE
    )
  }

  variant <- .resolve_variant(
    manifest,
    list(template = template, example = example)
  )
  for (slide_file in filenames) {
    .execute_kit(
      scaffold_path = scaffold_path,
      kit = variant,
      target_path = target_dir,
      placeholders = list(filename = paste0(slide_file, ".qmd")),
      overwrite = FALSE
    )
  }

  if (scss_already_exists) {
    message(glue(
      "A SCSS file has been found in the slides project location & will not ",
      "be modified. Please create the slides in a different folder if you'd ",
      "like to use the SCSS file from the {template} template."
    ))
  }

  in_package <- file.exists(file.path(orig_path, "DESCRIPTION")) ||
    file.exists(file.path(dirname(orig_path), "DESCRIPTION")) ||
    (basename(orig_path) == "slides" &&
      basename(dirname(orig_path)) == "inst" &&
      file.exists(file.path(dirname(dirname(orig_path)), "DESCRIPTION")))

  going_to_inst_slides <- grepl(
    "(^|/|\\\\)inst(/|\\\\)slides($|/|\\\\)",
    target_dir
  )

  if (in_package && !going_to_inst_slides) {
    warning(paste0(
      "rUM has created your slide deck.\n",
      "However, we have detected that you are currently in a package ",
      'environment. Place slide decks inside "inst/slides" (for example via ',
      '`write_slides(..., path = "inst")`) so find_slides() and ',
      "show_slides() can find them and CRAN checks pass."
    ))
  }

  if (is_interactive()) {
    walk(filenames, function(slide_file) {
      the_quarto_file <- file.path(target_dir, paste0(slide_file, ".qmd"))
      if (file.exists(the_quarto_file)) {
        edit_file(the_quarto_file)
      }
    })
  }

  invisible(NULL)
}
