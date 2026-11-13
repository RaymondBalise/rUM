#' Create a New Quarto Document
#'
#' This function creates a new Quarto document (`.qmd` file) complete with a
#' starter YAML header.
#'
#' @param filename Character string. Name of the file without the `.qmd`
#'   extension. Only letters, numbers, hyphens, and underscores are allowed.
#' @param path Character string. Directory where the file will be created.
#'   Defaults to `here::here()`.
#' @param example Logical or `NULL`. Default `NULL` writes a lean standalone
#'   Quarto document. `TRUE` writes a manuscript template with worked table and
#'   figure cross-references, custom SCSS, and `bib/` citations. `FALSE` writes
#'   the manuscript template with placeholder chunks.
#'
#' @importFrom glue glue
#' @importFrom here here
#' @importFrom rlang is_interactive
#' @importFrom stringr regex str_replace_all
#' @importFrom usethis edit_file
#' @importFrom yaml read_yaml
#'
#' @return Invisibly returns `NULL` (and opens the file when interactive).
#'
#' @examples
#' \dontrun{
#' write_quarto(filename = "data_cleaning", path = tempdir())
#' }
#' @export
write_quarto <- function(filename = NULL, path = here(), example = NULL) {
  # Validate example and filename before creating any directories on disk
  if (!is.null(example) && !.is_bool(example)) {
    stop("Parameter `example` must be NULL, TRUE, or FALSE.")
  }

  if (is.null(filename)) stop("Invalid filename. Please input a value.")
  if (!is.character(filename) || length(filename) != 1L || is.na(filename)) {
    stop("Invalid filename: must be character.")
  }

  filename <- str_replace_all(
    filename,
    regex("\\.qmd$", ignore_case = TRUE),
    ""
  )
  .validate_filename(filename)

  if (
    is.null(path) ||
      !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(trimws(path))
  ) {
    stop("Invalid `path`. Please enter a valid project directory.")
  }
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  .validate_path(path)
  path <- normalizePath(path, mustWork = TRUE)

  if (file.access(path, mode = 2) != 0) {
    stop(glue(
      "You do not have permission to write to the path location: {path}\n",
      "Specify a writable directory via `rUM::write_quarto(..., path = ...)`."
    ))
  }

  the_quarto_file <- file.path(path, paste0(filename, ".qmd"))
  if (file.exists(the_quarto_file)) {
    stop(glue("{filename}.qmd already exists in the specified path."))
  }

  if (
    basename(path) == "vignettes" &&
      file.exists(file.path(dirname(path), "DESCRIPTION"))
  ) {
    message(paste(
      "Note: write_quarto() scaffolds a standalone Quarto document (without",
      "`vignette:` package metadata). Use `usethis::use_vignette()` or",
      "copy the `vignette:` YAML block from `vignettes/analysis.qmd` if",
      "registering a package vignette."
    ))
  }

  scaffold_path <- .get_rum_scaffold()
  manifest <- read_yaml(
    file.path(scaffold_path, "manifests", "quarto_standalone.yml")
  )
  variant <- .resolve_variant(manifest, list(example = example))

  .execute_kit(
    scaffold_path = scaffold_path,
    kit = variant,
    target_path = path,
    placeholders = list(filename = paste0(filename, ".qmd")),
    overwrite = FALSE
  )

  if (!file.exists(the_quarto_file)) {
    stop("The file does not exist.")
  }
  if (is_interactive()) {
    edit_file(the_quarto_file)
  }

  invisible(NULL)
}
