#' Make an Analysis Project
#'
#' @description This function makes an R project that includes an `analysis.Rmd`
#' or `analysis.qmd` file using the `conflicted` and `tidyverse` packages. This
#' project automatically includes an aggressive `.gitignore` designed to help
#' protect against leaking data (with protected health information), starter
#' bibliography files (`bib/references.bib` and `bib/packages.bib`), Citation
#' Style Language (`.csl`) files in `bib/` for APA 7th (default), MLA 9th, and
#' the New England Journal of Medicine, and AI coding assistant skills.
#'
#' @param path Path to the project directory (also used by
#'   `research_project.dcf` in `./rstudio/templates/project/`).
#' @param type Choose between `"Quarto (analysis.qmd)"` (`"Q"`) or
#'   `"R Markdown (analysis.Rmd)"` (`"R"`). Case-insensitive abbreviations
#'   (`"q"`, `"quarto"`, `"r"`, `"rmd"`) are accepted.
#' @param example Logical. Will the analysis file include an example
#'   table/figure?
#' @param vignette `r lifecycle::badge("deprecated")` Use `make_package()`
#'   instead.
#' @param overwrite Logical. When `FALSE` (default), existing `.Rproj` and
#'   supporting files (`.gitignore`, `README.md`, `custom.scss`, `AGENTS.md`)
#'   are preserved untouched. When `TRUE` (useful on Posit.Cloud), existing
#'   `.Rproj` and supporting files are replaced. Existing `analysis.qmd` or
#'   `analysis.Rmd` files are never overwritten.
#' @param openInteractive Logical. Should this new project be opened in a new
#'   IDE window when interactive? Defaults to `TRUE`.
#'
#' @importFrom lifecycle deprecate_warn
#' @importFrom quarto quarto_version
#' @importFrom rlang abort is_interactive
#' @importFrom usethis create_project
#'
#' @details Behind the scenes, this function is used by `research_project.dcf`
#' when a user selects New project... > New Directory > rUM Research Project
#' Template within the RStudio IDE.
#'
#' Requires an internet connection on first use to download templates from
#' `rum_templates`; downloaded templates are cached locally for offline use (or
#' override via the `RUM_TEMPLATES_PATH` environment variable).
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Make a project with an example Quarto paper
#'   make_project(
#'     path = "~/test_project_quarto",
#'     type = "Quarto (analysis.qmd)",
#'     example = TRUE
#'   )
#'
#'   # Abbreviations on project type: "Q" for Quarto or "R" for R Markdown
#'   make_project(path = "~/test_project_q", "Q", example = TRUE)
#'   make_project(path = "~/test_project_rmd", "R", example = TRUE)
#' }
make_project <- function(
  path,
  type = c("Quarto (analysis.qmd)", "R Markdown (analysis.Rmd)"),
  example = FALSE,
  vignette = FALSE,
  overwrite = FALSE,
  openInteractive = TRUE
) {
  # Input validation -----------------------------------------------------------
  if (
    missing(path) ||
      is.null(path) ||
      !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(trimws(path))
  ) {
    abort("Parameter `path` must be a non-empty character string.")
  }

  if (!.is_bool(vignette)) {
    stop("Parameter `vignette` must be TRUE or FALSE")
  }

  is_quarto_project <- .arg_validation(
    type,
    example,
    overwrite,
    openInteractive
  )

  # If the user chooses make_project() to create an R package, warn & delegate:
  if (isTRUE(vignette)) {
    deprecate_warn(
      when = "2.2.0",
      what = "make_project(vignette)",
      with = "make_package()"
    )
    return(make_package(
      path = path,
      type = type,
      example = example,
      overwrite = overwrite,
      openInteractive = openInteractive
    ))
  }

  # Prevent overwriting an existing analysis file before touching disk
  if (
    dir.exists(path) &&
      (file.exists(file.path(path, "analysis.Rmd")) ||
        file.exists(file.path(path, "analysis.qmd")))
  ) {
    abort("The directory you choose already has an analysis file. Stopping.")
  }

  if (is_quarto_project) {
    q_ver <- tryCatch(quarto_version(), error = function(e) NULL)
    if (is.null(q_ver)) {
      warning(paste0(
        "Quarto CLI was not found on PATH. Install Quarto from ",
        "https://quarto.org/docs/download/ to render `analysis.qmd`."
      ))
    }
  }

  # Ensure external templates can be resolved before creating .Rproj skeleton
  scaffold_path <- .get_rum_scaffold()

  # Track pre-existing .gitignore before usethis::create_project() writes its
  # 1-line `.Rproj.user` stub on brand-new or uninitialized directories
  had_gitignore <- dir.exists(path) &&
    file.exists(file.path(path, ".gitignore"))

  # Create directory and perform initial project setup
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  has_rproj <- length(list.files(path = path, pattern = "\\.Rproj$")) > 0
  should_open <- isTRUE(openInteractive) && is_interactive()
  if (!has_rproj || overwrite) {
    create_project(path = path, open = should_open, rstudio = TRUE)
  }

  path <- normalizePath(path, mustWork = TRUE)

  # Scaffold project from external templates manifest
  format_input <- if (is_quarto_project) "quarto" else "rmd"
  .scaffold_from_manifest(
    manifest_name = "project",
    inputs = list(format = format_input, example = example),
    target_path = path,
    overwrite = overwrite
  )

  if (!had_gitignore && !overwrite) {
    .copy_template_file(
      src = file.path(scaffold_path, "supporting", ".gitignore.md"),
      dst = file.path(path, ".gitignore"),
      scaffold_path = scaffold_path,
      overwrite = TRUE
    )
  }

  invisible(NULL)
}
