#' Make an Analysis Package
#'
#' @description This function creates the structure for an R package with an
#' `analysis.qmd` (or `analysis.Rmd`) manuscript in the `vignettes/` directory.
#' This package includes an aggressive `.gitignore` designed to help protect
#' against leaking data (with protected health information), starter
#' bibliography and Citation Style Language (`.csl`) files in `vignettes/bib/`
#' for APA 7th (default), MLA 9th, and the New England Journal of Medicine,
#' starter scripts in `inst/scripts/` and `inst/extdata/`, and AI coding
#' assistant skills.
#'
#' @param path Path to the new package directory. Its basename must be a valid
#'   CRAN R package name (at least two characters, starting with a letter,
#'   containing only letters, numbers, and periods, and not ending with a
#'   period).
#' @param type Choose between `"Quarto (analysis.qmd)"` (`"Q"`) or
#'   `"R Markdown (analysis.Rmd)"` (`"R"`). Case-insensitive abbreviations
#'   (`"q"`, `"quarto"`, `"r"`, `"rmd"`) are accepted.
#' @param example Logical. Will the analysis file include an example
#'   table/figure?
#' @param overwrite Logical. Overwrite an existing `.Rproj` and supporting
#'   files in `path`? Required on Posit.Cloud (`make_package("./", overwrite =
#'   TRUE)`). Existing `analysis.qmd` / `analysis.Rmd` files are never
#'   overwritten.
#' @param openInteractive Logical. Should this new package be opened in a new
#'   IDE window when interactive? Defaults to `TRUE`.
#'
#' @importFrom fs path_abs
#' @importFrom quarto quarto_version
#' @importFrom rlang abort is_interactive
#' @importFrom usethis create_package
#'
#' @details Requires an internet connection on first use to download templates
#' from `rum_templates`; downloaded templates are cached locally for offline use
#' (or override via the `RUM_TEMPLATES_PATH` environment variable).
#'
#' @note If you need to create multiple packages from the same session, restart
#'   RStudio/Positron after making each one. See
#'   [open issue](https://github.com/RaymondBalise/rUM/issues/84).
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Make a package with an example Quarto paper in vignettes/
#'   make_package(
#'     path = "~/testpkgquarto",
#'     type = "Quarto (analysis.qmd)",
#'     example = TRUE
#'   )
#'
#'   # Abbreviations on project type: "Q" for Quarto or "R" for R Markdown
#'   make_package(path = "~/testpkgq", "Q", TRUE)
#'   make_package(path = "~/testpkgrmd", "R", example = TRUE)
#' }
make_package <- function(
  path,
  type = c("Quarto (analysis.qmd)", "R Markdown (analysis.Rmd)"),
  example = FALSE,
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

  is_quarto_project <- .arg_validation(
    type,
    example,
    overwrite,
    openInteractive
  )

  # 1) Validate package directory name before touching disk
  abs_path <- path_abs(path)
  dir_name_char <- basename(abs_path)
  if (!.valid_package_name(path = dir_name_char)) {
    abort(paste0(
      '"', dir_name_char, '" is not a valid R package name. ',
      "Package names must have at least two characters, start with a letter, ",
      "contain only letters, numbers, and periods, and not end with a period."
    ))
  }

  # 2) Check Quarto CLI version only when building a Quarto vignette package
  if (is_quarto_project) {
    the_version <- tryCatch(quarto_version(), error = function(e) NULL)
    if (is.null(the_version) || the_version < "1.4.549") {
      abort(paste0(
        "STOPPING: You need a modern version of Quarto (>= 1.4.549) from ",
        "https://quarto.org/docs/download/ in order to make a package ",
        "with a Quarto vignette."
      ))
    }
  }

  # 3) Protect existing analysis files first (before .Rproj check)
  if (
    dir.exists(path) &&
      (file.exists(file.path(path, "vignettes", "analysis.Rmd")) ||
        file.exists(file.path(path, "vignettes", "analysis.qmd")) ||
        file.exists(file.path(path, "analysis.Rmd")) ||
        file.exists(file.path(path, "analysis.qmd")))
  ) {
    abort("The directory you choose already has an analysis file. Stopping.")
  }

  has_rproj <- dir.exists(path) &&
    length(list.files(path = path, pattern = "\\.Rproj$")) > 0

  if (has_rproj && !overwrite) {
    abort(paste0(
      "STOPPING: The folder/directory you chose to hold the package ",
      "already has an RStudio project file (.Rproj) in it. If you want to ",
      "create a package in a directory/folder that already has an RStudio ",
      "project in it, add the overwrite = TRUE option when you use ",
      "make_package()."
    ))
  }

  # 4) Ensure external templates resolve before creating package skeleton
  scaffold_path <- .get_rum_scaffold()

  # Track pre-existing .gitignore before usethis::create_package() writes its
  # 1-line `.Rproj.user` stub on brand-new directories
  had_gitignore <- dir.exists(path) &&
    file.exists(file.path(path, ".gitignore"))

  parent_dir <- dirname(abs_path)
  if (!dir.exists(parent_dir)) {
    dir.create(parent_dir, recursive = TRUE, showWarnings = FALSE)
  }

  should_open <- isTRUE(openInteractive) && is_interactive()
  if (!has_rproj || overwrite) {
    create_package(path = path, open = should_open, rstudio = TRUE)
  }

  path <- normalizePath(path, mustWork = TRUE)

  # 5) Scaffold package from external templates manifest
  format_input <- if (is_quarto_project) "quarto" else "rmd"
  .scaffold_from_manifest(
    manifest_name = "package",
    inputs = list(format = format_input, example = example),
    target_path = path,
    context = list(is_quarto_project = is_quarto_project),
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

  .add_rum_Rbuildignore(path = path)

  invisible(NULL)
}


#------------------------------------------------------------------------------
# Helper functions for make_package()
#------------------------------------------------------------------------------
#' Check for valid CRAN package name
#' @description
#' Parses the provided `rUM::make_package()` path basename.
#'
#' @param path Character. Package directory basename.
#'
#' @importFrom stringr str_detect
#'
#' @noRd
.valid_package_name <- function(path) {
  is.character(path) &&
    length(path) == 1L &&
    !is.na(path) &&
    str_detect(path, "^[a-zA-Z][a-zA-Z0-9.]+$") &&
    !str_detect(path, "\\.$")
}


#------------------------------------------------------------------------------
#' Add non-package root files to .Rbuildignore
#' @description
#' Appends rUM agent configuration files, `DECISIONS.md`, and pipeline summary
#' artifacts to `.Rbuildignore` without duplicating existing patterns.
#'
#' @param path Character. Path to package root directory.
#'
#' @noRd
.add_rum_Rbuildignore <- function(path) {
  rbuildignore_path <- file.path(path, ".Rbuildignore")
  existing <- if (file.exists(rbuildignore_path)) {
    readLines(rbuildignore_path, warn = FALSE)
  } else {
    character(0)
  }

  patterns <- c(
    "^AGENTS\\.md$",
    "^CLAUDE\\.md$",
    "^GEMINI\\.md$",
    "^\\.agents$",
    "^DECISIONS\\.md$",
    "do_everything_summary\\.RData$"
  )

  to_add <- setdiff(patterns, existing)
  if (length(to_add) > 0L) {
    writeLines(c(existing, to_add), con = rbuildignore_path)
  }
}


#------------------------------------------------------------------------------
#' Add content to the DESCRIPTION file
#' @description
#' Populates `Suggests` dependencies and `VignetteBuilder` in `DESCRIPTION`
#' without creating duplicate DCF fields.
#'
#' @param is_quarto_project Logical. Quarto (`TRUE`) vs R Markdown (`FALSE`).
#' @param path Character. Path to package root directory.
#'
#' @importFrom usethis local_project use_package
#'
#' @noRd
.rUM_build_description <- function(
  is_quarto_project,
  path = "."
) {
  local_project(path, quiet = TRUE, force = TRUE)

  suppressMessages({
    use_package("cli", type = "suggests")
    use_package("here", type = "suggests")
    use_package("knitr", type = "suggests")
    use_package("rmarkdown", type = "suggests")
    use_package("roxygen2", type = "suggests")

    use_package("conflicted", type = "suggests")
    use_package("glue", type = "suggests")
    use_package("gtsummary", type = "suggests", min_version = "2.0.3")
    use_package("readr", type = "suggests")
    use_package("rUM", type = "suggests")
    use_package("rio", type = "suggests")
    use_package("stringr", type = "suggests")
    use_package("table1", type = "suggests")
    use_package("tidymodels", type = "suggests")
    use_package("tidyverse", type = "suggests")

    if (is_quarto_project) {
      use_package("quarto", type = "suggests", min_version = "1.3.12")
    } else {
      use_package("bookdown", type = "suggests")
    }
  })

  builder <- if (is_quarto_project) "quarto" else "knitr"
  desc_file <- file.path(path, "DESCRIPTION")
  desc_lines <- readLines(desc_file, warn = FALSE)

  # Add optional arrow to Suggests without usethis::use_package() host check
  sug_idx <- grep("^Suggests:", desc_lines)
  if (length(sug_idx) == 1L && !any(grepl("^\\s*arrow\\b", desc_lines))) {
    desc_lines <- append(desc_lines, "    arrow,", after = sug_idx)
  }

  vb_line <- paste0("VignetteBuilder: ", builder)

  if (any(grepl("^VignetteBuilder:", desc_lines))) {
    desc_lines[grepl("^VignetteBuilder:", desc_lines)] <- vb_line
    writeLines(desc_lines, con = desc_file)
  } else {
    writeLines(c(desc_lines, vb_line), con = desc_file)
  }
}
