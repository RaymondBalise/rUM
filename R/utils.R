# Collection of rUM helper functions: -----------------------------------------

#' Check scalar non-NA logical
#' @noRd
.is_bool <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

#' Argument validation
#' @description
#' Argument validation function for `make_project()` and `make_package()`.
#'
#' @param type Project format (`"Quarto (analysis.qmd)"` / `"Q"` or
#'   `"R Markdown (analysis.Rmd)"` / `"R"`).
#' @param example Logical scalar. Include worked table/figure example?
#' @param overwrite Logical scalar. Overwrite existing `.Rproj` and support
#'   files?
#' @param openInteractive Logical scalar. Open project in a new IDE window?
#'
#' @importFrom rlang abort
#'
#' @return Logical scalar (`TRUE` for Quarto, `FALSE` for R Markdown).
#'
#' @noRd
.arg_validation <- function(
  type = c("Quarto (analysis.qmd)", "R Markdown (analysis.Rmd)"),
  example,
  overwrite,
  openInteractive
) {
  # 1) Normalize and validate `type` (case-insensitive abbreviations supported)
  if (is.null(type)) {
    type <- "Quarto (analysis.qmd)"
  }
  if (!is.character(type)) {
    stop("Argument 'type' must be a character string.")
  }
  if (length(type) > 1L) {
    default_choices <- c("Quarto (analysis.qmd)", "R Markdown (analysis.Rmd)")
    if (identical(type, default_choices)) {
      type <- type[1L]
    } else {
      abort(paste(
        "Parameter `type` must be a single string:",
        "'Quarto (analysis.qmd)' ('Q') or 'R Markdown (analysis.Rmd)' ('R')."
      ))
    }
  }
  if (length(type) != 1L || is.na(type)) {
    abort(paste(
      "Parameter `type` must be 'Quarto (analysis.qmd)' ('Q') or",
      "'R Markdown (analysis.Rmd)' ('R')."
    ))
  }

  norm_type <- tolower(trimws(type))
  quarto_aliases <- c("q", "qmd", "quarto", "quarto (analysis.qmd)")
  rmd_aliases <- c(
    "r", "rmd", "rmarkdown", "r markdown", "r markdown (analysis.rmd)"
  )

  if (norm_type %in% quarto_aliases) {
    is_quarto_project <- TRUE
  } else if (norm_type %in% rmd_aliases) {
    is_quarto_project <- FALSE
  } else {
    abort(paste(
      "The type must be 'Quarto (analysis.qmd)' ('Q') or",
      "'R Markdown (analysis.Rmd)' ('R')."
    ))
  }

  # 2) Validate scalar booleans before any disk writes
  if (!.is_bool(example)) {
    stop("Parameter `example` must be TRUE or FALSE")
  }
  if (!.is_bool(overwrite)) {
    stop("Parameter `overwrite` must be TRUE or FALSE")
  }
  if (!.is_bool(openInteractive)) {
    stop("Parameter `openInteractive` must be TRUE or FALSE")
  }

  is_quarto_project
}


#------------------------------------------------------------------------------
#' Path validation
#'
#' @description
#' Validate path to output file(s) specified by user.
#'
#' @param path Character string.
#'
#' @noRd
.validate_path <- function(path) {
  if (
    is.null(path) ||
      !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(trimws(path)) ||
      !dir.exists(path)
  ) {
    stop("Invalid `path`. Please enter a valid project directory.")
  }
}


#------------------------------------------------------------------------------
#' Filename validation
#'
#' @description
#' Validate output filename stem specified by user.
#'
#' @param filename Character string.
#'
#' @importFrom stringr str_detect
#'
#' @noRd
.validate_filename <- function(filename) {
  if (!is.character(filename) || length(filename) != 1L || is.na(filename)) {
    stop("Invalid filename: must be character.")
  }

  if (str_detect(filename, "/")) {
    stop(paste(
      "Invalid filename. You included a forward slash in the file name.",
      "If you are trying to give a folder/directory location, use the",
      "`path =` argument."
    ))
  }
  if (str_detect(filename, "\\\\")) {
    stop(paste(
      "Invalid filename. You included a backslash in the file name.",
      "If you are trying to give a folder/directory location, use the",
      "`path =` argument."
    ))
  }
  if (str_detect(filename, "\\.")) {
    stop(paste(
      "Invalid filename. Omit file extensions and periods;",
      "use only letters, numbers, hyphens, and underscores."
    ))
  }
  if (!str_detect(filename, "^[a-zA-Z0-9_-]+$")) {
    stop(
      "Invalid filename. Use only letters, numbers, hyphens, and underscores."
    )
  }
}


# Manifest-driven template infrastructure -------------------------------------

#' Session cache for downloaded scaffold
#' @noRd
.rum_scaffold_cache <- new.env(parent = emptyenv())

#' Unzip and validate a rum_templates archive
#' @noRd
.extract_valid_scaffold <- function(zip_file) {
  if (!file.exists(zip_file)) return(NULL)
  out_dir <- tempfile("rum_")
  suppressWarnings(unzip(zip_file, exdir = out_dir))
  dirs <- list.dirs(out_dir, recursive = FALSE)
  if (length(dirs) == 0L || !dir.exists(file.path(dirs[1], "manifests"))) {
    return(NULL)
  }
  dirs[1]
}

#------------------------------------------------------------------------------
#' Get rUM Templates Scaffold
#' @description
#' Resolves the `rum_templates` directory via `RUM_TEMPLATES_PATH` (if set),
#' session cache, live GitHub download (persisting an offline fallback copy in
#' `tools::R_user_dir("rUM", which = "cache")`), or the cached zip when offline.
#'
#' @importFrom tools R_user_dir
#' @importFrom utils download.file unzip
#'
#' @return Character path to the scaffold root directory.
#'
#' @noRd
.get_rum_scaffold <- function() {
  local_override <- Sys.getenv("RUM_TEMPLATES_PATH", unset = "")
  if (nzchar(local_override)) {
    if (
      !dir.exists(local_override) ||
        !dir.exists(file.path(local_override, "manifests"))
    ) {
      stop(paste(
        "RUM_TEMPLATES_PATH does not point to a valid rum_templates",
        "directory containing `manifests/`."
      ))
    }
    return(normalizePath(local_override, mustWork = TRUE))
  }

  if (
    !is.null(.rum_scaffold_cache$path) &&
      !is.na(.rum_scaffold_cache$path) &&
      dir.exists(.rum_scaffold_cache$path)
  ) {
    return(.rum_scaffold_cache$path)
  }

  in_check_or_test <- identical(Sys.getenv("TESTTHAT"), "true") ||
    nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) ||
    nzchar(Sys.getenv("_R_CHECK_THINGS_IN_OTHER_DIRS_"))
  cache_dir <- getOption(
    "rUM.cache_dir",
    if (in_check_or_test) {
      file.path(tempdir(), "rUM_cache")
    } else {
      R_user_dir("rUM", which = "cache")
    }
  )
  cached_zip <- file.path(cache_dir, "templates.zip")

  url <- paste0(
    "https://github.com/RaymondBalise/",
    "rum_templates/archive/refs/heads/main.zip"
  )
  zip_path <- tempfile(fileext = ".zip")

  dl_code <- tryCatch(
    suppressWarnings(download.file(url, zip_path, mode = "wb", quiet = TRUE)),
    error = function(e) 1L
  )

  if (identical(dl_code, 0L) && file.exists(zip_path)) {
    scaffold_root <- .extract_valid_scaffold(zip_path)
    if (!is.null(scaffold_root)) {
      tryCatch(
        {
          if (!dir.exists(cache_dir)) {
            dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
          }
          file.copy(zip_path, cached_zip, overwrite = TRUE)
        },
        error = function(e) NULL
      )
      .rum_scaffold_cache$path <- scaffold_root
      return(scaffold_root)
    }

    # Captive-portal HTML response: fall back to cached archive if available
    fallback_root <- .extract_valid_scaffold(cached_zip)
    if (!is.null(fallback_root)) {
      message("Offline: using cached rUM templates.")
      .rum_scaffold_cache$path <- fallback_root
      return(fallback_root)
    }

    stop(paste(
      "Downloaded template archive is invalid or incomplete.",
      "Please check your internet connection."
    ))
  }

  # Offline download failure: fall back to persistent cached archive if present
  fallback_root <- .extract_valid_scaffold(cached_zip)
  if (!is.null(fallback_root)) {
    message("Offline: using cached rUM templates.")
    .rum_scaffold_cache$path <- fallback_root
    return(fallback_root)
  }

  stop(paste(
    "Could not download templates from GitHub.",
    "Please check your internet connection."
  ))
}

#------------------------------------------------------------------------------
#' Validate relative manifest path against parent traversal
#' @noRd
.check_safe_manifest_path <- function(rel_path) {
  if (grepl("(^|/|\\\\)\\.\\.(/|\\\\|$)", rel_path)) {
    stop(paste("Unsafe parent traversal in manifest path:", rel_path))
  }
}

#------------------------------------------------------------------------------
#' Resolve Variant from Manifest
#' @description
#' Matches user inputs against the `when` conditions in a manifest to select
#' the appropriate template variant.
#'
#' @param manifest Parsed YAML manifest list.
#' @param inputs Named list of user inputs to match against variant conditions.
#'
#' @return The matched variant list.
#'
#' @noRd
.resolve_variant <- function(manifest, inputs) {
  variants <- manifest$variants

  for (v_name in names(variants)) {
    v <- variants[[v_name]]
    conditions <- v$when

    if (is.null(conditions) || length(conditions) == 0L) {
      stop(paste("Manifest variant", v_name, "is missing a `when` condition."))
    }

    is_match <- TRUE
    for (cond_key in names(conditions)) {
      expected <- conditions[[cond_key]]
      actual <- inputs[[cond_key]]

      # Normalize NULLs for comparison
      if (is.null(expected)) expected <- "null"
      if (is.null(actual)) actual <- "null"

      if (as.character(actual) != as.character(expected)) {
        is_match <- FALSE
        break
      }
    }

    if (is_match) return(v)
  }

  stop("Could not find a matching template variant for the provided inputs.")
}

#------------------------------------------------------------------------------
#' Copy Template File with Optional Partial Injection
#' @description
#' Copies a file from the template scaffold to the target destination, splicing
#' any `{{token}}` placeholders defined in `inject`.
#'
#' @param src Full path to the source template file.
#' @param dst Full path to the destination file.
#' @param scaffold_path Root path of the downloaded template scaffold.
#' @param inject Optional named list mapping token names to partial relative
#'   paths inside `scaffold_path`.
#' @param overwrite Logical. Overwrite `dst` if it already exists.
#'
#' @importFrom stringr fixed str_replace_all
#'
#' @noRd
.copy_template_file <- function(
  src,
  dst,
  scaffold_path,
  inject = NULL,
  overwrite = TRUE
) {
  if (!file.exists(src)) {
    stop(paste("Could not find template source file:", basename(src)))
  }

  if (!overwrite && file.exists(dst)) {
    return(invisible(FALSE))
  }

  if (!dir.exists(dirname(dst))) {
    dir.create(dirname(dst), recursive = TRUE)
  }

  if (is.null(inject) || length(inject) == 0L) {
    copied <- file.copy(from = src, to = dst, overwrite = overwrite)
    if (!isTRUE(copied)) {
      stop(paste("Failed to copy template file to:", dst))
    }
    return(invisible(TRUE))
  }

  content <- paste(readLines(src, warn = FALSE), collapse = "\n")
  for (token in names(inject)) {
    .check_safe_manifest_path(inject[[token]])
    partial_file <- file.path(scaffold_path, inject[[token]])
    if (!file.exists(partial_file)) {
      stop(paste("Could not find partial file:", inject[[token]]))
    }
    partial_text <- paste(
      readLines(partial_file, warn = FALSE),
      collapse = "\n"
    )
    content <- str_replace_all(
      content,
      fixed(paste0("{{", token, "}}")),
      partial_text
    )
  }

  writeLines(content, con = dst)
  invisible(TRUE)
}

#------------------------------------------------------------------------------
#' Execute Template Kit
#' @description
#' Performs the file copies and actions defined in a manifest kit.
#'
#' @param scaffold_path Path to the downloaded scaffold root.
#' @param kit A list with `files` and/or `actions` from a manifest.
#' @param target_path The destination project/package path.
#' @param placeholders Named list of placeholder replacements for file paths.
#' @param context Named list of additional context passed to action dispatcher.
#' @param overwrite Logical. Overwrite existing files when copying.
#'
#' @importFrom stringr fixed str_replace_all
#'
#' @noRd
.execute_kit <- function(
  scaffold_path,
  kit,
  target_path,
  placeholders = list(),
  context = list(),
  overwrite = TRUE
) {
  context$overwrite <- overwrite

  .fill <- function(str) {
    s <- str_replace_all(str, fixed("{path}"), target_path)
    for (n in names(placeholders)) {
      s <- str_replace_all(
        s,
        fixed(paste0("{", n, "}")),
        as.character(placeholders[[n]])
      )
    }
    s
  }

  # 1. Handle Files
  if (!is.null(kit$files)) {
    for (f in kit$files) {
      .check_safe_manifest_path(f$src)
      .check_safe_manifest_path(f$dst)
      src <- file.path(scaffold_path, f$src)
      dst <- .fill(f$dst)

      # Paths without {path} placeholder are relative to target_path
      if (!grepl("\\{path\\}", f$dst)) {
        dst <- file.path(target_path, dst)
      }

      .copy_template_file(
        src = src,
        dst = dst,
        scaffold_path = scaffold_path,
        inject = f$inject,
        overwrite = overwrite
      )
    }
  }

  # 2. Handle Actions
  if (!is.null(kit$actions)) {
    for (act in kit$actions) {
      .dispatch_action(act, target_path, context)
    }
  }
}

#------------------------------------------------------------------------------
#' Action Dispatcher
#' @description
#' Executes a single action from a manifest kit.
#'
#' @param action A list describing the action (type, path, etc.).
#' @param target_path The destination project/package path.
#' @param context Named list of additional context (e.g., is_quarto_project,
#'   overwrite).
#'
#' @noRd
.dispatch_action <- function(action, target_path, context = list()) {
  type <- action$type
  is_overwrite <- !isFALSE(context$overwrite)

  path <- if (!is.null(action$path)) {
    .check_safe_manifest_path(action$path)
    resolved <- str_replace_all(action$path, fixed("{path}"), target_path)
    if (!grepl("\\{path\\}", action$path)) {
      file.path(target_path, resolved)
    } else {
      resolved
    }
  } else {
    target_path
  }

  switch(
    type,
    "create_dir" = {
      if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    },
    "create_bib" = {
      if (!dir.exists(path)) dir.create(path, recursive = TRUE)
      if (!file.exists(file.path(path, "packages.bib"))) {
        writeLines("", con = file.path(path, "packages.bib"))
      }
      if (!file.exists(file.path(path, "references.bib"))) {
        writeLines("", con = file.path(path, "references.bib"))
      }
    },
    "build_description" = {
      is_quarto <- isTRUE(context$is_quarto_project)
      .rUM_build_description(is_quarto_project = is_quarto, path = target_path)
    },
    "write_readme" = {
      readme_dst <- file.path(target_path, "README.md")
      if (file.exists(readme_dst) && !is_overwrite) {
        return(invisible(NULL))
      }
      scaffold_path <- .get_rum_scaffold()
      readme_src <- file.path(scaffold_path, "supporting/README.md")
      .copy_template_file(
        src = readme_src,
        dst = readme_dst,
        scaffold_path = scaffold_path,
        overwrite = is_overwrite
      )
    },
    "write_scss" = {
      scss_dst <- file.path(target_path, "custom.scss")
      if (file.exists(scss_dst) && !is_overwrite) {
        return(invisible(NULL))
      }
      scaffold_path <- .get_rum_scaffold()
      scss_src <- file.path(scaffold_path, "styling/custom.scss")
      .copy_template_file(
        src = scss_src,
        dst = scss_dst,
        scaffold_path = scaffold_path,
        overwrite = is_overwrite
      )
    },
    stop(paste("Unknown action type:", type))
  )
}

#------------------------------------------------------------------------------
#' Scaffold from Manifest
#' @description
#' High-level helper that downloads the scaffold, reads a manifest, resolves
#' the variant, and executes both common and variant kits.
#'
#' @param manifest_name Name of the manifest file (without .yml extension).
#' @param inputs Named list of inputs to match variant conditions.
#' @param target_path Destination path for the scaffolded project.
#' @param placeholders Named list of placeholder replacements for file paths.
#' @param context Named list of additional context passed to actions.
#' @param overwrite Logical. Overwrite existing support files when copying.
#'
#' @importFrom yaml read_yaml
#'
#' @return Invisibly returns a list with scaffold_path and resolved variant.
#'
#' @noRd
.scaffold_from_manifest <- function(
  manifest_name,
  inputs,
  target_path,
  placeholders = list(),
  context = list(),
  overwrite = TRUE
) {
  scaffold_path <- .get_rum_scaffold()
  manifest_file <- file.path(
    scaffold_path,
    "manifests",
    paste0(manifest_name, ".yml")
  )
  if (!file.exists(manifest_file)) {
    stop(paste("Could not find manifest file:", paste0(manifest_name, ".yml")))
  }
  manifest <- read_yaml(manifest_file)
  variant <- .resolve_variant(manifest, inputs)

  # Execute common files + actions
  if (!is.null(manifest$common)) {
    .execute_kit(
      scaffold_path = scaffold_path,
      kit = manifest$common,
      target_path = target_path,
      placeholders = placeholders,
      context = context,
      overwrite = overwrite
    )
  }

  # Execute variant files + actions
  .execute_kit(
    scaffold_path = scaffold_path,
    kit = variant,
    target_path = target_path,
    placeholders = placeholders,
    context = context,
    overwrite = overwrite
  )

  invisible(list(scaffold_path = scaffold_path, variant = variant))
}
