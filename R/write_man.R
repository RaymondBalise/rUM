#' Write a manual page template for package dataset documentation
#'
#' @description
#' Generates a `roxygen2` dataset documentation file inside `R/` (for example,
#' passing `penguins_clean` creates `R/penguins_clean.R`). Run
#' `devtools::document()` (or `roxygen2::roxygenise()`) afterward to compile
#' `man/<dataset>.Rd`. The generated template records whether the dataset is a
#' `data.frame` or `tibble`, its row and column counts, and each variable's
#' type, factor levels (if applicable), and variable label (from `labelled` /
#' `haven` / `tidyREDCap` when present).
#'
#' @param the_dataset Dataset object (unquoted, such as `mtcars` or
#'   `palmerpenguins::penguins`) or dataset name as a character string
#'   (`"mtcars"`).
#'
#' @importFrom labelled var_label
#' @importFrom stringr str_squish
#'
#' @note Ensure `Roxygen: list(markdown = TRUE)` is set in `DESCRIPTION`
#'   (configured automatically by `rUM::make_package()`), save the dataset to
#'   `data/` with `usethis::use_data()`, and run `devtools::document()` to
#'   generate `man/<dataset>.Rd`.
#'
#' @return Invisibly returns `NULL` after writing `R/<dataset>.R`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Inside an R package root containing an R/ folder:
#'   write_man(mtcars)
#'
#'   # Or pass a dataset name as a quoted string:
#'   iris_copy <- iris
#'   write_man("iris_copy")
#' }
write_man <- function(the_dataset) {
  if (missing(the_dataset)) {
    stop('Argument "the_dataset" is missing, with no default.')
  }

  expr_sub <- substitute(the_dataset)
  caller_env <- parent.frame()

  if (is.symbol(expr_sub)) {
    sym_name <- as.character(expr_sub)
    if (
      !exists(sym_name, envir = caller_env) &&
        !exists(sym_name, envir = .GlobalEnv)
    ) {
      stop("Dataset '", sym_name, "' not found in the global environment")
    }
    val <- if (exists(sym_name, envir = caller_env)) {
      get(sym_name, envir = caller_env)
    } else {
      get(sym_name, envir = .GlobalEnv)
    }
    if (is.character(val) && length(val) == 1L) {
      the_dataset_name <- sub("\\.[rR]$", "", val)
      if (
        !exists(the_dataset_name, envir = caller_env) &&
          !exists(the_dataset_name, envir = .GlobalEnv)
      ) {
        stop(
          "Dataset '", the_dataset_name, "' not found in the global environment"
        )
      }
      the_dataset <- if (exists(the_dataset_name, envir = caller_env)) {
        get(the_dataset_name, envir = caller_env)
      } else {
        get(the_dataset_name, envir = .GlobalEnv)
      }
    } else {
      the_dataset_name <- sym_name
      the_dataset <- val
    }
  } else if (is.character(the_dataset) && length(the_dataset) == 1L) {
    if (is.na(the_dataset) || !nzchar(trimws(the_dataset))) {
      stop("Invalid dataset name.")
    }
    the_dataset_name <- sub("\\.[rR]$", "", the_dataset)
    if (
      !exists(the_dataset_name, envir = caller_env) &&
        !exists(the_dataset_name, envir = .GlobalEnv)
    ) {
      stop(
        "Dataset '", the_dataset_name, "' not found in the global environment"
      )
    }
    the_dataset <- if (exists(the_dataset_name, envir = caller_env)) {
      get(the_dataset_name, envir = caller_env)
    } else {
      get(the_dataset_name, envir = .GlobalEnv)
    }
  } else {
    expr_str <- deparse1(expr_sub)
    the_dataset_name <- sub("^.*::+", "", expr_str)
    the_dataset <- tryCatch(
      force(the_dataset),
      error = function(e) {
        stop("Dataset '", expr_str, "' not found in the global environment")
      }
    )
  }

  if (!grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", the_dataset_name)) {
    stop(paste0(
      "Dataset name '", the_dataset_name,
      "' is not a valid R object/file name (avoid spaces and special ",
      "characters)."
    ))
  }

  if (!inherits(the_dataset, "data.frame")) {
    stop("Object '", the_dataset_name, "' is not a data.frame or tibble")
  }

  if (!dir.exists("R")) {
    stop("The R folder does not exist in the current directory")
  }

  r_file_path <- file.path("R", paste0(the_dataset_name, ".R"))
  if (file.exists(r_file_path)) {
    stop("Documentation file '", r_file_path, "' already exists")
  }

  is_tibble <- inherits(the_dataset, "tbl_df")
  n_rows_formatted <- format(
    nrow(the_dataset),
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
  n_cols_formatted <- format(
    ncol(the_dataset),
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
  data_structure <- if (is_tibble) "tibble" else "data.frame"

  out <- c(
    paste0("#' ", the_dataset_name, " dataset"),
    "#'",
    paste0(
      "#' @description Description of the ",
      the_dataset_name,
      " dataset goes here"
    ),
    "#'",
    paste0(
      "#' @format A ", data_structure, " with ",
      n_rows_formatted, " rows and ", n_cols_formatted, " variables:"
    ),
    "#' \\describe{"
  )

  for (var_name in names(the_dataset)) {
    col <- the_dataset[[var_name]]
    safe_var_name <- .sanitize_rd_text(var_name)
    out <- c(out, paste0("#'   \\item{", safe_var_name, "}{"))

    raw_lbl <- tryCatch(var_label(col), error = function(e) NULL)
    if (is.list(raw_lbl)) raw_lbl <- unlist(raw_lbl, use.names = FALSE)
    has_lbl <- !is.null(raw_lbl) &&
      length(raw_lbl) > 0L &&
      any(nzchar(trimws(as.character(raw_lbl))))

    desc_line <- if (has_lbl) {
      clean_lbl <- paste(as.character(raw_lbl), collapse = " ") |>
        .remove_braces() |>
        .brackets_to_backticks() |>
        .sanitize_md_cell()
      paste0("#' | *Description:* | ", clean_lbl, " |")
    } else {
      paste0(
        "#' | *Description:* | Description for ",
        .sanitize_md_cell(var_name),
        " goes here |"
      )
    }

    col_classes <- setdiff(
      class(col),
      c("labelled", "haven_labelled", "vctrs_vctr")
    )
    var_type <- if (length(col_classes) > 0L && !is.na(col_classes[1L])) {
      col_classes[1L]
    } else {
      typeof(col)
    }
    if (identical(var_type, "double")) var_type <- "numeric"

    if (var_type == "integer") {
      out <- c(
        out,
        "#'",
        "#' | *Type:*        | integer       |",
        "#' | -------------- | ------------- |",
        "#' |                |               |",
        desc_line,
        "#'"
      )
    } else if (var_type %in% c("factor", "ordered") || is.factor(col)) {
      levs <- levels(col)
      n_lev <- length(levs)
      if (n_lev > 100L) {
        warning(paste0(
          "Variable '", var_name,
          "' has more than 100 levels. Your dataset documentation may be ",
          "huge! Did you mean to do this?"
        ))
      }

      first_level <- if (n_lev > 0L) .sanitize_md_cell(levs[1L]) else "none"
      all_levels <- if (n_lev > 0L) {
        .sanitize_md_cell(paste(levs, collapse = ", "))
      } else {
        "none"
      }
      fac_label <- if ("ordered" %in% class(col) || is.ordered(col)) {
        paste0("Ordered factor (First/Reference level = `", first_level, "`)")
      } else {
        paste0("factor (First/Reference level = `", first_level, "`)")
      }

      out <- c(
        out,
        "#'",
        paste0("#' | *Type:*        | ", fac_label, " |"),
        paste0(
          "#' | -------------- | ",
          "---------------------------------------------------- |"
        ),
        paste0(
          "#' |                | ",
          "                                                     |"
        ),
        desc_line,
        paste0(
          "#' |                | ",
          "                                                     |"
        ),
        paste0("#' | *Levels:*      | `", all_levels, "` |"),
        "#'"
      )
    } else {
      out <- c(
        out,
        "#'",
        paste0("#' | *Type:*        | ", .sanitize_md_cell(var_type), " |"),
        "#' | -------------- | ------------- |",
        "#' |                |               |",
        desc_line,
        "#'"
      )
    }

    out <- c(out, "#'   }")
  }

  out <- c(
    out,
    "#' }",
    "#' @source Where the data came from",
    paste0('"', the_dataset_name, '"')
  )

  writeLines(out, con = r_file_path)
  message("Documentation file created at ", r_file_path)
  invisible(NULL)
}


#------------------------------------------------------------------------------
# Helper functions for write_man()
#------------------------------------------------------------------------------

#' Strip matched `{...}` blocks (e.g. `{other}`)
#' @noRd
.remove_braces <- function(text) {
  gsub("\\{[^\\}]*\\}", "", text)
}

#' Convert `[...]` to inline code backticks
#' @noRd
.brackets_to_backticks <- function(text) {
  gsub("\\[(.*?)\\]", "`\\1`", text)
}

#' Sanitize text for roxygen2 Markdown table cells and Rd safety
#' @noRd
.sanitize_md_cell <- function(text) {
  out <- str_squish(text)
  out <- gsub("\\{", "\\\\{", out)
  out <- gsub("\\}", "\\\\}", out)
  out <- gsub("\\|", "\\\\|", out)
  out
}

#' Escape braces in Rd item names
#' @noRd
.sanitize_rd_text <- function(text) {
  out <- str_squish(text)
  out <- gsub("\\{", "\\\\{", out)
  out <- gsub("\\}", "\\\\}", out)
  out
}
