#' Create a Quarto SCSS file
#'
#' This function creates a `.scss` file so that any Quarto project can be
#' customized with SCSS styling variables, mixins, and rules. When
#' `add_to_yaml = TRUE`, the function appends the SCSS file to the `theme:`
#' block of `analysis.qmd` while preserving existing theme cascade order.
#'
#' @details
#' For more information on customizing Quarto documents with SCSS, refer to
#' <https://quarto.org/docs/output-formats/html-themes.html#customizing-themes>,
#' <https://quarto.org/docs/output-formats/html-themes-more.html>, and
#' <https://github.com/twbs/bootstrap/blob/v5.3.3/scss/_variables.scss> for
#' over 1500 lines of SCSS variables.
#'
#' @param filename Name of the SCSS file (with or without `.scss` extension).
#'   Defaults to `"custom"`.
#' @param path Destination directory for the SCSS file. Defaults to
#'   `here::here()`.
#' @param add_to_yaml Logical. Append the SCSS filename to the `theme:` list in
#'   `analysis.qmd`? Defaults to `FALSE`.
#' @param name `r lifecycle::badge("deprecated")` Use `filename` instead.
#'
#' @importFrom glue glue
#' @importFrom here here
#' @importFrom lifecycle deprecated deprecate_warn is_present
#' @importFrom rlang is_interactive
#' @importFrom stringr regex str_replace_all
#' @importFrom usethis ui_done ui_info
#' @importFrom utils menu
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   tmp <- tempdir()
#'   write_scss(filename = "custom", path = tmp)
#'   write_scss(filename = "special_theme", path = tmp)
#' }
write_scss <- function(
  filename = "custom",
  path = here(),
  add_to_yaml = FALSE,
  name = deprecated()
) {
  if (is_present(name)) {
    deprecate_warn("2.4.0", "write_scss(name)", "write_scss(filename)")
    filename <- name
  }

  if (!.is_bool(add_to_yaml)) {
    stop("Parameter `add_to_yaml` must be TRUE or FALSE.")
  }

  .validate_path(path)
  path <- normalizePath(path, mustWork = TRUE)

  if (!is.character(filename) || length(filename) != 1L || is.na(filename)) {
    stop("Invalid filename: must be character.")
  }
  filename <- str_replace_all(
    filename,
    regex("\\.scss$", ignore_case = TRUE),
    ""
  )
  .validate_filename(filename)

  the_scss_file <- file.path(path, paste0(filename, ".scss"))

  if (file.exists(the_scss_file)) {
    ui_info(paste0('A file named "', filename, '.scss" already exists!'))
    if (!is_interactive()) {
      stop(paste0(
        'File "', filename, '.scss" already exists and session is not ',
        "interactive. Remove the existing file first or run interactively."
      ))
    }
    choice <- menu(
      choices = c("No", "Yes"),
      title = paste0('Overwrite existing "', filename, '.scss"?')
    )
    if (choice != 2L) {
      ui_info("Operation cancelled; no file was created or modified.")
      return(invisible(NULL))
    }
  }

  scaffold_path <- .get_rum_scaffold()
  scss_src <- file.path(scaffold_path, "styling/custom.scss")
  if (!file.exists(scss_src)) {
    stop("Could not find custom.scss in downloaded templates")
  }
  file.copy(from = scss_src, to = the_scss_file, overwrite = TRUE)
  ui_done(paste0("Created ", filename, ".scss"))

  if (add_to_yaml) {
    .update_yaml(filename = filename, path = path)
  }

  .display_reference_links()

  invisible(NULL)
}


#------------------------------------------------------------------------------
#' Update the Quarto document YAML
#'
#' @description
#' Appends `<filename>.scss` to the end of the `theme:` list in `analysis.qmd`
#' without duplicating existing entries or inverting CSS cascade order.
#'
#' @noRd
.update_yaml <- function(filename, path = here()) {
  analysis_file <- file.path(path, "analysis.qmd")
  scss_entry <- paste0(filename, ".scss")

  if (!file.exists(analysis_file)) {
    ui_info("No analysis.qmd file found in the target directory.")
    return(invisible(NULL))
  }

  lines <- readLines(analysis_file, warn = FALSE)
  delim_idx <- which(trimws(lines) == "---")

  if (length(delim_idx) >= 2L) {
    fm_end <- delim_idx[2L]
    fm_lines <- lines[seq_len(fm_end)]

    # Case 1: Inline `theme: [default, ...]`
    inline_idx <- grep("^\\s*theme:\\s*\\[.*\\]\\s*$", fm_lines)
    if (length(inline_idx) == 1L) {
      idx <- inline_idx[1L]
      indent <- sub("^(\\s*)theme:.*$", "\\1", fm_lines[idx])
      inner <- sub("^\\s*theme:\\s*\\[(.*)\\]\\s*$", "\\1", fm_lines[idx])
      items <- trimws(strsplit(inner, ",", fixed = TRUE)[[1L]])
      items <- items[nzchar(items)]
      if (scss_entry %in% items) {
        ui_info(paste0(scss_entry, " is already listed in analysis.qmd YAML."))
        return(invisible(NULL))
      }
      new_block <- c(
        paste0(indent, "theme:"),
        paste0(indent, "  - ", c(items, scss_entry))
      )
      updated <- append(lines[-idx], new_block, after = idx - 1L)
      writeLines(updated, con = analysis_file)
      ui_done("The YAML in analysis.qmd has been updated.")
      return(invisible(NULL))
    }

    # Case 2: Block list `theme:\n  - default\n  - custom.scss`
    block_idx <- grep("^\\s*theme:\\s*$", fm_lines)
    if (length(block_idx) == 1L) {
      b_idx <- block_idx[1L]
      item_idx <- integer(0)
      k <- b_idx + 1L
      while (k < fm_end && grepl("^\\s*-\\s+\\S+", lines[k])) {
        item_idx <- c(item_idx, k)
        k <- k + 1L
      }
      if (length(item_idx) > 0L) {
        existing_items <- trimws(sub("^\\s*-\\s+", "", lines[item_idx]))
        if (scss_entry %in% existing_items) {
          ui_info(
            paste0(scss_entry, " is already listed in analysis.qmd YAML.")
          )
          return(invisible(NULL))
        }
        last_idx <- item_idx[length(item_idx)]
        item_indent <- sub("^(\\s*)-.*$", "\\1", lines[last_idx])
        new_line <- paste0(item_indent, "- ", scss_entry)
        updated <- append(lines, new_line, after = last_idx)
        writeLines(updated, con = analysis_file)
        ui_done("The YAML in analysis.qmd has been updated.")
        return(invisible(NULL))
      }
    }
  }

  has_custom <- file.exists(file.path(path, "custom.scss")) &&
    filename != "custom"
  example_lines <- c(
    "    theme:",
    "      - default",
    if (has_custom) "      - custom.scss",
    paste0("      - ", scss_entry)
  )
  ui_info(paste0(
    "\nBe sure to update your listed SCSS files in the YAML manually. ",
    "Example:\n",
    paste(example_lines, collapse = "\n")
  ))
}


#------------------------------------------------------------------------------
#' Display HTML links for customizing with SCSS
#'
#' @noRd
.display_reference_links <- function() {
  links <- glue(
    "For more SCSS styling options, visit:\n",
    "- https://quarto.org/docs/output-formats/html-themes.html",
    "#customizing-themes\n",
    "- https://quarto.org/docs/output-formats/html-themes-more.html\n",
    "- https://github.com/twbs/bootstrap/blob/v5.3.3/scss/_variables.scss\n"
  )
  message(links)
}
