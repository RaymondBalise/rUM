#' Create a Quarto SCSS file
#'
#' This function creates the \code{.scss} file so that any Quarto project can be easily
#' customized with SCSS styling variables, mixins, and rules. When creating additional
#' SCSS files beyond the default \code{custom.scss}, the function will attempt to
#' update the YAML of your Quarto document while preserving any existing SCSS
#' configurations.
#'
#' @details
#' The function includes a robust YAML handling mechanism that:
#' \itemize{
#'   \item Preserves existing YAML structure and indentation
#'   \item Safely adds new SCSS files without disrupting existing ones
#'   \item Provides manual instructions if the YAML structure differs from expected
#' }
#'
#' For more information on customizing Quarto documents with SCSS, please refer to
#' \url{https://quarto.org/docs/output-formats/html-themes.html#customizing-themes},
#' \url{https://quarto.org/docs/output-formats/html-themes-more.html}, and
#' \url{https://github.com/twbs/bootstrap/blob/main/scss/_variables.scss} will provide
#' you with over 1500 lines of SCSS variables.
#'
#' @param name The name of the scss file without extension. Default \code{name} is
#' "custom".
#' @param path The destination directory for the SCSS file. Defaults to \code{
#' here::here()}.
#' @param add_to_yaml Boolean. Add the new SCSS filename to YAML structure.
#' 
#' @return A \code{.scss} file to customize Quarto styling. If \code{name} is not
#' "custom", the function will also attempt to update the Quarto document's YAML to
#' include the new SCSS file while preserving any existing SCSS configurations.
#'
#' @export
#' @examples
#' # Create the default custom.scss in a temporary directory
#' tmp <- tempdir()
#' write_scss(name = "custom", path = tmp)
#'
#' # Add another SCSS file and update YAML in the temporary directory
#' write_scss(name = "special_theme", path = tmp)

write_scss <- function(name = "custom", path = here::here(), add_to_yaml = FALSE) {
  
  # Validate path
  if (is.null(path) || !dir.exists(path)) {
    stop("Invalid `path`. Please enter a valid project directory.")
  }
  
  # Normalize the path for consistency
  path <- normalizePath(path, mustWork = TRUE)
  
  # Define the target file path
  the_scss_file <- file.path(path, paste0(name, ".scss"))

  # Check existing .scss files
  if (file.exists(the_scss_file)) {
    ui_info(paste0('A file named "', name, '.scss" already exists!'))
    if (!ui_yeah("Do you want to overwrite this specific file?")) {
      ui_info("Operation cancelled - no file was created or modified.")
      return(invisible(NULL))
    }
  }

  # Write the SCSS file
  scss_template <- .generate_scss_template()
  writeLines(scss_template, the_scss_file)
  ui_done(paste0("Created ", name, ".scss"))
  
  # Update YAML after successful file creation only if not custom.scss
  if (name != "custom" && add_to_yaml) { .update_yaml(name) }
  
  # Display reference links
  .display_reference_links()

  return(invisible(NULL))

}

# Helper functions
.generate_scss_template <- function() {
  glue::glue(
    '/*-- scss:defaults --*/
    // Colors
    // $primary: #2c365e;
    // $body-bg: #fefefe;
    // $link-color: $primary;
    // Fonts
    // $font-family-sans-serif: "Open Sans", sans-serif;
    // $font-family-monospace: "Source Code Pro", monospace;\n\n
    /*-- scss:mixins --*/\n\n
    /*-- scss:rules --*/
    // Custom theme rules
    // .title-block {{
    //   margin-bottom: 2rem;
    //   border-bottom: 3px solid $primary;
    // }}
    // code {{
    //   color: darken($primary, 10%);
    //   padding: 0.2em 0.4em;
    //   border-radius: 3px;
    // }}'
  )
}

.update_yaml <- function(name) {
  if (!file.exists("analysis.qmd")) {
    ui_info("No analysis.qmd file found in the current directory.")
    return(invisible(NULL))
  }
  
  qmd_content <- readr::read_file("analysis.qmd")
  original_yaml <- glue::glue(
"
format:
  html:
    embed-resources: true
    theme:
      - default
      - custom.scss"
  )

  new_yaml <- glue::glue(
"
format:
  html:
    embed-resources: true
    theme:
      - default
      - custom.scss
      - {name}.scss"
  )
  
  # Attempt to update the YAML or show user how to do it manually
  if (grepl(original_yaml, qmd_content)) {
    updated_content <- stringr::str_replace(qmd_content, original_yaml, new_yaml)
    readr::write_file(updated_content, file = "analysis.qmd")
    ui_done("The YAML in analysis.qmd has been updated.")
  } else {
    ui_info(glue::glue(
      "\nBe sure to update your listed SCSS files in the YAML manually. Example:\n",
      new_yaml
    ))
  }
}

.display_reference_links <- function() {
  links <- glue::glue(
    "For more SCSS styling options, visit:\n",
    "- https://quarto.org/docs/output-formats/html-themes.html#customizing-themes\n",
    "- https://quarto.org/docs/output-formats/html-themes-more.html\n",
    "- https://github.com/twbs/bootstrap/blob/main/scss/_variables.scss\n"
  )
  message(links)
}