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
#' @importFrom glue glue
#' @importFrom here here
#' @importFrom readr read_file write_file
#' @importFrom stringr str_detect str_replace
#' @importFrom usethis ui_done ui_info ui_yeah
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

write_scss <- function(name = "custom", path = here(), add_to_yaml = FALSE) {
  
  # Validate path
  .validate_path(path)
  
  # Normalize the path for consistency
  path <- normalizePath(path, mustWork = TRUE)
  
  .validate_filename(name)
  
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


#----------------------------------------------------------------------------------------
# Helper functions for write_scss()
#----------------------------------------------------------------------------------------
#' Generate the SCSS template
#' 
#' @description
#' Creating the SCSS template here. Another future consideration is a package or
#' GitHub downloaded gist.
#' 
#' @return Message object used to write into the SCSS file.
#' 
#' @noRd
.generate_scss_template <- function() {
  glue(
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


#----------------------------------------------------------------------------------------
#' Update the Quarto document YAML
#' 
#' @description
#' Update the Quarto document YAML with the newly-created SCSS template. Using the
#' `glue` package, this will add the new SCSS file under the `custom.scss` file in
#' the Quarto document YAML. If the YAML cannot be updated via this method, a message
#' is returned to the user & the main `write_scss` function continues.
#' 
#' @return Content object
#' 
#' @noRd
.update_yaml <- function(name) {
  if (!file.exists("analysis.qmd")) {
    ui_info("No analysis.qmd file found in the current directory.")
    return(invisible(NULL))
  }
  
  qmd_content <- read_file("analysis.qmd")
  original_yaml <- glue(
"
format:
  html:
    embed-resources: true   # true = a single file, false = multiple files
    theme:
      - default
      - custom.scss"
  )

  new_yaml <- glue(
"
format:
  html:
    embed-resources: true   # true = a single file, false = multiple files
    theme:
      - default
      - custom.scss
      - {name}.scss"
  )
  
  # Attempt to update the YAML in the qmd file or show user how to do it manually
  if (str_detect(qmd_content, original_yaml)) {
    updated_content <- str_replace(qmd_content, original_yaml, new_yaml)
    write_file(updated_content, file = "analysis.qmd")
    ui_done("The YAML in analysis.qmd has been updated.")
  } else {
    ui_info(glue(
      "\nBe sure to update your listed SCSS files in the YAML manually. Example:\n",
      new_yaml
    ))
  }
}


#----------------------------------------------------------------------------------------
#' Display HTML links for customizing with SCSS
#' 
#' @description
#' SCSS reference links -- how to use, customize, and apply styling with Quarto & SCSS.
#' 
#' @return Console output.
#' 
#' @noRd
.display_reference_links <- function() {
  links <- glue(
    "For more SCSS styling options, visit:\n",
    "- https://quarto.org/docs/output-formats/html-themes.html#customizing-themes\n",
    "- https://quarto.org/docs/output-formats/html-themes-more.html\n",
    "- https://github.com/twbs/bootstrap/blob/main/scss/_variables.scss\n"
  )
  message(links)
}