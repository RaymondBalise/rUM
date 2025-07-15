#' Write a manual page for package dataset documentation
#' 
#' @description
#' This function produces a \code{roxygen2} R manual (man) page for a dataset that will 
#' be included in an R package. To be documented, the dataset needs to be in the global 
#' environment. The new documentation template will be named to match the datasets and
#' will be saved in the R folder (i.e., using an \code{analysis} dataset will produce the 
#' \code{R/analysis.Rd} file). The page will indicate if the dataset is a data frame
#' or tibble along with the number of rows and columns. For each variable documentation
#' will indicate the variables a type, factor level information (if appropriate), and a 
#' generic description section. If the variable is labelled (using the labelled package 
#' or packages which use labelled, like \code{tidyREDCap}) the variable label will be 
#' used as the default description.
#' 
#' @param the_dataset Dataset object (unquoted) or dataset as character (quoted)
#' 
#' @note You will need to import the `roxygen2` package and add \code{Roxygen: list(markdown = TRUE)}
#' to your DESCRIPTION file. If you made a project using rUM this happens automatically.
#' 
#' 
#' @return A \code{.Rd} file in the \code{man} package directory corresponding to the 
#' name of the supplied dataset.
#' 
#' @export
#' 
#' @examples
#' if (interactive()) {
#'   # Dataset object from Global Environment (unquoted)
#'   write_man(mtcars)
#'   
#'   # Dataset object from Global Environment as character string (quoted)
#'   write_man("mtcars")
#' }
write_man <- function(the_dataset) {

  # Handle both quoted and unquoted dataset names
  if (is.character(the_dataset) && length(the_dataset) == 1) {
    # Input is a string:
    # This allows write_man("mtcars")
    the_dataset_name <- the_dataset
    
    # Check if the dataset exists
    if (!exists(the_dataset_name, envir = .GlobalEnv)) {
      stop("Dataset '", the_dataset_name, "' not found in the global environment")
    }
    
    # Get the actual dataset object
    the_dataset <- get(the_dataset_name, envir = .GlobalEnv)
  } else {
    # Input is an unquoted object:
    # This allows write_man(mtcars)
    the_dataset_name <- deparse(substitute(the_dataset))
    
    # Check if the unquoted dataset exists
    if (!exists(the_dataset_name, envir = .GlobalEnv)) {
      stop("Dataset '", the_dataset_name, "' not found in the global environment")
    }
  }

  # rUM needs labelled dependencies for labelled::var_label
  # Check if the dataset exists in the global environment: use the quoted name
  if (!exists(the_dataset_name, envir = .GlobalEnv)) {
    stop("Dataset '", the_dataset_name, "' not found in the global environment")
  }
  
  # EDIT: We already have the dataset object in the_dataset parameter, so this section
  # is being removed.
  # dataset_obj <- get(the_dataset, envir = .GlobalEnv)
  
  # Check if the object is a data.frame or tibble
  if (!inherits(the_dataset, "data.frame")) {
    stop("Object '", the_dataset_name, "' is not a data.frame or tibble")
  }
  
  # Check if R folder exists
  if (!dir.exists("R")) {
    stop("The R folder does not exist in the current directory")
  }
  
  # Construct the potential file path for the R documentation file
  # Use the string name of the dataset for the file name
  r_file_path <- file.path("R", paste0(the_dataset_name, ".R"))
  
  # Check if the file already exists
  if (file.exists(r_file_path)) {
    stop("Documentation file '", r_file_path, "' already exists")
  }
  
  # Determine if it's specifically a tibble
  is_tibble <- inherits(the_dataset, "tbl_df")
  
  n_rows <- nrow(the_dataset)
  n_cols <- ncol(the_dataset)
  
  n_rows_formatted <-
    format(n_rows, big.mark = ",", scientific = FALSE, trim = TRUE)
  n_cols_formatted <-
    format(n_cols, big.mark = ",", scientific = FALSE, trim = TRUE)
  
  # Choose the appropriate data structure label
  data_structure <- if (is_tibble) "tibble" else "data frame"
  
  # Create the file and open a connection to it
  file_conn <- file(r_file_path, "w")
  
  # Write the documentation content to the file using the string value (the_dataset_name)
  cat(paste0("#' ", the_dataset_name, " dataset\n"), file = file_conn)
  cat("#'\n", file = file_conn)
  cat(paste0("#' @description Description of the ", the_dataset_name, " dataset goes here\n"), file = file_conn)
  cat("#'\n", file = file_conn)
  cat(paste0("#' @format A ", data_structure, " with ", n_rows_formatted, " rows and ", n_cols_formatted, " variables:\n"), file = file_conn)
  cat("#' \\describe{\n", file = file_conn)
  
  # Loop through each variable in the dataset
  for (var_name in names(the_dataset)) {
    
    cat(paste0("#'   \\item{", var_name, "}{\n"), file = file_conn)
    
    description <- if(!is.null(labelled::var_label(the_dataset[[var_name]]))){
      the_label <-
        labelled::var_label(the_dataset[[var_name]]) |>
        .remove_braces() |>
        .replace_brackets_with_backticks()
      
      paste0(
        "#' | *Description:* | ",
        the_label,
        " |\n"
      )
    } else {
      paste0(
        "#' | *Description:* | Description for ",
        var_name,
        " goes here              |\n"
      )
    }
    
    # see if a variable has a label
    var_type <- 
      # check to see if the first class is labeled (from from labelVector)
      if(class(the_dataset[[var_name]])[1] == "labelled"){
        # Get primary class which will be the second class listed after 
        #   "labelled" if data was labeled using labelVector instead of labelled
        class(the_dataset[[var_name]])[2]
      } else {
        class(the_dataset[[var_name]])[1]
      }
    
    
    # Create markdown table based on variable type
    if (var_type == "integer") {
      cat("#'\n", file = file_conn)
      cat("#' | *Type:*        | integer       |\n", file = file_conn)
      cat("#' | -------------- | ------------- |\n", file = file_conn)
      cat("#' |                |               |\n", file = file_conn)
      cat(description, file = file_conn)
      cat("#'\n", file = file_conn)
    } else if (var_type == "factor") {
      # For factor variables, include levels information
      cat("#'\n", file = file_conn)
      first_level <- levels(the_dataset[[var_name]])[1]
      all_levels <- paste(levels(the_dataset[[var_name]]), collapse = ", ")
      
      cat(paste0("#' | *Type:*        | factor (First/Reference level = `", first_level, "`) |\n"), file = file_conn)
      cat("#' | -------------- | ---------------------------------------------------- |\n", file = file_conn)
      cat("#' |                |                                                      |\n", file = file_conn)
      cat(description, file = file_conn)
      cat("#' |                |                                                      |\n", file = file_conn)
      cat(paste0("#' | *Levels:*      | `", all_levels, "`           |\n"), file = file_conn)
      cat("#'\n", file = file_conn)
    } else {
      # Generic description for other types
      cat("#'\n", file = file_conn)
      cat(paste0("#' | *Type:*        | ", var_type, "       |\n"), file = file_conn)
      cat("#' | -------------- | ------------- |\n", file = file_conn)
      cat("#' |                |               |\n", file = file_conn)
      cat(description, file = file_conn)
      cat("#'\n", file = file_conn)
    }
    
    cat("#'   }\n", file = file_conn)
  }
  
  cat("#' }\n", file = file_conn)
  cat("#' @source Where the data came from\n", file = file_conn)
  cat(paste0("\"", the_dataset_name, "\""), file = file_conn)
  
  # Close the connection
  close(file_conn)
  
  message("Documentation file created at ", r_file_path)
}
