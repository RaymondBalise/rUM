#' Write a manual page for package dataset documentation
#' 
#' @description
#' This function produces a {roxygen2} R manual (man) page for a dataset that will 
#' be included in an R package. To be documented, the dataset needs to be in the global 
#' environment. The new documentation template will be named to match the datasets and
#' will be saved in the R folder (i.e., using an \code{analysis} dataset will produce the 
#' \code{R/analysis.Rd} file). The page will indicate if the dataset is a data frame
#' or tibble along with the number of rows and columns. For each variable documentation
#' will indicate the variables a type, factor level information (if appropriate), and a 
#' generic description section. If the variable is labelled (using the labelled package 
#' or packages which use labelled, like {tidyREDCap}) the variable label will be 
#' used as the default description.
#' 
#' @param the_dataset Dataset object
#' 
#' @return A \code{.Rd} file in the \code{man} package directory corresponding to the 
#' name of the supplied dataset.
#' 
#' @export
#' 
#' @examples
#' if (interactive()) write_man(mtcars)
write_man <- function(the_dataset) {
  # rUM needs labelled dependencies for labelled::var_label
  # Check if the dataset exists in the global environment
  if (!exists(the_dataset, envir = .GlobalEnv)) {
    stop("Dataset '", the_dataset, "' not found in the global environment")
  }
  
  # Get the dataset object
  dataset_obj <- get(the_dataset, envir = .GlobalEnv)
  
  # Check if the object is a data.frame or tibble
  if (!inherits(dataset_obj, "data.frame")) {
    stop("Object '", the_dataset, "' is not a data.frame or tibble")
  }
  
  # Check if R folder exists
  if (!dir.exists("R")) {
    stop("The R folder does not exist in the current directory")
  }
  
  # Construct the potential file path for the R documentation file
  r_file_path <- file.path("R", paste0(the_dataset, ".R"))
  
  # Check if the file already exists
  if (file.exists(r_file_path)) {
    stop("Documentation file '", r_file_path, "' already exists")
  }
  
  # Determine if it's specifically a tibble
  is_tibble <- inherits(dataset_obj, "tbl_df")
  
  n_rows <- nrow(dataset_obj)
  n_cols <- ncol(dataset_obj)
  
  n_rows_formatted <-
    format(n_rows, big.mark = ",", scientific = FALSE, trim = TRUE)
  n_cols_formatted <-
    format(n_cols, big.mark = ",", scientific = FALSE, trim = TRUE)
  
  # Choose the appropriate data structure label
  data_structure <- if (is_tibble) "tibble" else "data frame"
  
  # Create the file and open a connection to it
  file_conn <- file(r_file_path, "w")
  
  # Write the documentation content to the file using the_dataset value
  cat(paste0("#' ", the_dataset, " dataset\n"), file = file_conn)
  cat("#'\n", file = file_conn)
  cat(paste0("#' @description Description of the ", the_dataset, " dataset goes here\n"), file = file_conn)
  cat("#'\n", file = file_conn)
  cat(paste0("#' @format A ", data_structure, " with ", n_rows_formatted, " rows and ", n_cols_formatted, " variables:\n"), file = file_conn)
  cat("#' \\describe{\n", file = file_conn)
  
  # Loop through each variable in the dataset
  for (var_name in names(dataset_obj)) {
    
    cat(paste0("#'   \\item{", var_name, "}{\n"), file = file_conn)
    
    description <- if(!is.null(labelled::var_label(dataset_obj[[var_name]]))){
      the_label <-
        labelled::var_label(dataset_obj[[var_name]]) |>
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
        " goes here |\n"
      )
    }
    
    # see if a variable has a label
    var_type <- if(!is.null(labelled::var_label(dataset_obj[[var_name]]))){
      # Get primary class which will be after labelled
      class(dataset_obj[[var_name]])[2]
    } else {
      # Get primary class
      class(dataset_obj[[var_name]])[1]
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
      first_level <- levels(dataset_obj[[var_name]])[1]
      all_levels <- paste(levels(dataset_obj[[var_name]]), collapse = ", ")
      
      cat(paste0("#' | *Type:* | factor (First/Reference level = `", first_level, "`) |\n"), file = file_conn)
      cat("#' | -------------- | ---------------------------------------------------- |\n", file = file_conn)
      cat("#' |                |                                                      |\n", file = file_conn)
      cat(description, file = file_conn)
      cat("#' |                |                                                      |\n", file = file_conn)
      cat(paste0("#' | *Levels:*      | `", all_levels, "`           |\n"), file = file_conn)
      cat("#'\n", file = file_conn)
    } else {
      # Generic description for other types
      cat("#'\n", file = file_conn)
      cat(paste0("#' | *Type:* | ", var_type, " |\n"), file = file_conn)
      cat("#' | ------- | ------------- |\n", file = file_conn)
      cat("#' |         |               |\n", file = file_conn)
      cat(description, file = file_conn)
      cat("#'\n", file = file_conn)
    }
    
    cat("#'   }\n", file = file_conn)
  }
  
  cat("#' }\n", file = file_conn)
  cat("#' @source Where the data came from\n", file = file_conn)
  cat(paste0("\"", the_dataset, "\""), file = file_conn)
  
  # Close the connection
  close(file_conn)
  
  message("Documentation file created at ", r_file_path)
}
