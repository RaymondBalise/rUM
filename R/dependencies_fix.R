dependencies_fix <- function(variables) {
  # The here and roxygen2 packages are needed to build vignettes. To make sure 
  # people can use the vignette = TRUE option in make_project() function the
  # packages are included as imports in the DESCRIPTION file.  This unused 
  # function allows CRAN checks to be passed.
  
  # Do NOT call the useless function.
  return()
  here::here()
  roxygen2::block_get_tag()
}