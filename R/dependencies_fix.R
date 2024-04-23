#' @title Dummy function to require packages
#'
#' @description Does nothing, on purpose.
#' 
#' @details
#' The \code{here} and \code{roxygen2} packages are needed to build vignettes.
#'   To make sure people can use the \code{vignette = TRUE} option in the
#'   \code{make_project()} function, these packages are included as imports in
#'   the DESCRIPTION file. This unused function allows CRAN checks to be passed.
#' 
#' @keywords internal
#' @noRd

dependencies_fix <- function() {
  # Do NOT call the useless function.
  return()
  here::here()
  roxygen2::block_get_tag()
}
