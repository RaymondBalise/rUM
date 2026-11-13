#' Create a project progress note (Deprecated)
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `write_notes()` was deprecated in rUM 2.4.0 because scaffolded projects and
#' packages now include built-in AI assistant instructions (`AGENTS.md`) and the
#' `rum-devlog` skill (`DECISIONS.md` and `CHANGELOG.md` / `NEWS.md`) instead of
#' `dated_progress_notes.md`.
#'
#' @param path The destination directory for the progress notes file. Defaults
#'   to \code{here::here()}.
#'
#' @importFrom here here
#' @importFrom lifecycle deprecate_warn
#' @importFrom usethis ui_done
#'
#' @return Invisibly returns \code{NULL} after creating
#'   \code{dated_progress_notes.md}.
#'
#' @details
#' The \code{dated_progress_notes.md} file is initialized with the current date
#' to track project milestones chronologically. If the progress notes file
#' already exists, the function stops with an error.
#'
#' @export
#' @examples
#' if (interactive()) {
#'   tmp <- tempdir()
#'   write_notes(path = tmp)
#' }
write_notes <- function(path = here()) {
  deprecate_warn(
    when = "2.4.0",
    what = "write_notes()",
    details = paste(
      "Projects now scaffold AGENTS.md and the rum-devlog skill",
      "(DECISIONS.md and CHANGELOG.md / NEWS.md) instead."
    )
  )

  .validate_path(path)
  path <- normalizePath(path, mustWork = TRUE)

  progress_notes_content <- paste0(
    "# Project updates\n\n",
    format(Sys.Date(), "%b %d, %Y"),
    ": project started"
  )

  if (file.exists(file.path(path, "dated_progress_notes.md"))) {
    stop("A dated_progress_notes.md has been found in the specified directory!")
  }

  writeLines(
    progress_notes_content,
    con = file.path(path, "dated_progress_notes.md")
  )
  ui_done("A dated_progress_notes.md template has been created.")

  invisible(NULL)
}
