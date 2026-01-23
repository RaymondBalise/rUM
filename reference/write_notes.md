# Create a project progress note

This function streamlines project documentation by creating and managing
both README.md and dated_progress_notes.md files. It provides
interactive prompts for existing files and maintains consistent project
documentation structure.

## Usage

``` r
write_notes(path = here())
```

## Arguments

- path:

  The destination directory for the progress notes file. Defaults to
  ` `[`here::here()`](https://here.r-lib.org/reference/here.html).

## Value

Creates a chronological project progress notes tracker

## Details

The dated_progress_notes.md file is initialized with the current date
and is designed to help track project milestones chronologically. If the
progress notes file already exists, the function will stop and warn the
user.

## Examples

``` r
# Create new progress note file in temporary directory
tmp <- tempdir()
write_notes(path = tmp)
#> ✔ A dated_progress_notes.md template has been created.
```
