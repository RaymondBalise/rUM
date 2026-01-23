# Create a project README file

This function streamlines project documentation by creating and managing
a README.md file. It provides interactive prompts for existing files and
maintains consistent project documentation structure.

## Usage

``` r
write_readme(path = here())
```

## Arguments

- path:

  The destination directory for the README file. Defaults to
  ` `[`here::here()`](https://here.r-lib.org/reference/here.html).

## Value

Creates a comprehensive README template for project documentation.

## Details

The README.md template includes structured sections for:

- Project description (study name, principal investigator, author)

- Project setup steps for reproducibility

- File and directory descriptions

- Miscellaneous project notes

If the README file already exists, the function will stop and warn the
user. The templates include example documentation that can be modified
to suit project needs.

## Examples

``` r
# Create new README in temporary directory
tmp <- tempdir()
write_readme(path = tmp)
#> ✔ A README.md template has been created.
```
