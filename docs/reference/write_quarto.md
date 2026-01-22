# Create a New Quarto Document

This function creates a new Quarto document (.qmd file) complete with a
useful header.

## Usage

``` r
write_quarto(filename = NULL, path = here(), example = NULL)
```

## Arguments

- filename:

  Character string. The name of the file without the '.qmd' extension.
  Only letters, numbers, hyphens, and underscores are allowed.

- path:

  Character string. Directory where the file will be created. Defaults
  to the current project's base directory.

- example:

  Logical. Will the analysis file include a paper example with table/
  figure? Default is `NULL` and will use a default, non-paper template.
  If this is set to `TRUE`, then it will use the Quarto paper template
  with examples of a table and figure with cross-referencing. If this is
  `FALSE`, then it will use the Quarto paper template without examples
  for tables.

## Value

Opens file after creating the Quarto document.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a new Quarto document
write_quarto(filename = "data_cleaning", path = tempdir())
} # }
```
