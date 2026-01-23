# Create a Quarto SCSS file

This function creates the `.scss` file so that any Quarto project can be
easily customized with SCSS styling variables, mixins, and rules. When
creating additional SCSS files beyond the default `custom.scss`, the
function will attempt to update the YAML of your Quarto document while
preserving any existing SCSS configurations.

## Usage

``` r
write_scss(name = "custom", path = here(), add_to_yaml = FALSE)
```

## Arguments

- name:

  The name of the scss file without extension. Default `name` is
  "custom".

- path:

  The destination directory for the SCSS file. Defaults to
  ` `[`here::here()`](https://here.r-lib.org/reference/here.html).

- add_to_yaml:

  Boolean. Add the new SCSS filename to YAML structure.

## Value

A `.scss` file to customize Quarto styling. If `name` is not "custom",
the function will also attempt to update the Quarto document's YAML to
include the new SCSS file while preserving any existing SCSS
configurations.

## Details

The function includes a robust YAML handling mechanism that:

- Preserves existing YAML structure and indentation

- Safely adds new SCSS files without disrupting existing ones

- Provides manual instructions if the YAML structure differs from
  expected

For more information on customizing Quarto documents with SCSS, please
refer to
<https://quarto.org/docs/output-formats/html-themes.html#customizing-themes>,
<https://quarto.org/docs/output-formats/html-themes-more.html>, and
<https://github.com/twbs/bootstrap/blob/main/scss/_variables.scss> will
provide you with over 1500 lines of SCSS variables.

## Examples

``` r
# Create the default custom.scss in a temporary directory
tmp <- tempdir()
write_scss(name = "custom", path = tmp)
#> ✔ Created custom.scss
#> For more SCSS styling options, visit:
#> - https://quarto.org/docs/output-formats/html-themes.html#customizing-themes
#> - https://quarto.org/docs/output-formats/html-themes-more.html
#> - https://github.com/twbs/bootstrap/blob/main/scss/_variables.scss

# Add another SCSS file and update YAML in the temporary directory
write_scss(name = "special_theme", path = tmp)
#> ✔ Created special_theme.scss
#> For more SCSS styling options, visit:
#> - https://quarto.org/docs/output-formats/html-themes.html#customizing-themes
#> - https://quarto.org/docs/output-formats/html-themes-more.html
#> - https://github.com/twbs/bootstrap/blob/main/scss/_variables.scss
```
