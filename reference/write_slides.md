# Create a Quarto slide deck template

Creates a pre-formatted .qmd file for presentation slides using
reveal.js along with necessary supporting files (SCSS styling and
RStudio theme). The generated template includes optimized YAML
configuration and slide structure to quickly start building academic &
professional presentations. For more information look in the [Creating
Slides with
write_slides()](https://raymondbalise.github.io/rUM/doc/ah_write_slides.md)
vignette.

## Usage

``` r
write_slides(
  filenames,
  path = here(),
  new_folder = "slides",
  example = FALSE,
  template = "none",
  format = "revealjs"
)
```

## Arguments

- filenames:

  Character vector with minimal length of 1. This allows for the ability
  to batch create multiple slide decks in one function call.

- path:

  Character string. Directory where the file will be created. Defaults
  to the current project's base directory.

- new_folder:

  Character. Default folder is `"slides"`. Options are:

  - `"none"`: No folder is created. All files (.qmd & other) are created
    in the working directory (path of
    [`here::here()`](https://here.r-lib.org/reference/here.html)).

  - `"all"`: One folder is created for each value in the length of
    `filenames`.

  - `"slide_"`: One folder is created by appending "slides\_" to the
    first file in `filenames` argument. If `filenames = "day_1"`, then
    the folder will be named `"slides_day_1"`.

  - A character value.

- example:

  Logical. Whether to include example slides with demonstrations of
  including content.

- template:

  Character. Whether to include a slide template for common slide
  layouts and formatting (default: "none")

  - optional: `"miami"` for a University of Miami theme.

  - optional: `"rmed2025"` for a R/Med 2025 theme.

- format:

  Character string. Slide format to use. Currently supports 'reveal.js',
  with planned support for PowerPoint and Beamer in future releases.

## Value

Invisibly returns NULL. The created .qmd file is automatically opened in
the RStudio editor upon successful creation.

## Details

The function creates three files:

- A .qmd file with the specified filename containing the slide template

- A slides.scss file for custom styling

- An RStudio theme file for consistent code highlighting

All filenames must contain only letters, numbers, hyphens, and
underscores.

## Note

Be sure to specify `path = "inst"` if you are adding slides to a
package.

## Examples

``` r
if (interactive()) {
  # Create basic slides template in current directory
  write_slides(filenames = "my_presentation")

  # Create slides with example content in a specific directory
  tmp <- tempdir()
  write_slides(filenames = "tutorial_slides", path = tmp, example = TRUE)

  # Create a slidedeck for a package in the inst directory
  tmp <- tempdir()
  write_slides(filenames = "tutorial_slides", path = 'inst', example = TRUE)
}
```
