# Preview slide deck from a package

Render a Quarto slide deck from the supplied package in the Viewer or
browser. For more information look in the [Creating Slides with
write_slides()](https://raymondbalise.github.io/rUM/doc/ah_write_slides.md)
vignette.

## Usage

``` r
show_slides(package = NULL, deck = NULL, ...)
```

## Arguments

- package:

  Character. Provide the package containing one or more slide decks.

- deck:

  Character. The name of the slide deck to render without ".qmd".

- ...:

  Optional arguments passed to
  [`quarto::quarto_preview()`](https://quarto-dev.github.io/quarto-r/reference/quarto_preview.html).

## Note

Uses
[`quarto::quarto_preview()`](https://quarto-dev.github.io/quarto-r/reference/quarto_preview.html)
to display the slide deck in the Viewer pane or browser.

## Examples

``` r
if (interactive()) {
  # Preview a known slide deck name from a package:
  show_slides(package = "rUM", deck = "rUM_the_package")

  # Use find_slides to pipe the output:
  find_slides(package = "rUM") |> show_slides()
}
```
