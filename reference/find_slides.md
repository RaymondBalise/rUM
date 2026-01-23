# List all slide decks in a package

Returns the names of all Quarto slide decks in a package. This is
designed to work with
[`rUM::show_slides()`](https://raymondbalise.github.io/rUM/reference/show_slides.md)
to preview the slide deck. For more information look in the [Creating
Slides with
write_slides()](https://raymondbalise.github.io/rUM/doc/ah_write_slides.md)
vignette.

## Usage

``` r
find_slides(package = NULL)
```

## Arguments

- package:

  Character. Provide the package containing one or more slide decks.

## Value

A list of class "slide_finder" containing the name of the package and
the name of the slides.

## Examples

``` r
if (interactive()) {
  find_slides("rUM")
}
```
