# 9. Find and Show Slides in a Package

## Introduction

With
[`write_slides()`](https://raymondbalise.github.io/rUM/reference/write_slides.md)
you can save a deck into a package. To learn about that function look at
the [Creating Slides from a Template with
write_slides()](https://raymondbalise.github.io/rUM/articles/ah_write_slides.md)
vignette. How can you check to see if a package contains slides? If
there is one or more deck, how can you see the slides?

## Getting Started

To see if a package has slides use
[`rUM::find_slides()`](https://raymondbalise.github.io/rUM/reference/find_slides.md).
Note that our functions
[`find_slides()`](https://raymondbalise.github.io/rUM/reference/find_slides.md)
and
[`show_slides()`](https://raymondbalise.github.io/rUM/reference/show_slides.md)
are designed to work with slides made with
[`rUM::write_slides()`](https://raymondbalise.github.io/rUM/reference/write_slides.md).
These functions should work with other Quarto slide decks built using
reveal.js. [Open an issue](https://github.com/RaymondBalise/rUM/issues)
if you run into problems. If you want to know if the `rUM` package has
slides, you would check with:

`find_slides("rUM")`

It will give you an object containing the names of the slide decks.

``` r
> rUM::find_slides("rUM")
Available reveal.js slides in package "rUM":
  - rUM_the_package
  - rUM_the_word
```

To see the slide deck use
[`rUM::show_slides()`](https://raymondbalise.github.io/rUM/reference/show_slides.md).
For example, to see the slide deck called **rUM_the_package** from
inside the `rUM` package you would type:

`rUM::show_slides(package = "rUM", deck = "rUM_the_package")` or  
`rUM::show_slides("rUM", "rUM_the_package")`

You can also pipe from
[`rUM::find_slides()`](https://raymondbalise.github.io/rUM/reference/find_slides.md)
to
[`rUM::show_slides()`](https://raymondbalise.github.io/rUM/reference/show_slides.md)
like this and it will ask you to choose which slide deck:

``` r
> find_slides('rUM') |> show_slides()
Select a slide deck from "rUM" package: 

1: rUM_the_package
2: rUM_the_word

Selection: 1
```
