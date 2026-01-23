# Changelog

## rUM 2.3.0 (rUM Swizzle)

### Enhancements

- The `template` argument for
  [`write_slides()`](https://raymondbalise.github.io/rUM/reference/write_slides.md)
  now uses `"rmed"` instead of `"rmed2025"`. The old value still works
  but is no longer documented. Year references removed from RMed
  templates and files.

### Minor improvements

- Removed references to 2025 from RMed templates & files.

### Fixes

- Breaking changes to {purrr} in version 1.2.0 required internal changes
  to
  [`write_slides()`](https://raymondbalise.github.io/rUM/reference/write_slides.md).
  Uses [`walk()`](https://purrr.tidyverse.org/reference/map.html)
  instead of
  [`map_chr()`](https://purrr.tidyverse.org/reference/map.html) to
  download `"rmed"` slide art & files. See
  <https://purrr.tidyverse.org/news/index.html#breaking-changes-1-2-0>
  for more information.

## rUM 2.2.1

- Fix bug in write_man() that showed ordered factors as factors

## rUM 2.2.0 (rUM Runner)

CRAN release: 2025-07-23

CRAN release: 2025-07

### New Features

- Automatic dataset documentation with
  [`write_man()`](https://raymondbalise.github.io/rUM/reference/write_man.md).
- Slide templates: Quarto reveal.js templates with
  [`write_slides()`](https://raymondbalise.github.io/rUM/reference/write_slides.md)
- Functions for embedding and finding slides in a package with
  [`find_slides()`](https://raymondbalise.github.io/rUM/reference/find_slides.md)
  and
  [`show_slides()`](https://raymondbalise.github.io/rUM/reference/show_slides.md)

### Enhnancements

- [`write_quarto()`](https://raymondbalise.github.io/rUM/reference/write_quarto.md)
  now accepts an `example` argument.

## rUM 2.1.0 (rUM and Coke)

CRAN release: 2025-03-21

CRAN release: 2025-03-21

### New Features

- Automated `make_project(vignette = TRUE)` development tools during
  project build to:
  - Restructure `analysis.qmd` & `analysis.Rmd` YAML for vignette engine
    & building
  - Write user’s `rUM` project packages to `DESCRIPTION` file
  - Adding `VignetteBuilder` (quarto or knitr) to project `DESCRIPTION`
    file
  - Adding items to `.Rbuildignore` & `vignettes/.gitignore`
- Added
  [`rUM::write_quarto()`](https://raymondbalise.github.io/rUM/reference/write_quarto.md)
  function that will add a quarto html document with many useful
  options.
- Provides `README.md` and `dated_progress_notes.md` templates to new
  projects. It will ask if you’d like to overwrite an existing README if
  one exists in the current project.
- Added custom.scss to non-package Quarto projects with
  [`rUM::write_scss()`](https://raymondbalise.github.io/rUM/reference/write_scss.md)
  during project creation.
- Added
  [`rUM::write_scss()`](https://raymondbalise.github.io/rUM/reference/write_scss.md)
  function that will write a SCSS template for Quarto projects. This
  function is available for use outside of `rUM` projects when a SCSS
  template is desired (perhaps, a Quarto blog or Shiny app).
- Added
  [`rUM::write_notes()`](https://raymondbalise.github.io/rUM/reference/write_notes.md)
  function to add the progress notes template to users’ projects when
  one doesn’t currently exist. This can be used in rUM & non-rUM
  projects alike.
- Added
  [`rUM::write_readme()`](https://raymondbalise.github.io/rUM/reference/write_readme.md)
  function to insert a structured README file to users’ projects when
  one doesn’t currently exist. This may also be used in rUM & non-rUM
  projects.
- Added a vignette for using Posit.Cloud

### Other Updates

- removed `RUN_ME_FIRST.R` file and instead automate those processes as
  described above
- Empty
  [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
  object included in manuscript & vignette templates
- Moved external GitHub gists to `inst/gists` directory

### Bug Fixes

- (none)

## rUM 2.0.0 (Overproof Rum)

CRAN release: 2024-06-02

- Added an `example` argument to
  [`make_project()`](https://raymondbalise.github.io/rUM/reference/make_project.md)
- Added a `vignette` argument to
  [`make_project()`](https://raymondbalise.github.io/rUM/reference/make_project.md)
- Added knitr chunk options for graphics
- Fixed a bug that caused duplicate RStudio windows to open if run from
  menus

### 1.1.0.9008

- fix carriage return for building vignettes in Windows

### 1.1.0.9007

- prevent an empty folder from being created if a package name is
  invalid
- add header to manual_change\_\*\_vignette.R

### 1.1.0.9006

- add dependencies_fix function for passing Windows CRAN checks.

### 1.1.0.9005

- set package dependencies for vignette to suggest

### 1.1.0.9004

- add ?rUM page fix quarto check message to be clearer

### 1.1.0.9003

- Add an `overwrite` option to `make_project`

### 1.1.0.9002

- change run_me.R to RUN_ME_FIRST.R

## rUM 1.1

- Harmonize templates
- Include example paper option

## rUM 1.0.2

CRAN release: 2023-02-18

- Linted all code
  - Cleaned up `options(dplyr.summarise.inform=F)` everywhere.
- Added gists to a inst/gists directory

## rUM 1.0.1

CRAN release: 2022-08-16

- Added [https:\\](https:%5C%5C) to github link in vignettes for CRAN
- Removed `![rUM](package-logo.jpg){width=0.8in}` from vignette titles
- Added “with R Markdown” to vignette title

## rUM 1.0

- Added quarto options in project creation template/wizard
- Added quarto support for make_project
- Added rStudio project files
- Added check to make sure analysis does not exist - Thanks to Francisco
  Cardozo
- Updated .gitignore
- Added citation info to README
- Added vignettes
- Add more options to setup chunks
- Removed `suppressMessages(conflict_prefer("spec", "yardstick"))` from
  R Markdown templates. It is now in `tidymodels_prefer()`.
- Add quarto to gitignore gist
- Add [@importFrom](https://github.com/importFrom) bookdown, rmarkdown,
  table1 cran checks

## rUM 0.3.2

- Fixed package version for table1 to be `packageVersion("table1")`
  instead of `packageVersion("tidyverse")`

## rUM 0.3.1

CRAN release: 2021-11-22

- Fixed capitalization bug in
  [`make_project()`](https://raymondbalise.github.io/rUM/reference/make_project.md).
- `packageVersion("rum")` is now `packageVersion("rUM")`

## rUM 0.3.0

CRAN release: 2021-07-26

- Added automatic second bibliography file that writes all packages used
  in a project
- Added rUM to methods section of the research template
- Added first draft of a vignette

##### Minor Changes

- Updated documentation
- Added dependency on the `table1::` package
- To match dependency on `table1::`, we now depend on R (\>= 3.5)

## rUM 0.2.1

- Fixes in DESCRIPTION file

## rUM 0.2

- Added methods section and bibliography

## rUM 0.1

- Added tidymodels with conflicted to setup

## rUM 0.0.5

- Added tidyverse and conflicted to setup
- Added a `NEWS.md` file to track changes to the package
- Added Website
