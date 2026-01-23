# Make an Analysis Project

This function makes an R project that includes an analysis.Rmd or
analysis.qmd file using the conflicted and tidyverse packages. This
project automatically includes an aggressive .gitignore which is
designed to help protect against leaking data (with protected health
information), a starter bibliography file called "references" (in
standard .bib format), and a stock Citation Style Language (.csl) file
for the New England Journal of Medicine.

## Usage

``` r
make_project(
  path,
  type = c("Quarto (analysis.qmd)", "R Markdown (analysis.Rmd)"),
  example = FALSE,
  vignette = FALSE,
  overwrite = FALSE,
  openInteractive = TRUE
)
```

## Arguments

- path:

  Path automatically set by research_project.dcf (see
  `./rstudio/templates/project/`)

- type:

  Choose between "Quarto (analysis.qmd)" or "R Markdown (analysis.Rmd)"

- example:

  Will the analysis file include an example table/figure?

- vignette:

  Will the analysis file be saved as a package vignette?

- overwrite:

  Will an existing RStudio project be overwritten? This is needed for
  for Posit.Cloud. You will be prompted to confirm this option.

- openInteractive:

  Should this new project be opened in a new RStudio window? Defaults to
  `TRUE`. NOTE: this option exists to prevent RStudio from opening two
  duplicate versions of the new project when this function is executed
  from RStudio menus. MODIFY WITH CAUTION.

## Value

Returns nothing. See description above.

## Details

Behind the scenes, this function used by research_project.dcf when a
user selects New project... \> New Directory \> rUM Research Project
Template within the RStudio IDE. See `./rstudio/templates/project/`.

## Examples

``` r
if (FALSE) { # \dontrun{
  # This makes a project with an example Quarto paper in the project's folder.
  make_project(path = "~/test", type = "Quarto (analysis.qmd)", 
              example = TRUE, vignette = TRUE)
  
  # make_project() allows abbreviations on the project type: "Q" for Quarto or "R" for R Markdown
  make_project(path = "~/test_project", "Q", TRUE, TRUE)
  
  # This makes a project with an example R Markdown paper in the project's folder.
  make_project(path = "~/test", type = "R Markdown (analysis.Rmd)", 
              example = TRUE, vignette = TRUE)
              
  # This makes a project with an example paper in the project's folder.
  make_project(path = "~/test_project", "R", example = TRUE)
} # }
```
