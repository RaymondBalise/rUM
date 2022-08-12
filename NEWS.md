# rUM 1.0

* Added quarto options in project creation template/wizard
* Added quarto support for make_project
* Added rStudio project files
* Added check to make sure analysis does not exist - Thanks to Francisco Cardozo
* Updated .gitignore
* Added citation info to README
* Added vignettes
* Add more options to setup chunks
* Removed `suppressMessages(conflict_prefer("spec", "yardstick"))` from R Markdown templates.  It is now in `tidymodels_prefer()`.
* Add quarto to gitignore gist 
* Add @importFrom bookdown, rmarkdown, table1 cran checks


# rUM 0.3.2

* Fixed package version for table1 to be `packageVersion("table1")` instead of `packageVersion("tidyverse")`

# rUM 0.3.1

* Fixed capitalization bug in `make_project()`.
* `packageVersion("rum")` is now `packageVersion("rUM")`

# rUM 0.3.0

* Added automatic second bibliography file that writes all packages used in a project
* Added rUM to methods section of the research template
* Added first draft of a vignette

#### Minor Changes

* Updated documentation
* Added dependency on the `table1::` package
* To match dependency on `table1::`, we now depend on R (>= 3.5)


# rUM 0.2.1

* Fixes in DESCRIPTION file

# rUM 0.2

* Added methods section and bibliography

# rUM 0.1

* Added tidymodels with conflicted to setup

# rUM 0.0.5

* Added tidyverse and conflicted to setup
* Added a `NEWS.md` file to track changes to the package
* Added Website



