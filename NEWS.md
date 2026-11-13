# rUM 2.4.0 (rUM Sommelier)

## New Features
+ Templates are now sourced from an external repository ([rum_templates](https://github.com/RaymondBalise/rum_templates)) instead of bundled `inst/gists`. This allows template updates without requiring a package release, and all scaffolded templates now carry `template-version: 1` in YAML frontmatter (#105).
+ Manifest-driven scaffolding: YAML manifests in the external repo define which files, `{{knitr_opts}}` partial injections, and actions each project type requires, replacing hardcoded template logic.
+ Downloaded templates (including R/Medicine slide artwork) are cached for the R session and persisted locally in `tools::R_user_dir("rUM", which = "cache")` for automatic offline fallback, with local override support via `RUM_TEMPLATES_PATH`.
+ Bundled Citation Style Language (`.csl`) files for APA 7th (`apa.csl`, new default), MLA 9th (`mla.csl`), and NEJM (`the-new-england-journal-of-medicine.csl`) ship alongside `references.bib` and `packages.bib` in a dedicated `bib/` subdirectory (`vignettes/bib/` for packages) without live Zotero downloads (#88).
+ Projects and packages now include built-in AI coding assistant instructions (`AGENTS.md`, `CLAUDE.md`, `GEMINI.md`) and `.agents/skills/` (`rum-workflow`, `rum-devlog`, `rum-commit`), documented in a new `ak_ai_agents_and_skills` vignette.
+ `make_package()` now scaffolds `inst/extdata/`, `inst/scripts/libraries.R`, and `inst/scripts/do_everything.R` (#104), conditionally loads `library(arrow)` when installed (#101), and includes a dev-first `read_extdata()` dataset loader loop in vignette templates (#103).

## Enhancements
+ `make_project()` and `make_package()` accept case-insensitive `type` values (`"Quarto"`, `"q"`, `"qmd"`, `"R"`, `"rmd"`, `"R Markdown"`), check external template availability and existing `analysis.*` files before modifying `.Rproj` skeletons, gate `quarto_version()` checks strictly to Quarto vignettes, and configure `.Rbuildignore` for agent, decision log, and `do_everything_summary.RData` files.
+ `make_project()` into an existing folder is now non-destructive by default (`overwrite = FALSE` preserves existing `.Rproj`, `.gitignore`, `README.md`, `custom.scss`, and agent instruction files while adding missing scaffold assets).
+ R Markdown vignette templates now use `bookdown::html_vignette2` (with `bookdown` added to `Suggests`) so `\@ref()` table and figure cross-references resolve cleanly, and Quarto templates open external links in a new tab by default (`link-external-newwindow: true`, #93, #98).
+ Enhanced `.gitignore` templates now ignore `*.arrow` (#102), additional SAS/tabular/database formats (`*.sas7bndx`, `*.xpt`, `*.sd2`, `*.sd7`, `*.ssd*`, `*.tsv`, `*.dat`, `*.db`, `*.sqlite*`, `*.duckdb`, #100), `data-raw/`, `raw-data/`, and Quarto `*_files/` artifact directories while keeping slide `clean_title_page.html` assets trackable.
+ `write_quarto()` resolves templates, `{{knitr_opts}}` partials, and bundled `.csl` files via `quarto_standalone.yml` (#83).
+ `write_slides()` resolves `"none"`, `"miami"`, and `"rmed"` decks case-insensitively via `slides.yml`, accepts `.qmd` extensions cleanly via `.validate_filename()` (#87), auto-creates `inst/slides` without double-nesting, and validates `template` and `format` strictly.
+ `show_slides()` stages installed package slide directories into a writable temporary folder before calling `quarto::quarto_preview()`, supporting read-only system libraries and nested `inst/slides/` subdirectories discovered by `find_slides()`.
+ `write_scss()` standardizes on the `filename` parameter (soft-deprecating `name`, #87), strips optional `.scss` extensions, appends new stylesheets in proper CSS cascade order after `custom.scss`, and copies its starter template from `rum_templates`.
+ `write_readme()` sources the README template from `rum_templates` with deterministic overwrite prompts.
+ `write_man()` accepts `pkg::dataset` references and data frames directly, unwraps `haven_labelled` columns cleanly, sanitizes multi-line, pipe (`|`), and brace (`{}`) variable labels, and writes complete UTF-8 `.R` files atomically.
+ Bundled `style.css` and `bibliography.bib` directly in the `bookdown_siteWithRum` R Markdown skeleton and fixed YAML headers across R Markdown skeletons.
+ Updated package vignettes and `README.md` to reflect `bib/` citation directories, package `inst/scripts/` and `read_extdata()` helpers, `write_scss(filename, add_to_yaml = TRUE)`, and built-in AI assistant workflows.

## Deprecations & Breaking Changes
+ Soft-deprecated `write_notes()` and removed `dated_progress_notes.md` from default project/package scaffolding in favor of `DECISIONS.md` and `CHANGELOG.md` / `NEWS.md` via the bundled `rum-devlog` skill.
+ Soft-deprecated `write_scss(name = )` in favor of `write_scss(filename = )` (#87).
+ Removed bundled `inst/gists` and `inst/img` directories now served from `rum_templates`.
+ An internet connection is required once to populate the local `rum_templates` cache (unless `RUM_TEMPLATES_PATH` points to a local checkout); subsequent offline sessions use the cached bundle automatically.

## Testing & Internal Cleanup
+ Added `yaml` to Imports.
+ Removed legacy internal helpers replaced by manifest infrastructure: `.add_quarto_doc()`, `.add_rmd_doc()`, `.add_gitignore()`, `.generate_scss_template()`, `.add_citation_files()`, `.rUM_package_builder()`, and `.add_notes_to_Rbuildignore()`.
+ Expanded offline mocked unit tests across manifest infrastructure, persistent offline cache fallback, exported artifact functions, `find_slides()`, and `show_slides()`, and added a Codecov GitHub Actions workflow and badge (#86).


# rUM 2.3.0 (rUM Swizzle)

## Enhancements
* The `template` argument for `write_slides()` now uses `"rmed"` instead of `"rmed2025"`. The old value still works but is no longer documented. Year references removed from RMed templates and files.

## Minor improvements
+ Removed references to 2025 from RMed templates & files.

## Fixes
+ Breaking changes to {purrr} in version 1.2.0 required internal changes to `write_slides()`. Uses `walk()` instead of `map_chr()` to download `"rmed"` slide art & files. See https://purrr.tidyverse.org/news/index.html#breaking-changes-1-2-0 for more information.


# rUM 2.2.1

+ Fix bug in write_man() that showed ordered factors as factors


# rUM 2.2.0 (rUM Runner)

CRAN release: 2025-07

## New Features
+ Automatic dataset documentation with `write_man()`.
+ Slide templates: Quarto reveal.js templates with `write_slides()`
+ Functions for embedding and finding slides in a package with `find_slides()` and `show_slides()`

## Enhnancements
+ `write_quarto()` now accepts an `example` argument.


# rUM 2.1.0 (rUM and Coke)

CRAN release: 2025-03-21

## New Features
+ Automated `make_project(vignette = TRUE)` development tools during project build to:
  + Restructure `analysis.qmd` & `analysis.Rmd` YAML for vignette engine & building
  + Write user's `rUM` project packages to `DESCRIPTION` file
  + Adding `VignetteBuilder` (quarto or knitr) to project `DESCRIPTION` file
  + Adding items to `.Rbuildignore` & `vignettes/.gitignore`
+ Added `rUM::write_quarto()` function that will add a quarto html document with many useful options.
+ Provides `README.md` and `dated_progress_notes.md` templates to new projects. It will ask if you'd like to overwrite an existing README if one exists in the current project.
+ Added custom.scss to non-package Quarto projects with `rUM::write_scss()` during project creation.
+ Added `rUM::write_scss()` function that will write a SCSS template for Quarto projects. This function is available for use outside of `rUM` projects when a SCSS template is desired (perhaps, a Quarto blog or Shiny app). 
+ Added `rUM::write_notes()` function to add the progress notes template to users' projects when one doesn't currently exist. This can be used in rUM & non-rUM projects alike.
+ Added `rUM::write_readme()` function to insert a structured README file to users' projects when one doesn't currently exist. This may also be used in rUM & non-rUM projects.
+ Added a vignette for using Posit.Cloud

## Other Updates
+ removed `RUN_ME_FIRST.R` file and instead automate those processes as described above
+ Empty `ggplot()` object included in manuscript & vignette templates
+ Moved external GitHub gists to `inst/gists` directory

## Bug Fixes
+ (none)


# rUM 2.0.0 (Overproof Rum) 

+ Added an `example` argument to `make_project()`
+ Added a `vignette` argument to `make_project()`
+ Added knitr chunk options for graphics
+ Fixed a bug that caused duplicate RStudio windows to open if run from menus

## 1.1.0.9008
+ fix carriage return for building vignettes in Windows

## 1.1.0.9007
+ prevent an empty folder from being created if a package name is invalid
+ add header to manual_change_*_vignette.R

## 1.1.0.9006
+ add dependencies_fix function for passing Windows CRAN checks.

## 1.1.0.9005
+ set package dependencies for vignette to suggest

## 1.1.0.9004
+ add ?rUM page fix quarto check message to be clearer

## 1.1.0.9003

+ Add an `overwrite` option to `make_project`

## 1.1.0.9002

+ change run_me.R to RUN_ME_FIRST.R

# rUM 1.1

+ Harmonize templates
+ Include example paper option 

# rUM 1.0.2

+ Linted all code
  + Cleaned up `options(dplyr.summarise.inform=F)` everywhere.
+ Added gists to a inst/gists directory

# rUM 1.0.1

+ Added https:\\ to github link in vignettes for CRAN
+ Removed `![rUM](package-logo.jpg){width=0.8in}` from vignette titles
+ Added "with R Markdown" to vignette title

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



