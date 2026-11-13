# Offline unit tests for exported artifact-generating functions

build_realistic_scaffold <- function(
  base_dir = withr::local_tempdir(.local_envir = parent.frame())
) {
  s <- file.path(base_dir, "rum_templates_mock")
  dirs <- c(
    "manifests",
    "analysis",
    "slides",
    "styling/img",
    "supporting/partials",
    "supporting/inst/scripts",
    "supporting/agents/skills/rum-workflow-project",
    "supporting/agents/skills/rum-workflow-package",
    "supporting/agents/skills/rum-devlog",
    "supporting/agents/skills/rum-commit"
  )
  for (d in dirs) dir.create(file.path(s, d), recursive = TRUE)

  writeLines("knitr:\n  opts_chunk:\n    echo: false",
             file.path(s, "supporting/partials/knitr_analysis.yml"))
  writeLines("knitr:\n  opts_chunk:\n    echo: false",
             file.path(s, "supporting/partials/knitr_slides.yml"))
  writeLines(
    "```{r setup, echo=FALSE}\nknitr::opts_chunk$set(echo = FALSE)\n```",
    file.path(s, "supporting/partials/knitr_rmd.Rmd")
  )

  qmd_hdr <- c(
    "---",
    "format:",
    "  html:",
    "    link-external-newwindow: true",
    "    embed-resources: true",
    "    theme:",
    "      - default",
    "      - custom.scss",
    "{{knitr_opts}}",
    "bibliography: [bib/references.bib, bib/packages.bib]",
    "csl: bib/apa.csl",
    "template-version: 1",
    "---"
  )
  for (f in c(
    "default_quarto.qmd",
    "analysis_qmd_with_example.qmd",
    "analysis_qmd_wo_example.qmd",
    "analysis_qmd_vignette_with_example.qmd",
    "analysis_qmd_vignette_wo_example.qmd"
  )) {
    writeLines(qmd_hdr, file.path(s, "analysis", f))
  }
  for (f in c(
    "analysis_rmd_with_example.Rmd",
    "analysis_rmd_wo_example.Rmd",
    "analysis_rmd_vignette_with_example.Rmd",
    "analysis_rmd_vignette_wo_example.Rmd"
  )) {
    writeLines(c("---", "template-version: 1", "---", "{{knitr_opts}}"),
               file.path(s, "analysis", f))
  }
  for (f in c(
    "quarto_slides.qmd",
    "quarto_slides_example.qmd",
    "quarto_slides_miami.qmd",
    "quarto_slides_example_miami.qmd",
    "quarto_slides_rmed.qmd",
    "quarto_slides_example_rmed.qmd"
  )) {
    writeLines(c("---", "{{knitr_opts}}", "template-version: 1", "---"),
               file.path(s, "slides", f))
  }

  writeLines("*.arrow\n*.xpt", file.path(s, "supporting/.gitignore.md"))
  writeLines("*.html\n*.R", file.path(s, "supporting/vignettes_gitignore.md"))
  writeLines("# Project README", file.path(s, "supporting/README.md"))
  csl_files <- c(
    "apa.csl",
    "mla.csl",
    "the-new-england-journal-of-medicine.csl"
  )
  for (csl in csl_files) {
    writeLines("<style/>", file.path(s, "supporting", csl))
  }
  writeLines("# libs", file.path(s, "supporting/inst/scripts/libraries.R"))
  writeLines("# run", file.path(s, "supporting/inst/scripts/do_everything.R"))
  writeLines("# agents", file.path(s, "supporting/agents/AGENTS_project.md"))
  writeLines(
    "# agents pkg",
    file.path(s, "supporting/agents/AGENTS_package.md")
  )
  writeLines("@AGENTS.md", file.path(s, "supporting/agents/CLAUDE.md"))
  writeLines("@AGENTS.md", file.path(s, "supporting/agents/GEMINI.md"))
  for (sk in c(
    "rum-workflow-project",
    "rum-workflow-package",
    "rum-devlog",
    "rum-commit"
  )) {
    writeLines("---\nname: x\n---",
               file.path(s, "supporting/agents/skills", sk, "SKILL.md"))
  }

  writeLines("/*-- scss:defaults --*/", file.path(s, "styling/custom.scss"))
  writeLines("/* slides */", file.path(s, "styling/slides.scss"))
  writeLines("/* miami */", file.path(s, "styling/custom_miami.scss"))
  writeLines("/* rmed */", file.path(s, "styling/slides_example_rmed.scss"))
  writeLines("{}", file.path(s, "styling/rstudio_default-light.theme"))
  writeLines("<script></script>", file.path(s, "styling/clean_title_page.html"))
  for (img in c(
    "rmed_background.png",
    "rmed_narrow.png",
    "rmed.png",
    "rmed.ico"
  )) {
    writeBin(raw(4), file.path(s, "styling/img", img))
  }

  writeLines(
    '
name: quarto_standalone
variants:
  default:
    when: { example: null }
    files:
      - src: analysis/default_quarto.qmd
        dst: "{filename}"
        inject: { knitr_opts: supporting/partials/knitr_analysis.yml }
  example:
    when: { example: true }
    files:
      - src: analysis/analysis_qmd_with_example.qmd
        dst: "{filename}"
        inject: { knitr_opts: supporting/partials/knitr_analysis.yml }
      - { src: supporting/apa.csl, dst: bib/apa.csl }
    actions:
      - { type: create_bib, path: bib }
  no_example:
    when: { example: false }
    files:
      - src: analysis/analysis_qmd_wo_example.qmd
        dst: "{filename}"
        inject: { knitr_opts: supporting/partials/knitr_analysis.yml }
      - { src: supporting/apa.csl, dst: bib/apa.csl }
    actions:
      - { type: create_bib, path: bib }
',
    file.path(s, "manifests/quarto_standalone.yml")
  )

  writeLines(
    '
name: slides
common:
  files:
    - src: styling/rstudio_default-light.theme
      dst: "{path}/rstudio_default-light.theme"
  actions:
    - { type: create_dir, path: "{path}" }
variants:
  default:
    when: { template: none, example: false }
    files:
      - src: slides/quarto_slides.qmd
        dst: "{path}/{filename}"
        inject: { knitr_opts: supporting/partials/knitr_slides.yml }
      - { src: styling/slides.scss, dst: "{path}/slides.scss" }
  default_example:
    when: { template: none, example: true }
    files:
      - src: slides/quarto_slides_example.qmd
        dst: "{path}/{filename}"
        inject: { knitr_opts: supporting/partials/knitr_slides.yml }
      - { src: styling/slides.scss, dst: "{path}/slides.scss" }
  miami:
    when: { template: miami, example: false }
    files:
      - src: slides/quarto_slides_miami.qmd
        dst: "{path}/{filename}"
        inject: { knitr_opts: supporting/partials/knitr_slides.yml }
      - { src: styling/custom_miami.scss, dst: "{path}/slides.scss" }
  rmed:
    when: { template: rmed, example: false }
    files:
      - src: slides/quarto_slides_rmed.qmd
        dst: "{path}/{filename}"
        inject: { knitr_opts: supporting/partials/knitr_slides.yml }
      - { src: styling/slides_example_rmed.scss, dst: "{path}/slides.scss" }
      - src: styling/clean_title_page.html
        dst: "{path}/img/clean_title_page.html"
      - { src: styling/img/rmed.png, dst: "{path}/img/rmed.png" }
',
    file.path(s, "manifests/slides.yml")
  )

  writeLines(
    "
name: project
common:
  files:
    - { src: supporting/.gitignore.md, dst: .gitignore }
    - { src: supporting/apa.csl, dst: bib/apa.csl }
    - { src: supporting/agents/AGENTS_project.md, dst: AGENTS.md }
  actions:
    - { type: create_dir, path: data }
    - { type: write_readme }
    - { type: create_bib, path: bib }
variants:
  quarto_example:
    when: { format: quarto, example: true }
    files:
      - src: analysis/analysis_qmd_with_example.qmd
        dst: analysis.qmd
        inject: { knitr_opts: supporting/partials/knitr_analysis.yml }
    actions:
      - { type: write_scss }
  rmd:
    when: { format: rmd, example: false }
    files:
      - src: analysis/analysis_rmd_wo_example.Rmd
        dst: analysis.Rmd
        inject: { knitr_opts: supporting/partials/knitr_rmd.Rmd }
",
    file.path(s, "manifests/project.yml")
  )

  writeLines(
    "
name: package
common:
  files:
    - { src: supporting/.gitignore.md, dst: .gitignore }
    - { src: supporting/vignettes_gitignore.md, dst: vignettes/.gitignore }
    - { src: supporting/apa.csl, dst: vignettes/bib/apa.csl }
    - src: supporting/inst/scripts/libraries.R
      dst: inst/scripts/libraries.R
    - src: supporting/inst/scripts/do_everything.R
      dst: inst/scripts/do_everything.R
    - { src: supporting/agents/AGENTS_package.md, dst: AGENTS.md }
  actions:
    - { type: create_dir, path: inst/extdata }
    - { type: create_dir, path: vignettes }
    - { type: write_readme }
    - { type: create_bib, path: vignettes/bib }
    - { type: build_description }
variants:
  quarto:
    when: { format: quarto, example: false }
    files:
      - src: analysis/analysis_qmd_vignette_wo_example.qmd
        dst: vignettes/analysis.qmd
        inject: { knitr_opts: supporting/partials/knitr_analysis.yml }
  rmd:
    when: { format: rmd, example: false }
    files:
      - src: analysis/analysis_rmd_vignette_wo_example.Rmd
        dst: vignettes/analysis.Rmd
        inject: { knitr_opts: supporting/partials/knitr_rmd.Rmd }
",
    file.path(s, "manifests/package.yml")
  )

  s
}

test_that("write_quarto scaffolds standalone & paper templates and validates", {
  scaffold <- build_realistic_scaffold()
  target <- withr::local_tempdir()
  local_mocked_bindings(
    .get_rum_scaffold = function() scaffold,
    edit_file = function(...) invisible(NULL)
  )

  expect_error(
    write_quarto(filename = "bad", path = target, example = "yes"),
    "Parameter `example` must be NULL, TRUE, or FALSE"
  )
  expect_error(
    write_quarto(filename = "sub/doc", path = target),
    "forward slash"
  )

  write_quarto(filename = "notes.qmd", path = target, example = NULL)
  expect_true(file.exists(file.path(target, "notes.qmd")))
  notes_lines <- readLines(file.path(target, "notes.qmd"))
  expect_false(any(grepl("knitr_opts", notes_lines)))

  write_quarto(filename = "paper", path = target, example = TRUE)
  expect_true(file.exists(file.path(target, "paper.qmd")))
  expect_true(file.exists(file.path(target, "bib/apa.csl")))
  expect_true(file.exists(file.path(target, "bib/packages.bib")))
})

test_that("write_slides handles .qmd suffix, batch decks, and rmed assets", {
  scaffold <- build_realistic_scaffold()
  target <- withr::local_tempdir()
  local_mocked_bindings(
    .get_rum_scaffold = function() scaffold,
    edit_file = function(...) invisible(NULL)
  )

  # Regression for #87: passing .qmd suffix should succeed
  write_slides(
    filenames = c("talk_1.qmd", "talk_2"),
    path = target,
    new_folder = "rmed_deck",
    template = "rmed2025",
    example = FALSE
  )

  expect_true(file.exists(file.path(target, "rmed_deck/talk_1.qmd")))
  expect_true(file.exists(file.path(target, "rmed_deck/talk_2.qmd")))
  expect_true(file.exists(file.path(target, "rmed_deck/slides.scss")))
  clean_html <- file.path(target, "rmed_deck/img/clean_title_page.html")
  expect_true(file.exists(clean_html))
  expect_true(file.exists(file.path(target, "rmed_deck/img/rmed.png")))
})

test_that("write_scss uses filename, updates YAML, and deprecates name", {
  scaffold <- build_realistic_scaffold()
  target <- withr::local_tempdir()
  local_mocked_bindings(.get_rum_scaffold = function() scaffold)

  writeLines(
    c(
      "---",
      "format:",
      "  html:",
      "    link-external-newwindow: true",
      "    embed-resources: true",
      "    theme:",
      "      - default",
      "      - custom.scss",
      "---"
    ),
    file.path(target, "analysis.qmd")
  )

  suppressMessages(
    write_scss(filename = "tables", path = target, add_to_yaml = TRUE)
  )
  expect_true(file.exists(file.path(target, "tables.scss")))
  qmd_after <- readLines(file.path(target, "analysis.qmd"))
  expect_true(any(grepl("tables\\.scss", qmd_after)))

  lifecycle::expect_deprecated(
    suppressMessages(write_scss(name = "legacy_style", path = target))
  )
  expect_true(file.exists(file.path(target, "legacy_style.scss")))
})

test_that("write_notes emits deprecation warning and creates progress file", {
  target <- withr::local_tempdir()
  lifecycle::expect_deprecated(
    suppressMessages(write_notes(path = target))
  )
  expect_true(file.exists(file.path(target, "dated_progress_notes.md")))
})

test_that("write_man documents data.frame and tibble variables in R/", {
  pkg_dir <- withr::local_tempdir()
  dir.create(file.path(pkg_dir, "R"))
  withr::local_dir(pkg_dir)

  demo_tbl <- dplyr::tibble(
    count = 1:3,
    grp = factor(c("a", "b", "a")),
    ord = ordered(c("low", "high", "low"), levels = c("low", "high")),
    txt = c("x", "y", "z")
  )
  labelled::var_label(demo_tbl$txt) <- "Label [code] {drop}"
  assign("demo_tbl_test", demo_tbl, envir = .GlobalEnv)
  withr::defer(rm(list = "demo_tbl_test", envir = .GlobalEnv))

  suppressMessages(write_man("demo_tbl_test"))
  man_lines <- readLines(file.path("R", "demo_tbl_test.R"), warn = FALSE)
  expect_true(any(grepl("A tibble with 3 rows and 4 variables", man_lines)))
  expect_true(any(grepl("Ordered factor", man_lines)))
  expect_true(any(grepl("Label `code`", man_lines)))
})

test_that("make_project and make_package validate and scaffold offline", {
  scaffold <- build_realistic_scaffold()
  base <- withr::local_tempdir()
  local_mocked_bindings(
    .get_rum_scaffold = function() scaffold,
    create_project = function(path, ...) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      writeLines("Version: 1.0", file.path(path, "proj.Rproj"))
      if (!file.exists(file.path(path, ".gitignore"))) {
        writeLines(".Rproj.user", file.path(path, ".gitignore"))
      }
    },
    create_package = function(path, ...) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      writeLines("Version: 1.0", file.path(path, "pkg.Rproj"))
      if (!file.exists(file.path(path, ".gitignore"))) {
        writeLines(".Rproj.user", file.path(path, ".gitignore"))
      }
      writeLines(
        c(
          "Package: validpkg",
          "Title: Demo",
          "Version: 0.0.0.9000",
          "Encoding: UTF-8",
          "VignetteBuilder: knitr"
        ),
        file.path(path, "DESCRIPTION")
      )
    },
    quarto_version = function() NULL
  )

  # Invalid package name aborts before creating directory
  bad_pkg <- file.path(base, "123_invalid")
  expect_error(
    make_package(path = bad_pkg, type = "R"),
    "not a valid R package"
  )
  expect_false(dir.exists(bad_pkg))

  # R Markdown package succeeds when quarto_version() is NULL (no Quarto CLI)
  good_pkg <- file.path(base, "validpkg")
  suppressMessages(
    make_package(path = good_pkg, type = "R", openInteractive = FALSE)
  )
  expect_true(file.exists(file.path(good_pkg, "vignettes/analysis.Rmd")))
  expect_true(
    any(grepl("\\*\\.arrow", readLines(file.path(good_pkg, ".gitignore"))))
  )
  expect_true(file.exists(file.path(good_pkg, "vignettes/.gitignore")))
  expect_true(file.exists(file.path(good_pkg, "vignettes/bib/apa.csl")))
  expect_true(file.exists(file.path(good_pkg, "inst/scripts/libraries.R")))
  expect_true(file.exists(file.path(good_pkg, "inst/scripts/do_everything.R")))
  expect_true(dir.exists(file.path(good_pkg, "inst/extdata")))
  desc_lines <- readLines(file.path(good_pkg, "DESCRIPTION"))
  expect_equal(sum(grepl("^VignetteBuilder:", desc_lines)), 1L)

  # Re-running on existing vignette aborts cleanly (#5)
  expect_error(
    make_package(
      path = good_pkg,
      type = "R",
      overwrite = TRUE,
      openInteractive = FALSE
    ),
    "already has an analysis file"
  )

  # Posit.Cloud / Positron "./" relative path resolves enclosing folder name
  cloud_pkg <- file.path(base, "cloudpkg")
  dir.create(cloud_pkg)
  writeLines("Version: 1.0\n", file.path(cloud_pkg, "project.Rproj"))
  withr::with_dir(cloud_pkg, {
    suppressMessages(
      make_package(
        path = "./",
        type = "R",
        overwrite = TRUE,
        openInteractive = FALSE
      )
    )
  })
  expect_true(file.exists(file.path(cloud_pkg, "vignettes/analysis.Rmd")))
  rbuild_lines <- readLines(file.path(cloud_pkg, ".Rbuildignore"), warn = FALSE)
  expect_true("do_everything_summary\\.RData$" %in% rbuild_lines)

  # Quarto project scaffolds bib/, custom.scss, README.md, and AGENTS.md
  proj_dir <- file.path(base, "demo_proj")
  expect_warning(
    suppressMessages(
      make_project(
        path = proj_dir,
        type = "quarto",
        example = TRUE,
        openInteractive = FALSE
      )
    ),
    "Quarto CLI was not found"
  )
  expect_true(file.exists(file.path(proj_dir, "analysis.qmd")))
  expect_true(
    any(grepl("\\*\\.arrow", readLines(file.path(proj_dir, ".gitignore"))))
  )
  expect_true(file.path(proj_dir, "bib/apa.csl") |> file.exists())
  expect_true(file.path(proj_dir, "custom.scss") |> file.exists())
  expect_true(file.path(proj_dir, "README.md") |> file.exists())
  expect_true(file.path(proj_dir, "AGENTS.md") |> file.exists())

  # Re-running make_project on existing analysis.qmd aborts cleanly
  expect_error(
    make_project(path = proj_dir, type = "Q", openInteractive = FALSE),
    "already has an analysis file"
  )

  # Non-destructive make_project(overwrite = FALSE) preserves user files
  existing_dir <- file.path(base, "existing_user_dir")
  dir.create(existing_dir)
  writeLines("user rproj", file.path(existing_dir, "keep.Rproj"))
  writeLines("user gitignore", file.path(existing_dir, ".gitignore"))
  writeLines("user readme", file.path(existing_dir, "README.md"))
  writeLines("user scss", file.path(existing_dir, "custom.scss"))
  writeLines("user agents", file.path(existing_dir, "AGENTS.md"))
  expect_warning(
    suppressMessages(
      make_project(
        path = existing_dir,
        type = "Q",
        example = TRUE,
        overwrite = FALSE,
        openInteractive = FALSE
      )
    ),
    "Quarto CLI was not found"
  )
  expect_true(file.exists(file.path(existing_dir, "analysis.qmd")))
  expect_equal(
    readLines(file.path(existing_dir, ".gitignore")),
    "user gitignore"
  )
  expect_equal(readLines(file.path(existing_dir, "README.md")), "user readme")
  expect_equal(readLines(file.path(existing_dir, "custom.scss")), "user scss")
  expect_equal(readLines(file.path(existing_dir, "AGENTS.md")), "user agents")

  # Quarto package aborts cleanly when quarto_version() is missing/old
  q_old <- file.path(base, "qoldpkg")
  expect_error(
    make_package(path = q_old, type = "Q", openInteractive = FALSE),
    "STOPPING: You need a modern version of Quarto"
  )
  expect_error(
    make_package(path = q_old, type = "qmd", openInteractive = FALSE),
    "STOPPING: You need a modern version of Quarto"
  )
  expect_true(rUM:::.arg_validation("qmd", TRUE, FALSE, FALSE))
  expect_true(rUM:::.arg_validation("QMD", TRUE, FALSE, FALSE))

  # Single-char package name rejected by CRAN rule
  expect_error(
    make_package(path = file.path(base, "a"), type = "R"),
    "at least two characters"
  )

  # Existing .Rproj without overwrite aborts cleanly in make_package()
  has_proj_pkg <- file.path(base, "existingproj")
  dir.create(has_proj_pkg)
  writeLines("Version: 1.0", file.path(has_proj_pkg, "existing.Rproj"))
  expect_error(
    make_package(
      path = has_proj_pkg,
      type = "rmd",
      overwrite = FALSE,
      openInteractive = FALSE
    ),
    "already has an RStudio project file"
  )
})

test_that("make_package Quarto path and make_project(vignette = TRUE) work", {
  scaffold <- build_realistic_scaffold()
  base <- withr::local_tempdir()
  local_mocked_bindings(
    .get_rum_scaffold = function() scaffold,
    create_package = function(path, ...) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      writeLines("Version: 1.0", file.path(path, "pkg.Rproj"))
      writeLines(
        c(
          "Package: qpkg",
          "Title: Demo",
          "Version: 0.0.0.9000",
          "Encoding: UTF-8"
        ),
        file.path(path, "DESCRIPTION")
      )
    },
    quarto_version = function() numeric_version("1.5.57")
  )

  local_mocked_bindings(
    check_installed = function(pkg, ...) {
      if (any(pkg == "arrow")) stop("arrow missing on host")
      invisible(NULL)
    },
    .package = "rlang"
  )

  q_pkg <- file.path(base, "qpkg")
  lifecycle::expect_deprecated(
    suppressMessages(
      make_project(
        path = q_pkg,
        type = "Q",
        example = FALSE,
        vignette = TRUE,
        openInteractive = FALSE
      )
    )
  )
  expect_true(file.exists(file.path(q_pkg, "vignettes/analysis.qmd")))
  desc_txt <- readLines(file.path(q_pkg, "DESCRIPTION"), warn = FALSE)
  expect_true(any(grepl("^\\s*arrow,$", desc_txt)))
  expect_true(any(grepl("^VignetteBuilder: quarto$", desc_txt)))
})

test_that("write_slides covers folder modes, fallback deck, and pkg warning", {
  scaffold <- build_realistic_scaffold()
  base <- withr::local_tempdir()
  local_mocked_bindings(
    .get_rum_scaffold = function() scaffold,
    edit_file = function(...) invisible(NULL)
  )

  expect_error(write_slides("deck", path = base, example = "bad"), "TRUE")
  expect_error(write_slides(character(0), path = base), "Invalid filename")
  expect_error(write_slides(123, path = base), "must be character")
  expect_error(write_slides("deck", path = base, new_folder = 1), "character")
  expect_error(
    write_slides("deck", path = base, new_folder = "bad/name"),
    'Invalid "new_folder"'
  )

  # new_folder = "none" + pre-existing slides.scss message
  none_dir <- file.path(base, "flat")
  dir.create(none_dir)
  writeLines("/* keep */", file.path(none_dir, "slides.scss"))
  expect_message(
    write_slides("flat_talk", path = none_dir, new_folder = "none"),
    "A SCSS file has been found"
  )
  expect_true(file.exists(file.path(none_dir, "flat_talk.qmd")))
  expect_error(
    write_slides("flat_talk", path = none_dir, new_folder = "none"),
    "already exists"
  )

  # Strict template & format validation + case-insensitive "Miami"
  expect_error(
    write_slides("custom_deck", path = base, template = "unknown_custom"),
    'must be one of "none", "miami", or "rmed"'
  )
  expect_error(
    write_slides("custom_deck", path = base, format = "pptx"),
    'only supports "revealjs"'
  )
  write_slides(
    "custom_deck.QMD",
    path = base,
    new_folder = "slides_",
    template = "Miami"
  )
  pref_dir <- file.path(base, "slides_custom_deck")
  expect_true(file.exists(file.path(pref_dir, "custom_deck.qmd")))
  expect_true(file.exists(file.path(pref_dir, "slides.scss")))

  # Package root without inst/ emits CRAN warning; path = inst or inst/slides
  # auto-creates without warning or double-nesting
  pkg_root <- file.path(base, "simpkg")
  dir.create(pkg_root)
  writeLines("Package: simpkg", file.path(pkg_root, "DESCRIPTION"))
  expect_warning(
    write_slides("pkg_talk", path = pkg_root, new_folder = "slides"),
    "detected that you are currently in a package"
  )
  expect_no_warning(
    write_slides("inst_talk", path = file.path(pkg_root, "inst"))
  )
  expect_true(file.exists(file.path(pkg_root, "inst/slides/inst_talk.qmd")))
  expect_no_warning(
    write_slides("direct_talk", path = file.path(pkg_root, "inst/slides"))
  )
  expect_true(file.exists(file.path(pkg_root, "inst/slides/direct_talk.qmd")))
  expect_false(dir.exists(file.path(pkg_root, "inst/slides/slides")))
})

test_that("write_quarto, write_scss, write_readme, and write_man edge cases", {
  scaffold <- build_realistic_scaffold()
  target <- withr::local_tempdir()
  menu_ans <- 1L
  rlang::local_interactive(TRUE)
  local_mocked_bindings(
    .get_rum_scaffold = function() scaffold,
    edit_file = function(...) invisible(NULL),
    menu = function(choices, ...) menu_ans
  )

  # write_quarto validation & duplicate error
  expect_error(write_quarto(NULL, path = target), "Invalid filename")
  expect_error(write_quarto(123, path = target), "must be character")
  expect_error(write_quarto("doc", path = target, example = NA), "NULL, TRUE")
  write_quarto("dup_doc.QMD", path = target)
  expect_error(
    write_quarto("dup_doc", path = target),
    "already exists"
  )

  # write_scss extension stripping, cascade order, cancel/accept & inline theme
  writeLines(
    c(
      "---",
      "format:",
      "  html:",
      "    theme:",
      "      - default",
      "      - custom.scss",
      "---"
    ),
    file.path(target, "analysis.qmd")
  )
  suppressMessages(
    write_scss("extra.SCSS", path = target, add_to_yaml = TRUE)
  )
  qmd_cascade <- readLines(file.path(target, "analysis.qmd"))
  expect_true(
    which(grepl("custom\\.scss", qmd_cascade)) <
      which(grepl("extra\\.scss", qmd_cascade))
  )

  menu_ans <- 1L
  suppressMessages(res_cancel <- write_scss("extra", path = target))
  expect_null(res_cancel)

  menu_ans <- 2L
  writeLines(
    c("---", "format:", "  html:", "    theme: [default]", "---"),
    file.path(target, "analysis.qmd")
  )
  suppressMessages(
    write_scss("org_theme", path = target, add_to_yaml = TRUE)
  )
  expect_true(file.exists(file.path(target, "org_theme.scss")))
  expect_true(
    any(grepl("org_theme\\.scss", readLines(file.path(target, "analysis.qmd"))))
  )
  expect_message(rUM:::.display_reference_links(), "SCSS styling options")

  # write_readme create, cancel overwrite (1), and accept overwrite (2)
  suppressMessages(write_readme(path = target))
  expect_true(file.exists(file.path(target, "README.md")))
  menu_ans <- 1L
  suppressMessages(expect_null(write_readme(path = target)))
  menu_ans <- 2L
  suppressMessages(write_readme(path = target))

  # write_man local df, pkg::dataset, haven_labelled, pipes/newlines, >100 levs
  pkg_dir <- withr::local_tempdir()
  withr::with_dir(pkg_dir, {
    expect_error(write_man("missing_obj_xyz"), "not found in the global")

    assign("not_a_df_test", 1:5, envir = .GlobalEnv)
    withr::defer(rm(list = "not_a_df_test", envir = .GlobalEnv))
    expect_error(write_man("not_a_df_test"), "not a data.frame")

    df_plain <- data.frame(
      score = c(10.5, 20.5),
      big_fac = factor(paste0("lvl_", seq_len(101)))[1:2]
    )
    class(df_plain$score) <- c("haven_labelled", "vctrs_vctr", "double")
    attr(df_plain$score, "label") <- "Line 1\nLine 2 | pipe & {brace}"

    expect_error(write_man(df_plain), "R folder does not exist")
    dir.create("R")
    expect_warning(
      suppressMessages(write_man(df_plain)),
      "more than 100 levels"
    )
    expect_true(file.exists("R/df_plain.R"))
    man_txt <- readLines("R/df_plain.R", warn = FALSE)
    expect_true(any(grepl("numeric", man_txt)))
    expect_false(any(grepl("haven_labelled", man_txt)))
    expect_true(any(grepl("\\\\\\|", man_txt)))
    expect_error(write_man("df_plain.R"), "already exists")

    suppressMessages(write_man(datasets::mtcars))
    expect_true(file.exists("R/mtcars.R"))
  })

  # Internal validators (.arg_validation & .validate_filename)
  expect_error(rUM:::.arg_validation(1, FALSE, FALSE, FALSE), "character")
  expect_error(rUM:::.arg_validation("Z", FALSE, FALSE, FALSE), "Quarto")
  expect_error(rUM:::.arg_validation("Q", NA, FALSE, FALSE), "example")
  expect_error(rUM:::.arg_validation("Q", FALSE, "bad", FALSE), "overwrite")
  expect_error(
    rUM:::.arg_validation("Q", FALSE, FALSE, "bad"),
    "openInteractive"
  )
  expect_error(rUM:::.validate_path(123), "Invalid `path`")
  expect_error(rUM:::.validate_filename(""), "Invalid filename")
  expect_error(rUM:::.validate_filename(NA_character_), "must be character")
  expect_error(rUM:::.validate_filename("sub\\doc"), "backslash")
  expect_error(rUM:::.validate_filename("file.txt"), "extensions")
  expect_error(rUM:::.validate_filename("bad space"), "only letters")
})
