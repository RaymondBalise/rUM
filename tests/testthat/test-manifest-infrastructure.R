# Tests for manifest-driven template infrastructure

create_mock_scaffold <- function(
  base_dir = withr::local_tempdir(.local_envir = parent.frame())
) {
  scaffold <- file.path(base_dir, "mock_scaffold")
  dir.create(file.path(scaffold, "manifests"), recursive = TRUE)
  dir.create(file.path(scaffold, "analysis"), recursive = TRUE)
  dir.create(file.path(scaffold, "supporting/partials"), recursive = TRUE)
  dir.create(file.path(scaffold, "styling/img"), recursive = TRUE)
  dir.create(file.path(scaffold, "slides"), recursive = TRUE)

  writeLines(
    "mock analysis qmd",
    file.path(scaffold, "analysis/analysis_qmd_with_example.qmd")
  )
  writeLines(
    "mock analysis no ex",
    file.path(scaffold, "analysis/analysis_qmd_wo_example.qmd")
  )
  writeLines(
    "mock default qmd",
    file.path(scaffold, "analysis/default_quarto.qmd")
  )
  writeLines(
    "mock gitignore",
    file.path(scaffold, "supporting/.gitignore.md")
  )
  writeLines("mock readme", file.path(scaffold, "supporting/README.md"))
  writeLines("mock scss", file.path(scaffold, "styling/custom.scss"))
  writeLines("mock slides", file.path(scaffold, "slides/quarto_slides.qmd"))

  writeLines(
    "
name: test
description: Test manifest

common:
  files:
    - { src: supporting/.gitignore.md, dst: .gitignore }
    - { src: supporting/README.md, dst: README.md }
  actions:
    - { type: create_dir, path: data }

variants:
  quarto_example:
    when: { format: quarto, example: true }
    files:
      - { src: analysis/analysis_qmd_with_example.qmd, dst: analysis.qmd }

  quarto:
    when: { format: quarto, example: false }
    files:
      - { src: analysis/analysis_qmd_wo_example.qmd, dst: analysis.qmd }
",
    file.path(scaffold, "manifests/test.yml")
  )

  writeLines(
    '
name: standalone
description: Test standalone

variants:
  default:
    when: { example: null }
    files:
      - { src: analysis/default_quarto.qmd, dst: "{filename}" }

  example:
    when: { example: true }
    files:
      - { src: analysis/analysis_qmd_with_example.qmd, dst: "{filename}" }
',
    file.path(scaffold, "manifests/standalone.yml")
  )

  writeLines(
    '
name: pathed
description: Test with path placeholders

common:
  files:
    - { src: styling/custom.scss, dst: "{path}/theme.scss" }
  actions:
    - { type: create_dir, path: "{path}" }

variants:
  default:
    when: { template: none }
    files:
      - { src: slides/quarto_slides.qmd, dst: "{path}/presentation.qmd" }
',
    file.path(scaffold, "manifests/pathed.yml")
  )

  scaffold
}


# --- .resolve_variant() tests ---

test_that(".resolve_variant matches on logical TRUE/FALSE", {
  manifest <- yaml::read_yaml(text = "
variants:
  a:
    when: { format: quarto, example: true }
    files: []
  b:
    when: { format: quarto, example: false }
    files: []
")
  res1 <- rUM:::.resolve_variant(
    manifest,
    list(format = "quarto", example = TRUE)
  )
  expect_identical(res1$when$format, "quarto")
  expect_true(res1$when$example)

  res2 <- rUM:::.resolve_variant(
    manifest,
    list(format = "quarto", example = FALSE)
  )
  expect_false(res2$when$example)
})

test_that(".resolve_variant matches NULL conditions", {
  manifest <- yaml::read_yaml(text = "
variants:
  default:
    when: { example: null }
    files: []
  with_example:
    when: { example: true }
    files: []
")
  result <- rUM:::.resolve_variant(manifest, list(example = NULL))
  expect_null(result$when$example)

  result2 <- rUM:::.resolve_variant(manifest, list(example = TRUE))
  expect_true(result2$when$example)
})

test_that(".resolve_variant errors when no variant matches", {
  manifest <- yaml::read_yaml(text = "
variants:
  a:
    when: { format: quarto }
    files: []
")
  expect_error(
    rUM:::.resolve_variant(manifest, list(format = "rmd")),
    "Could not find a matching template variant"
  )
})


# --- .execute_kit() tests ---

test_that(".execute_kit copies files with relative dst paths", {
  scaffold <- create_mock_scaffold()
  target <- withr::local_tempdir()

  kit <- list(
    files = list(
      list(src = "supporting/README.md", dst = "README.md"),
      list(src = "supporting/.gitignore.md", dst = ".gitignore")
    )
  )

  rUM:::.execute_kit(scaffold, kit, target)

  expect_true(file.exists(file.path(target, "README.md")))
  expect_true(file.exists(file.path(target, ".gitignore")))
  expect_equal(readLines(file.path(target, "README.md")), "mock readme")
})

test_that(".execute_kit resolves {path} and custom placeholders", {
  scaffold <- create_mock_scaffold()
  target <- withr::local_tempdir()

  kit <- list(
    files = list(
      list(src = "slides/quarto_slides.qmd", dst = "{path}/presentation.qmd"),
      list(src = "analysis/default_quarto.qmd", dst = "{filename}")
    )
  )

  rUM:::.execute_kit(
    scaffold,
    kit,
    target,
    placeholders = list(filename = "my_doc.qmd")
  )

  expect_true(file.exists(file.path(target, "presentation.qmd")))
  expect_true(file.exists(file.path(target, "my_doc.qmd")))
  expect_equal(readLines(file.path(target, "my_doc.qmd")), "mock default qmd")
})

test_that(".execute_kit creates subdirectories as needed", {
  scaffold <- create_mock_scaffold()
  target <- withr::local_tempdir()

  kit <- list(
    files = list(
      list(src = "supporting/README.md", dst = "vignettes/README.md")
    )
  )

  rUM:::.execute_kit(scaffold, kit, target)

  expect_true(dir.exists(file.path(target, "vignettes")))
  expect_true(file.exists(file.path(target, "vignettes/README.md")))
})


# --- .dispatch_action() tests ---

test_that(".dispatch_action create_dir supports relative and {path} targets", {
  target <- withr::local_tempdir()
  rUM:::.dispatch_action(list(type = "create_dir", path = "data"), target)
  expect_true(dir.exists(file.path(target, "data")))

  sub_target <- file.path(target, "nested_slides")
  rUM:::.dispatch_action(list(type = "create_dir", path = "{path}"), sub_target)
  expect_true(dir.exists(sub_target))
})

test_that(".dispatch_action create_bib creates nested bib directories", {
  target <- withr::local_tempdir()

  rUM:::.dispatch_action(list(type = "create_bib", path = "bib"), target)
  expect_true(file.exists(file.path(target, "bib/packages.bib")))
  expect_true(file.exists(file.path(target, "bib/references.bib")))

  rUM:::.dispatch_action(
    list(type = "create_bib", path = "vignettes/bib"),
    target
  )
  expect_true(file.exists(file.path(target, "vignettes/bib/packages.bib")))
  expect_true(file.exists(file.path(target, "vignettes/bib/references.bib")))
})

test_that(".dispatch_action write_readme and write_scss use scaffold", {
  scaffold <- create_mock_scaffold()
  target <- withr::local_tempdir()
  local_mocked_bindings(.get_rum_scaffold = function() scaffold)

  suppressMessages({
    rUM:::.dispatch_action(list(type = "write_readme"), target)
    rUM:::.dispatch_action(list(type = "write_scss"), target)
  })

  expect_true(file.exists(file.path(target, "README.md")))
  expect_true(file.exists(file.path(target, "custom.scss")))
})

test_that(".dispatch_action unknown type aborts loudly", {
  target <- withr::local_tempdir()
  expect_error(
    rUM:::.dispatch_action(list(type = "nonexistent_action"), target),
    "Unknown action type"
  )
})


# --- .scaffold_from_manifest() & .get_rum_scaffold() tests ---

test_that(".scaffold_from_manifest copies common + variant files and actions", {
  scaffold <- create_mock_scaffold()
  target <- withr::local_tempdir()
  local_mocked_bindings(.get_rum_scaffold = function() scaffold)

  rUM:::.scaffold_from_manifest(
    manifest_name = "test",
    inputs = list(format = "quarto", example = TRUE),
    target_path = target
  )

  expect_true(file.exists(file.path(target, ".gitignore")))
  expect_true(file.exists(file.path(target, "README.md")))
  expect_true(dir.exists(file.path(target, "data")))
  expect_true(file.exists(file.path(target, "analysis.qmd")))
})

test_that(".scaffold_from_manifest works with NULL conditions & placeholders", {
  scaffold <- create_mock_scaffold()
  target <- withr::local_tempdir()
  local_mocked_bindings(.get_rum_scaffold = function() scaffold)

  rUM:::.scaffold_from_manifest(
    manifest_name = "standalone",
    inputs = list(example = NULL),
    target_path = target,
    placeholders = list(filename = "my_analysis.qmd")
  )

  expect_true(file.exists(file.path(target, "my_analysis.qmd")))
  expect_equal(
    readLines(file.path(target, "my_analysis.qmd")),
    "mock default qmd"
  )
})

test_that(".scaffold_from_manifest works with {path} placeholders", {
  scaffold <- create_mock_scaffold()
  base <- withr::local_tempdir()
  target <- file.path(base, "slide_out")
  local_mocked_bindings(.get_rum_scaffold = function() scaffold)

  rUM:::.scaffold_from_manifest(
    manifest_name = "pathed",
    inputs = list(template = "none"),
    target_path = target
  )

  expect_true(dir.exists(target))
  expect_true(file.exists(file.path(target, "presentation.qmd")))
  expect_true(file.exists(file.path(target, "theme.scss")))
})

test_that(".get_rum_scaffold rejects invalid captive-portal zip without NA", {
  cache_env <- rUM:::.rum_scaffold_cache
  old_path <- cache_env$path
  on.exit(cache_env$path <- old_path, add = TRUE)
  cache_env$path <- NULL

  empty_cache <- withr::local_tempdir()
  withr::local_options(list(rUM.cache_dir = empty_cache))
  withr::local_envvar(c(RUM_TEMPLATES_PATH = ""))

  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      writeLines("<html>captive portal</html>", destfile)
      0L
    }
  )

  expect_error(
    rUM:::.get_rum_scaffold(),
    "Downloaded template archive is invalid"
  )
  expect_null(cache_env$path)
})

test_that(".get_rum_scaffold supports RUM_TEMPLATES_PATH and offline cache", {
  cache_env <- rUM:::.rum_scaffold_cache
  old_path <- cache_env$path
  on.exit(cache_env$path <- old_path, add = TRUE)

  scaffold <- create_mock_scaffold()

  # 1) RUM_TEMPLATES_PATH override
  withr::with_envvar(c(RUM_TEMPLATES_PATH = scaffold), {
    cache_env$path <- NULL
    expect_identical(
      rUM:::.get_rum_scaffold(),
      normalizePath(scaffold, mustWork = TRUE)
    )
  })
  withr::with_envvar(c(RUM_TEMPLATES_PATH = file.path(scaffold, "missing")), {
    cache_env$path <- NULL
    expect_error(rUM:::.get_rum_scaffold(), "RUM_TEMPLATES_PATH")
  })

  # 2) Persistent cache populated on success, used offline when download fails
  cache_dir <- withr::local_tempdir()
  zip_src <- tempfile(fileext = ".zip")
  withr::with_dir(dirname(scaffold), {
    utils::zip(zip_src, files = basename(scaffold), flags = "-rq")
  })

  withr::local_options(list(rUM.cache_dir = cache_dir))
  withr::local_envvar(c(RUM_TEMPLATES_PATH = ""))

  cache_env$path <- NULL
  local_mocked_bindings(
    download.file = function(url, destfile, ...) {
      file.copy(zip_src, destfile, overwrite = TRUE)
      0L
    }
  )
  online_path <- rUM:::.get_rum_scaffold()
  expect_true(dir.exists(file.path(online_path, "manifests")))
  expect_true(file.exists(file.path(cache_dir, "templates.zip")))

  cache_env$path <- NULL
  local_mocked_bindings(
    download.file = function(url, destfile, ...) stop("network down")
  )
  expect_message(
    offline_path <- rUM:::.get_rum_scaffold(),
    "Offline: using cached rUM templates"
  )
  expect_true(dir.exists(file.path(offline_path, "manifests")))
})

test_that("manifest helpers reject missing when, missing src, and .. paths", {
  bad_manifest <- list(variants = list(broken = list(files = list())))
  expect_error(
    rUM:::.resolve_variant(bad_manifest, list(format = "quarto")),
    "missing a `when` condition"
  )

  scaffold <- create_mock_scaffold()
  target <- withr::local_tempdir()

  expect_error(
    rUM:::.execute_kit(
      scaffold,
      list(files = list(list(src = "../secret.txt", dst = "out.txt"))),
      target
    ),
    "Unsafe parent traversal"
  )
  expect_error(
    rUM:::.execute_kit(
      scaffold,
      list(files = list(list(src = "supporting/missing.md", dst = "out.md"))),
      target
    ),
    "Could not find template source file"
  )
})

test_that(".execute_kit injects partials and honors overwrite = FALSE", {
  scaffold <- withr::local_tempdir()
  target <- withr::local_tempdir()
  dir.create(file.path(scaffold, "analysis"), recursive = TRUE)
  dir.create(file.path(scaffold, "supporting/partials"), recursive = TRUE)

  writeLines(
    c("---", 'title: "Demo"', "{{knitr_opts}}", "template-version: 1", "---"),
    file.path(scaffold, "analysis/doc.qmd")
  )
  writeLines(
    c("knitr:", "  opts_chunk:", "    echo: false"),
    file.path(scaffold, "supporting/partials/knitr_analysis.yml")
  )
  writeLines("upstream apa", file.path(scaffold, "supporting/apa.csl"))
  writeLines("custom user apa", file.path(target, "apa.csl"))

  kit <- list(
    files = list(
      list(
        src = "analysis/doc.qmd",
        dst = "{filename}",
        inject = list(knitr_opts = "supporting/partials/knitr_analysis.yml")
      ),
      list(src = "supporting/apa.csl", dst = "apa.csl")
    )
  )

  rUM:::.execute_kit(
    scaffold_path = scaffold,
    kit = kit,
    target_path = target,
    placeholders = list(filename = "paper.qmd"),
    overwrite = FALSE
  )

  rendered <- readLines(file.path(target, "paper.qmd"))
  expect_false(any(grepl("knitr_opts", rendered, fixed = TRUE)))
  expect_true("    echo: false" %in% rendered)
  expect_equal(readLines(file.path(target, "apa.csl")), "custom user apa")
})

test_that(".add_rum_Rbuildignore writes agent and decision patterns once", {
  target <- withr::local_tempdir()
  writeLines("^.*\\.Rproj$", file.path(target, ".Rbuildignore"))

  rUM:::.add_rum_Rbuildignore(target)
  rUM:::.add_rum_Rbuildignore(target)

  lines <- readLines(file.path(target, ".Rbuildignore"))
  expect_equal(sum(lines == "^AGENTS\\.md$"), 1L)
  expect_equal(sum(lines == "^CLAUDE\\.md$"), 1L)
  expect_equal(sum(lines == "^GEMINI\\.md$"), 1L)
  expect_equal(sum(lines == "^\\.agents$"), 1L)
  expect_equal(sum(lines == "^DECISIONS\\.md$"), 1L)
  expect_equal(sum(lines == "do_everything_summary\\.RData$"), 1L)
})

test_that("sibling rum_templates manifests resolve all src and inject paths", {
  skip_on_cran()
  sibling <- test_path("..", "..", "..", "rum_templates")
  skip_if_not(dir.exists(file.path(sibling, "manifests")))

  ymls <- list.files(
    file.path(sibling, "manifests"),
    pattern = "\\.yml$",
    full.names = TRUE
  )
  for (yml in ymls) {
    m <- yaml::read_yaml(yml)
    kits <- c(list(m$common), m$variants)
    for (kit in kits) {
      if (is.null(kit) || is.null(kit$files)) next
      for (f in kit$files) {
        expect_true(
          file.exists(file.path(sibling, f$src)),
          info = paste(basename(yml), "missing src:", f$src)
        )
        if (!is.null(f$inject)) {
          for (p in f$inject) {
            expect_true(
              file.exists(file.path(sibling, p)),
              info = paste(basename(yml), "missing inject:", p)
            )
          }
        }
      }
    }
  }
})
