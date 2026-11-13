test_that("find_slides discovers bundled rUM slide decks", {
  res <- find_slides("rUM")
  expect_s3_class(res, "slide_finder")
  expect_identical(res$package, "rUM")
  expect_true(all(c("rUM_the_package", "rUM_the_word") %in% res$slides))
})

test_that("find_slides errors cleanly when package is missing or has none", {
  expect_error(
    find_slides("nonexistent_rum_pkg_xyz"),
    "there is no package called"
  )
  expect_error(
    find_slides("yaml"),
    "No slides found for package: yaml"
  )
})

test_that("find_slides validates package argument", {
  expect_error(find_slides(NULL), "Invalid NULL package")
  expect_error(find_slides(123), "must be a character")
})

test_that("show_slides validates interactive mode, args, menu, and preview", {
  rlang::local_interactive(FALSE)
  expect_error(
    show_slides("rUM", "rUM_the_package"),
    "interactive session"
  )

  previewed <- character(0)
  menu_choice <- 1L
  rlang::local_interactive(TRUE)
  local_mocked_bindings(
    quarto_preview = function(file, ...) {
      previewed <<- c(previewed, file)
      invisible(file)
    },
    menu = function(choices, title = NULL) menu_choice
  )

  expect_error(show_slides(NULL), "Invalid NULL package")
  expect_error(show_slides(42), "must be a character")

  # Direct call strips .qmd suffix and previews deck
  suppressMessages(show_slides("rUM", "rUM_the_package.qmd"))
  expect_length(previewed, 1L)
  expect_match(previewed[1], "rUM_the_package\\.qmd$")

  # Piped slide_finder with menu selection 1
  menu_choice <- 1L
  suppressMessages(find_slides("rUM") |> show_slides())
  expect_length(previewed, 2L)

  # Piped slide_finder with menu cancel (0)
  menu_choice <- 0L
  expect_message(
    res <- find_slides("rUM") |> show_slides(),
    "No slide selected"
  )
  expect_null(res)

  # Single-deck slide_finder auto-selects without prompting
  one_slide <- structure(
    list(package = "rUM", slides = "rUM_the_word"),
    class = "slide_finder"
  )
  suppressMessages(show_slides(one_slide))
  expect_match(previewed[3], "rUM_the_word\\.qmd$")

  # Empty slide_finder errors cleanly
  no_slides <- structure(
    list(package = "rUM", slides = character(0)),
    class = "slide_finder"
  )
  expect_error(show_slides(no_slides), "No reveal\\.js Quarto slide deck")

  # Invalid deck name errors cleanly
  expect_error(
    show_slides("rUM", "nonexistent_deck_xyz"),
    "not found in package"
  )
})

test_that("find_slides and show_slides handle nested dirs & read-only libs", {
  skip_on_os("windows")
  fake_lib <- withr::local_tempdir()
  fake_pkg <- file.path(fake_lib, "mockpkg")
  nested_dir <- file.path(fake_pkg, "slides", "week1")
  dir.create(nested_dir, recursive = TRUE)
  writeLines(
    c("Package: mockpkg", "Title: Mock", "Version: 0.1.0"),
    file.path(fake_pkg, "DESCRIPTION")
  )
  writeLines(
    c("---", "format: revealjs", "---", "# Nested Talk"),
    file.path(nested_dir, "lec_1.1.qmd")
  )
  Sys.chmod(nested_dir, mode = "0555")
  Sys.chmod(file.path(fake_pkg, "slides"), mode = "0555")
  withr::defer({
    Sys.chmod(file.path(fake_pkg, "slides"), mode = "0755")
    Sys.chmod(nested_dir, mode = "0755")
  })

  withr::local_libpaths(fake_lib, action = "prefix")

  previewed_path <- NULL
  rlang::local_interactive(TRUE)
  local_mocked_bindings(
    quarto_preview = function(file, ...) {
      previewed_path <<- file
      invisible(file)
    }
  )

  found <- find_slides("mockpkg")
  expect_identical(found$slides, "week1/lec_1.1")

  suppressMessages(show_slides(found))
  expect_true(file.exists(previewed_path))
  expect_equal(unname(file.access(dirname(previewed_path), mode = 2)), 0L)
})
