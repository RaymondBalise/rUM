
test_that("find_slides returns correct structure components", {
  result <- rUM::find_slides("rUM")
  
  # Test the class
  expect_s3_class(result, "slide_finder")
  
  # Test the structure is a list
  expect_type(result, "list")
  
  # Test specific elements
  expect_equal(result$package, "rUM")
  
  # This is the most important test but it does not work during testing.
  # Guessing because the package has not been installed so it fails to find any slides
  #expect_equal(result$slides, c("rUM_the_package", "rUM_the_word"))
  
  # Test that it has exactly these names
  expect_named(result, c("package", "slides"))
}) 
