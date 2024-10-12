# tests/testthat/test-cityview.R

library(testthat)
library(rcityviews)

test_that("cityview works with predefined city name", {
  skip_on_cran() # Skip tests that require external services on CRAN
  
  # Assuming "Amsterdam" is a predefined city
  expect_s3_class(cityview(name = "Amsterdam"), "ggplot")
})

test_that("cityview works with arbitrary location name", {
  skip_on_cran()
  
  # Test with an arbitrary location
  expect_s3_class(cityview(name = "Hellebecq, Belgium"), "ggplot")
})

test_that("cityview handles invalid location name gracefully", {
  skip_on_cran()
  
  expect_error(
    cityview(name = "ThisLocationDoesNotExist123"),
    "Geocoding failed: Unable to find coordinates for the provided location."
  )
})

test_that("cityview works with custom geocoding method", {
  skip_on_cran()
  
  # Using "osm" method
  expect_s3_class(cityview(name = "Paris, France", method = "osm"), "ggplot")
  
  # Using another method, e.g., "census" (assuming it's supported)
  # expect_s3_class(cityview(name = "Chicago, USA", method = "census"), "ggplot")
})

test_that("cityview requires either name or coordinates", {
  # Without 'name' and without 'coordinates', it should choose a random city
  expect_s3_class(cityview(), "ggplot")
})

test_that("cityview returns invisible when filename is specified", {
  skip_on_cran()
  
  expect_invisible(cityview(name = "Amsterdam", filename = tempfile(fileext = ".png")))
})

test_that("cityview handles invalid 'zoom' parameter", {
  expect_error(
    cityview(name = "Amsterdam", zoom = -1),
    "argument 'zoom' must be a single number > 0"
  )
})

test_that("cityview handles invalid 'method' parameter", {
  expect_error(
    cityview(name = "Amsterdam", method = 123),
    "argument 'method' must be a single character string"
  )
})
