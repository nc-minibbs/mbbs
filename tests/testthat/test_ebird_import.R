# Testing functions in
# ebird_import.R


test_that("load_ebird_data runs", {
  expect_no_error(load_ebird_data(x))
})

test_that("get_ebird_data runs", {
  expect_no_error(get_ebird_data(x))
})

test_that("get_exclusions runs", {
  x <- system.file("excluded_submissions.yml", package = "mbbs")
  expect_no_error(get_exclusions(x))
})
