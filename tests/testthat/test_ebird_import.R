# Testing functions in
# ebird_import.R


test_that("load_ebird_data runs", {
  testthat::skip_on_ci()

  withr::local_dir("../..")
  expect_no_error(load_ebird_data())
})

test_that("get_ebird_data runs", {
  testthat::skip_on_ci()

  withr::local_dir("../..")
  expect_no_error(get_ebird_data())
})

test_that("get_exclusions runs", {
  testthat::skip_on_ci()

  withr::local_dir("../..")
  expect_no_error(get_exclusions())
})
