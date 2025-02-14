# Testing functions in
# historical.R

test_that("get_historical_data runs", {
  testthat::skip_on_ci()

  withr::local_dir("../..")
  expect_no_error(get_historical_data())
})
