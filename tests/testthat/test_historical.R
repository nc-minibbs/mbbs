# Testing functions in
# historical.R

test_that("get_historical_data runs", {
  withr::local_dir("../..")
  expect_no_error(get_historical_data())
})
