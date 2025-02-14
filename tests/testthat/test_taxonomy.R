# Testing functions in
# taxonomy.R

test_that("get_ebird_taxonomy runs", {
  skip_on_ci()

  withr::local_dir("../..")
  expect_no_error(get_ebird_taxonomy())
})
