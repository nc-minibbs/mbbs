# Testing functions in
# taxonomy.R

test_that("get_ebird_taxonomy runs", {
  expect_no_error(get_ebird_taxonomy())
})
