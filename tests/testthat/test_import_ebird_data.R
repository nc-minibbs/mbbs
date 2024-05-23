# Testing functions in
# import_ebird_data.R


test_that("get_exclusions runs",
  {
    x <- system.file("excluded_submissions.yml", package = "mbbs")
    expect_no_error(get_exclusions(x))
  }
)