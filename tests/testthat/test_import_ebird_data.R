# Testing functions in
# import_ebird_data.R


test_that("get_exclusions runs",
  {
    expect_no_error(get_exclusions("../../inst/excluded_submissions.yml"))
  }
)