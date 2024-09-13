# Testing functions in
# process_observers.R

# these tests work in the context of the package, but not when
# automatically run by testthat

test_that("update_survey_events runs without error", {
  expect_no_error(update_survey_events())
})

test_that("survey_list has no rows with NA observers", {
  survey_list <- read.csv(system.file("extdata/survey_list.csv", package = "mbbs"), header = TRUE)
  expect_no_error(
    assertthat::assert_that(
      sum(is.na(survey_list$observers) == TRUE) == 0
    )
  )
})

test_that("mbbs_survey_events has no rows with NA observers", {
  load(system.file("data/mbbs_survey_events.rda", package = "mbbs"))
  expect_no_error(
    assertthat::assert_that(
      sum(is.na(mbbs_survey_events$observers) == TRUE) == 0
    )
  )
  # also check standardized_observers
  expect_no_error(
    assertthat::assert_that(
      sum(is.na(mbbs_survey_events$standardized_observers) == TRUE) == 0
    )
  )
})

test_that("update_mini_observer_table runs without error", {
  expect_no_error(update_mini_observer_table())
})


test_that("update_observer_table runs without error (error is not in the mini_table update)", {
  mbbs_orange <- read.csv(system.file("mbbs_orange_update_observer_table_testdata.csv", package = "mbbs"))
  expect_no_error(update_observer_tables(mbbs_orange, "orange"))
})
