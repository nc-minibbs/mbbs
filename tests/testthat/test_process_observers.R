# Testing functions in
# process_observers.R

test_that("survey_list has no rows with NA observers", {
  survey_list <- read.csv(system.file("extdata/survey_list.csv", package = "mbbs"), header = TRUE)
  expect_no_error(
    assertthat::assert_that(
      sum(is.na(survey_list$observers) == TRUE) == 0
    )
  )
})

test_that("survey_list has only one entry for every route-year", {
  survey_list <- read.csv(system.file("extdata/survey_list.csv", package = "mbbs"), header = TRUE)
  survey_list <- 
    survey_list %>%
    group_by(mbbs_county, route_num, year) %>%
    summarize(N = n())
  expect_no_error(
    assertthat::assert_that(
      sum(survey_list$N) == nrow(survey_list)
    )
  )
})

# load in test cases
mbbs_survey_events <- read.csv("update_survey_events_test_cases.csv", header = TRUE)

test_that("mbbs_survey_events has no rows with NA observers", {
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

test_that("mbbs_survey_events has one record per route-year," {
  check <-
    mbbs_survey_events %>%
    group_by(mbbs_county, route_num, year) %>%
    summarize(N = n())
  expect_no_error(
    assertthat::assert_that(
      sum(check$N) == nrow(check) #no N can be greater than 1
    )
  )
})

test_that("update_mini_observer_table runs without error", {
  expect_no_error(update_mini_observer_table(save = FALSE))
})


test_that("update_observer_table runs without error", {
  mbbs_orange <- mbbs::mbbs_orange
  expect_no_error(update_observer_table(mbbs_orange, "orange", save = FALSE))
})
