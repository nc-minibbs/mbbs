# Testing functions in
# process_observers.R

survey_list <- read.csv("test_survey_list.csv", header = TRUE)
test_row <- read.csv("survey_list_test_row.csv", header = TRUE)

test_that("survey_list has no rows with NA observers", {
  na_observer_rows <- survey_list %>%
    dplyr::filter((is.na(obs1) == TRUE &
                      is.na(obs2) == TRUE &
                      is.na(obs3) == TRUE))
  
  expect_no_error(
    assertthat::assert_that(
      nrow(na_observer_rows) == 0,
      sum(is.na(survey_list$standardized_observers) == TRUE) == 0
    )
  )
})

test_that("survey_list handles a new row as expected", {
  
})

#test_that("update_observer_table runs without error", {
#  
#  expect_no_error(
#    update_observer_table(test_row,
#                               config,
#                               save = FALSE))
#})
