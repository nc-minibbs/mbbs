# Testing functions in
# process_observers.R

test_that("update_survey_events runs without error", 
          {
            load("data/mbbs.rda")
            expect_no_error(update_survey_events())
          })
