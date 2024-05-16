# Testing functions in 
# prepare_stop_level_data.R

# setup
test_data <- read.csv("process_species_comments_test_cases.csv", stringsAsFactors = FALSE)

test_that("prepare_to_process simply runs without failure on test data",
 {
   expect_no_error(prepare_to_process(test_data))
 }
)