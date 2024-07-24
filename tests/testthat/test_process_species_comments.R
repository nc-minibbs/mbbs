# Testing functions in 
# prepare_stop_level_data.R

#test_data <- read.csv("tests/testthat/process_species_comments_test_cases.csv", stringsAsFactors = FALSE)

# setup
test_data <- read.csv("process_species_comments_test_cases.csv", stringsAsFactors = FALSE)

test_that("prepare_to_process simply runs without failure on test data",
 {
   expect_no_error(prepare_to_process_sp_com(test_data))
 }
)

#if passes test, re-format test data for further testing

test_data <- prepare_to_process_sp_com(test_data)

test_that("direct_to_sp_com_function processes all different cases without failure",
  { 
    expect_no_error(direct_to_sp_com_function(test_data[1,]))
    expect_no_error(direct_to_sp_com_function(test_data[2,]))
    expect_no_error(direct_to_sp_com_function(test_data[3,]))
    expect_no_error(direct_to_sp_com_function(test_data[4,]))
    expect_no_error(direct_to_sp_com_function(test_data[5,]))
    expect_no_error(direct_to_sp_com_function(test_data[6,]))
    
    expect_error(sp_com_comma_seperated(c(",,,,,", ",,,")))
    expect_error(sp_com_comma_seperated("1=1"))
    
    expect_error(sp_com_stop_equals_count(",,,,,"))
    expect_error(sp_com_stop_equals_count("19"))

    expect_error(sp_com_only_stop("191", count = 16))
  }
)


test_that("pad_or_truncate runs properly",
  {
    expect_no_error(pad_or_truncate(rep(1,7)))
    expect_no_error(pad_or_truncate(rep(1,30)))
  })
