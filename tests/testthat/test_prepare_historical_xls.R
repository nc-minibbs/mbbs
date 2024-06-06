# Testing functions in
# prepare_historical_xls.R

test_that("checks are working as expected", {
  x <- read.csv("prepare_hist_xls_test_cases.csv", header = TRUE)

  #error with reading excel file
  expect_error(hist_xls_run_checks(x[is.na(x$common_name),]))
  expect_error(hist_xls_run_checks(x[is.na(x$stop_num),]))
  expect_error(hist_xls_run_checks(x[is.na(x$count),]))
  
  #errors with hist_xls_extract_county_num_year_from_filename
  expect_error(hist_xls_run_checks(x[is.na(x$mbbs_county),]))
  expect_error(hist_xls_run_checks(x[is.na(x$year),]))
  expect_error(hist_xls_run_checks(x[is.na(x$route_num),]))
  
  #errors with hist_xls_add_species_and_route_info
  expect_error(hist_xls_run_checks(x[is.na(x$route_ID),]))
  expect_error(hist_xls_run_checks(x[is.na(x$sci_name),]))
  expect_no_error(hist_xls_sci_name_check(x[42,])) #Acc sp.
  
  #check that each species has 20 entries
  expect_error(hist_xls_run_checks(x[21:41,]))
  indexes <- c(21:40, 43:62)
  expect_no_error(hist_xls_run_checks(x[indexes,]))
})


