# Testing functions in
# process_observers.R

# set up test for

# test for survey_list to test if a new survey, eg. fake Chatham 1990 rt 1 can be
# id'd as not being on the list

# test for survey_list to test if an updated survey, eg. Durham 4 2002 with a new
# species of bird added so S +1 and N + 90 works and prompts an ask for information
# can prompts happen in testing?
# similarly needs a save = FALSE
# for a save = TESTING where then the file is saved in the test location
# and can be checked (automatically) for the presence of updated data.

UpSurEv_tcs <- read.csv("tests/testthat/update_survey_events_test_cases.csv", header = TRUE)
