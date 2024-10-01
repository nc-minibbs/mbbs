# Testing functions in
# ebird_commments.R
#
# NOTES
# -----
#
# * test_process_comments.csv was generated on
#   20241001
#   by the following:
# comments <- get_ebird_data() |>
#   distinct(submission, year, route, stop_num, comments) |>
#   select(submission, year, route, stop_num, comments)
# write.csv(comments, "tests/testthat/test_ebird_comments.csv",
#           row.names= FALSE)

## Get test cases

comment_test_cases <- read.csv("tests/testthat/test_ebird_comments.csv")

## Test cleaning step
test_that(
  "clean_comments runs and does what is expected",
  {
    expect_no_error(clean_comments(comment_test_cases$Checklist.Comments))
  }
)

cleaned_comment_test_cases <- comment_test_cases |>
  mutate(Checklist.Comments = clean_comments(Checklist.Comments))


## Test habitat parsing
habitat_test_1 <- "1M,OB,2M,S,3S,S,4B,B,5B,M,6B,B,7B,B,8M,B,9S,M,10OB,OB,11B,M,12B,M,13B,M,14B,B,15B,B,16B,M,17B,M,18B,B,19O,B,20B,M"

test_that(
  "habitat_test_1 correctly parses",
  {
    expect_no_error(get_habitat(habitat_test_1))
  }
)


habitat_comments <- cleaned_comment_test_cases |>
  mutate(
    habitat = extract_habitat(Checklist.Comments)
  ) |>
  filter(!is.na(habitat))