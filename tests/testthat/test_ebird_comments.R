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
#   filter(!is.na(comments)) |>
#   select(submission, year, route, stop_num, comments)
# write.csv(comments, "tests/testthat/test_ebird_comments.csv",
#           row.names= FALSE)

## Get test cases
comment_test_cases <- read.csv("tests/testthat/test_ebird_comments.csv")

x <- split_comment_cases(comment_test_cases)
route_comments <- x[[1]]
stop_comments <- x[[2]]

## Test cleaning step
test_that(
  "clean_comments runs and does what is expected",
  {
    expect_no_error(clean_comments(comment_test_cases$comments))
  }
)

test_that(
  "parse_stop_comments does something",
  {
    expect_no_error(parse_stop_comments(stop_comments$comments))
  }
)
