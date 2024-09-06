# Testing functions in
# process_comments.R
#
# NOTES
# -----
#
# * test_process_comments.csv was generated on
#   20240809
#   by taking the union of all the unique checklist comments from
#   'inst/extdata/MyEBirdData_Orange_20240730.csv'
#   'inst/extdata/MyEBirdData_Durham_20240730.csv'
#   'inst/extdata/MyEBirdData_Chatham_20240730.csv'
#
# The following code can be adapted to update/modify the process:
# get_comments <- \(county, date) {
#   glue::glue("inst/extdata/MyEBirdData_{county}_{date}.csv",
#               county = county,
#               date = date) |>
#     read.csv() |>
#     dplyr::distinct(Submission.ID, Location, Date, Checklist.Comments) |>
#     dplyr::filter(Checklist.Comments != "") |>
#     dplyr::mutate(
#       route = parse_route_from_location(Location),
#       stop = parse_stop_from_location(Location)
#     )
# }
# comments <-
#   dplyr::bind_rows(
#     get_comments("Chatham", "20240730"),
#     get_comments("Durham", "20240730"),
#     get_comments("Orange", "20240730"))

# write.csv(comments, "tests/testthat/test_process_comments.csv",
#           row.names= FALSE)

## Get test cases

comment_test_cases <- read.csv("tests/testthat/test_process_comments.csv")

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
