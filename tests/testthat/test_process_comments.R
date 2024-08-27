# Testing functions in
# process_comments.R

habitat_test_1 <- "1M,OB,2M,S,3S,S,4B,B,5B,M,6B,B,7B,B,8M,B,9S,M,10OB,OB,11B,M,12B,M,13B,M,14B,B,15B,B,16B,M,17B,M,18B,B,19O,B,20B,M"

test_that(
  "example1 correctly parses",
  {
    expect_no_error(get_habitat(habitat_test_1))
  }
)
