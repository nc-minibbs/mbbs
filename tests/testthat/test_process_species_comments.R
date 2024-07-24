# Testing functions in 
# prepare_stop_level_data.R

# setup
test_data <- read.csv("process_species_comments_test_cases.csv", stringsAsFactors = FALSE)

test_that("prepare_to_process simply runs without failure on test data",
 {
   expect_no_error(prepare_to_process(test_data))
 }
)

test_that("process_comment works on test cases",
  { 
    
    expect_error(process_comment(c(",,,,", ",,,")))
    expect_error(process_comment(",,,,,,,,"))
    expect_length(process_comment(",,,,,,,,,,,,,,,,,,,"), 20)
    # process_comment("Stops: 1,2,3,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,20")
    # process_comment("st 1=1, st 2=2, st 3=3, st 4=4, st 20=20")
    # process_comment("st 20")
    # process_comment("1,2,3,4,,,,,,,,,,,,,,,,20")
    # process_comment("1=1,2=2,3=3,4=4,20=20")

  }
)


test_that("pad_or_truncate runs properly",
  {
    expect_no_error(pad_or_truncate(rep(1,7)))
    expect_no_error(pad_or_truncate(rep(1,30)))
  })
