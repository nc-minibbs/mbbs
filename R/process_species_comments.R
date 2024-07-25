#------------------------------------------------------------------------------#
# Functions/workflows for processing the species_comments field
# Goal is returns a dataframe with species-route count information
# broken out to stop (eg. each row is species-route-stop)
#------------------------------------------------------------------------------#

#' Process species comments
#' Takes data/mbbs.rda rows that have non NA species_comments
#' and processes the row from a full-route summary of count for each species
#' to break it out by stop_num.
#' @param mbbs mbbs.rda
#' @importFrom dplyr group_by summarize filter anti_join select
#' @importFrom tidyr pivot_longer
#' @returns a df where each row is a count of a species at a stop
process_species_comments <- function(mbbs) {
  stopsmbbs <-
    mbbs %>%
    ungroup() %>%
    prepare_to_process_sp_com()

  # It would be great to modify this to use purrr:map at some point
  # However, this short for loop also works nicely.
  for (i in 1:nrow(stopsmbbs)) {
    stopsmbbs[i, ] <- direct_to_sp_com_function(stopsmbbs[i, ])
  }

  # find rows where the output does not match count.
  catch_errors <-
    stopsmbbs %>%
    group_by(sub_id, common_name, loc, year, species_comments) %>%
    summarize(
      out = sum(
        s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14,
        s15, s16, s17, s18, s19, s20
      ),
      count = count
    ) %>%
    filter(out != count)
  # save error rows for assessment.
  write.csv(catch_errors,
    "inst/extdata/species_comments_errors.csv",
    row.names = FALSE
  )
  # notify user of errors
  cat(c(
    "In processing species_comments:",
    "\n",
    nrow(catch_errors),
    "rows have been removed where the sum of the processed counts",
    "did not match the original ebird checklist's route-total count",
    "for that species",
    "\n",
    "To follow up see:",
    "inst/extdata/species_comments_errors.csv"
  ))

  # remove rows where output doesn't match count
  stopsmbbs <- anti_join(stopsmbbs, catch_errors)

  # pivot s1:s20 into stop_num and give count for each stop
  stopsmbbs <-
    stopsmbbs %>%
    select(-stop_num, -count, -sc_note) %>%
    pivot_longer(
      cols = s1:s20,
      names_to = "stop_num", names_prefix = "s", values_to = "count"
    )

  stopsmbbs # returns
}


#' Prepare mbbs dataset for processing species comments
#' @importFrom dplyr filter mutate bind_cols tibble relocate across
#' @importFrom rlang set_names
#' @importFrom stringr str_detect
#' @param mbbs mbbs.rda
prepare_to_process_sp_com <- \(mbbs) {
  mbbs %>%
    # Keep rows that have non-blank species_comments
    filter(species_comments != "") %>%
    # Keep rows that aren't already separated by stop
    filter(is.na(stop_num)) %>%
    # Keep rows that contain at least one number
    filter(str_detect(species_comments, "[0-9]")) %>%
    # Remove pre-dawn owling checklist
    filter(sub_id != "S46296942") %>%
    mutate(species_comments = fix_species_comments(species_comments)) %>%
    # Remove rows that were changed to have blank species_comments
    filter(species_comments != "") %>%
    dplyr::bind_cols(
      dplyr::tibble(
        !!!rlang::set_names(
          rep(NA_character_, 21),
          c(paste0("s", 1:20), "sc_note")
        )
      )
    ) %>%
    # set s1:s20 to integer columns
    mutate(across(.cols = s1:s20, .fns = as.integer)) %>%
    # relocate helpful for temp and error identification
    relocate(s1:s20, species_comments, count, sc_note)
}



#' Fix species comments
#' Fix unicode errors on enter signs,
#' and known typos within the dataset
#' @param x a character vector of species comments
#' @importFrom stringr str_replace_all
fix_species_comments <- \(x) {
  x %>%
    # fix unicode = errors
    str_replace_all("&#61;| =", "=") %>%
    # convert "Stops=" and "Stops:" to format where commas separate counts at each stop
    str_replace_all("Stop(s)?( )?(=)?(:)?( )?", "") %>%
    # replace tabs with empty string
    str_replace_all("\t", "") %>%
    # fixing specific errors
    str_replace_all(c(
      "1=-,2=0" = "1=0,2=0",
      ",,,,,1,,,,,,,,,,,,,,\t,1" = ",,,,,1,,,,,,,,,,,,,,1",
      "1=0,2=013=1," = "1=0,2=0,3=1,",
      "11=1212=0" = "11=0,12=0",
      "9=0110=0" = "9=1,10=0",
      "4=0.5=" = "4=0,5=",
      "7=28=" = "7=2,8=",
      "1=12=" = "1=1,2=",
      "8=09=" = "8=0,9=",
      "10=011=" = "10=0,11=",
      "; another bird seen just before counting at stop 16" = "",
      " song clearly heard in extensive pines with thinned understory in same habitat and general location where this species has occurred on these censuses for the past 20 years" = "",
      "; singing from trees in grassy field at 35.6383,-79.0035" = "",
      "(stop 3, )?heard at 3rd stop( )?-- it was hard to mistake." = "stop 3",
      "18 were perched at 14 on power lines" = "",
      "This is a breeding bird survey route with 20 stops." = "",
      "including 3 chasing each other!" = "",
      "this was over 10 miles on traveling count" = "",
      "over a 10 mile area" = "",
      "I had 2 additional vireos that I thought sounded like.*" = "",
      "3 white geese or ducks, walking.*" = "",
      "3 hens with 12 young birds!.*" = "",
      "Singing from field on south side of road, heard.*" = "",
      "Nesting in Eastern chimney of 1407 Baptist road" = "",
      "2 adults 5 young" = "",
      "Counted 2 groups of 5 with a couple additional birds" = "",
      "All 8 were roosting on powerline" = "",
      "One Horned Lark singing and walking.*" = "",
      "22 on wire at stop 17 (new school)" = "",
      "flying and twittering at 20" = "st 20",
      "heard at stop 17 on Mann's Chapel Road" = "st 17",
      "One flyover flock.*" = "",
      "Durham Mini Breeding Bird Survey, 20 stops" = "",
      "22 on wire at stop 17 [(]+new school[)]" = "",
      "1=0,2=0,3=0,4=1,5=1,6=1,7=0,8=0,9=1,10=1,11=,12=1,13=0," = "1=0,2=0,3=0,4=1,5=1,6=1,7=0,8=0,9=1,10=1,11=0,12=1,13=0,"
    ))
}


#' Takes a row from stopsmbbs
#' (an mbbs dataset that has gone through prepare_to_process_sp_com),
#' routes it to process the species_comment,
#' and then returns the row with 's1:s20' filled in
#' with the approprate counts
#' @param stopsmbbs_row a row from the mbbs that's gone through the prepare_to_process_sp_com
#'      function, ie: stopsmbbs. MUST have $species_comments, $count, and $s1:s20
#' @importFrom assertthat assert_that
#' @importFrom dplyr mutate
#' @importFrom stringr str_starts
direct_to_sp_com_function <- function(stopsmbbs_row) {
  assertthat::assert_that(
    is.character(stopsmbbs_row$species_comments),
    is.numeric(stopsmbbs_row$count),
    nrow(stopsmbbs_row) == 1
  )

  # pull out just species_comments
  species_comments <- stopsmbbs_row$species_comments

  # set up
  detect_pattern <- \(pattern) str_starts(species_comments, pattern)
  clean_with <- \(f) f(species_comments)
  clean_with_pluscount <- \(f) f(species_comments, stopsmbbs_row$count)

  # route species_comments, get back list of counts at all 20 stops
  stop_counts_list <-
    if (detect_pattern(",|[0-9]+,")) {
      clean_with(sp_com_comma_seperated)
    } else if (detect_pattern("[0-9]+=[0-9]+|st(op)?( )?[0-9]+=[0-9]+")) {
      clean_with(sp_com_stop_equals_count)
    } else if (detect_pattern("st(op)?")) {
      clean_with_pluscount(sp_com_only_stop)
    }

  # add the new list to s1:s20 of stopsmbbs
  snum <- paste0("s", 1:20)
  for (i in 1:length(stop_counts_list)) { # which will always be 20
    stopsmbbs_row <- stopsmbbs_row %>%
      mutate(!!as.name(snum[i]) := stop_counts_list[i])
  }

  stopsmbbs_row # return
}


#' Use when species comments follows ,,,,,,1,,,, format
#' eg. where commas seperate counts at each stop.
#' Takes a string and returns a list of 20 numbers
#' @param x a string from the species_comment column formatted with commas
#' @importFrom stringr str_detect str_split_1 str_replace_all
#' @importFrom assertthat assert_that
sp_com_comma_seperated <- function(x) {
  assertthat::assert_that(
    str_detect(x, ",") # check has commas as format requires
  )

  # split based on commas
  x %>%
    stringr::str_split_1(",") %>%
    str_replace_all(
      c( # Remove extraneous spaces
        " " = "",
        # Convert empty strings 0
        "^$" = "0",
        # Remove the Stops = / Stops:
        "Stop(s)?( )?(=)?(:)?( )?" = "",
        # If it starts with st, drop
        "( )?st(op)?( )?" = ""
      )
    ) %>%
    # make standard list of 20
    # 2 cases of 19 stops - minor dif in birds on either side of quarter route
    # leave as is.
    # Cases with 21st stop where it comes from extra comma after 20th stop
    pad_or_truncate() %>%
    as.integer() %>%
    (\(out) {
      assertthat::assert_that(
        length(out) == 20,
        any(is.na(out)) == FALSE
        # additional checks go here.
      )
      out # return
    })
}

#' Use when species comments follows 1=5 or st 1=5 to stop 1=5 format
#' Takes a string and returns a list of 20 numbers
#' @param x a string from the species_comment column formatted by "stop=count"
#' @importFrom stringr str_split_1 str_detect str_replace_all
#' @importFrom assertthat assert_that
#' @returns out a standardized list of 20 integers, representing the
#'    species count at 20 stops.
sp_com_stop_equals_count <- function(x) {
  assertthat::assert_that(
    str_detect(x, "=") # check has equals as format requires
  )

  # split based , or ;
  x %>%
    stringr::str_split_1(",|;") %>%
    str_replace_all(
      c( # Convert empty strings 0
        "^$" = "0",
        # Remove the Stops = / Stops:
        "Stop(s)?( )?(=)?(:)?( )?" = "",
        # If it starts with st, drop
        "( )?st(op)?( )?" = ""
      )
    ) %>%
    # make standard list of 20
    sp_com_standardize_stops_equals() %>%
    (\(out) {
      assertthat::assert_that(
        length(out) == 20,
        any(is.na(out)) == FALSE
      )
      out # return
    })
}


#' Use when species comment is ie: "Stop 6"
#' The count for that stop is the count for the whole route
#' and the species was not seen at any other stops
#' @param x a string from the species_comment column formatted by "stop=count"
#' @param count an integer, from the count column of the mbbs
#' @importFrom stringr str_count str_extract
#' @importFrom assertthat assert_that
sp_com_only_stop <- function(x, count = -999) {
  assertthat::assert_that(
    # check there's only one number, up to two digits
    str_count(x, "[0-9]([0-9])?") == 1,
    is.numeric(count),
    count != -999
  )

  x %>%
    # remove anything after a comma (ie: descriptive notes)
    str_extract(".*(,)?") %>%
    # extract numbers
    str_extract("[0-9]([0-9])?") %>%
    as.numeric() %>%
    # make standard list of 20
    paste("=", count, sep = "") %>%
    sp_com_standardize_stops_equals() %>%
    (\(out) {
      assertthat::assert_that(
        length(out) == 20,
        any(is.na(out)) == FALSE
        # additional checks go here.
      )
      out # return
    })
}


#' takes a list and either extends it with 0s to the max_length or cuts it down
#' @importFrom assertthat assert_that
#' @param x a list of numbers
#' @returns x either cut down to the max_length or with 0's added to the end to
#'  get it to the max_length
pad_or_truncate <- \(x, max_length = 20) {
  # pad with 0s if less than 20
  pad <- min(length(x), max_length)
  x <- c(x, rep(as.integer(0), max_length - pad))
  # truncate if longer than 20
  x <- x[1:max_length]

  # confirm no errors
  assertthat::assert_that(
    length(x) == 20
  )

  x # return
}


#' takes a list of [0-9]=[0=9] and returns a list of length 20
#' where the first number becomes the index in the list
#' and the second number becomes the value of the list at that index.
#' Indexes not explicitly changed in the list of 20 are set to have value '0'
#' @importFrom stringr str_extract
#' @importFrom assertthat assert_that
#' @param x list of characters formatted as [0-9]+=[0-9]+
sp_com_standardize_stops_equals <- \(x){
  # make standard list of 20
  # we create a temp as some stops may not have been included when the count was 0
  temp_list <- rep(as.integer(0), 20)

  # fill in list of 20
  # for each entry in the list, the start is the stop and end is the count
  for (a in 1:length(x)) {
    stop <- as.integer(str_extract(x[a], "[0-9]+")) # first set of nums
    count <- as.integer(str_extract(x[a], "(?<=\\=)( )?[0-9]+")) # nums after =
    temp_list[stop] <- count
  }
  # re-assign x
  x <- temp_list
  # checks
  assertthat::assert_that(
    length(x) == 20,
    is.integer(x)
  )

  # return
  x
}
