#------------------------------------------------------------------------------#
# Functions/workflows for processing the obs_details field
# Goal is returns a dataframe with species-route count information
# broken out to stop (eg. each row is species-route-stop)
#
# NOTE: the purpose of this functionality is to extract stop-level data
#       available in ebird observation details for submissions
#       prior to 2022
#------------------------------------------------------------------------------#

#' Process species comments
#' Takes data/mbbs.rda rows that have non NA species_comments
#' and processes the row from a full-route summary of count for each species
#' to break it out by stop_num.
#' @param ebird ebird data
#' @importFrom dplyr group_by summarize filter anti_join select
#' @importFrom tidyr pivot_longer
#' @returns a df where each row is a count of a species at a stop
process_obs_details <- function(ebird) {
  obs <- 
    ebird |>
    ungroup() |>
    prep_obs_details_data()

  obs |>
    mutate(
      stop_data =
        stringr::str_split(obs$obs_details, ",|;") |>
        (\(x) {
          purrr::pmap(
            .l = list(x, count = obs$count,
                      subid = obs$submission,
                      cname = obs$common_name),
            function(x, count, subid, cname) {
                str_replace_all(x, c("stop" = "", "st" = "", "\\(\\)" = "")) |>
                trimws() |>
                pad_or_truncate(subid = subid, common_name = cname) |>
                (\(x) {
                  tibble(
                    stop_num = 1:20,
                    count = `if`(
                      length(x) == 20,
                      parse_lengtheq20(x),
                      parse_lengthlt20(x, count)
                    )
                  )
                })()

            
            }
          )
        })()
    )
}

#' Prepare ebird dataset for processing observation comments
#' @importFrom dplyr filter mutate bind_cols tibble relocate across
#' @importFrom stringr str_detect
#' @param ebird ebird data
prep_obs_details_data <- \(ebird) {
  ebird |>
    # Keep rows that contain at least one number in observation details
    #           AND aren't already separated by stop
    filter(str_detect(obs_details, "[0-9]") & is.na(stop_num)) |>
    mutate(obs_details = fix_species_comments(obs_details)) |>
    # Remove rows that were changed to have blank species_comments
    filter(obs_details != "")
}

#' Fix species comments
#' Fix unicode errors on enter signs,
#' and known typos within the dataset
#' @param x a character vector of observation details
#' @importFrom stringr str_replace_all
fix_species_comments <- \(x) {
  x |>
    str_replace_all(
      c(
        # fix unicode = errors
        "&#61;| =" = "=",
        # convert "Stops=" and "Stops:" to format
        # where commas separate counts at each stop
        "Stop(s)?( )?(=)?(:)?( )?" = "",
        # replace tabs with empty string
        "\t" = ""
      )
    ) |>
    # fix specific errors
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
      "22 on wire at stop 17 \\(new school\\)" = "",
      "flying and twittering at 20" = "st 20",
      "heard at stop 17 on Mann's Chapel Road" = "st 17",
      "One flyover flock.*" = "",
      "Durham Mini Breeding Bird Survey, 20 stops" = "",
      "22 on wire at stop 17 [(]+new school[)]" = "",
      "1=0,2=0,3=0,4=1,5=1,6=1,7=0,8=0,9=1,10=1,11=,12=1,13=0," = "1=0,2=0,3=0,4=1,5=1,6=1,7=0,8=0,9=1,10=1,11=0,12=1,13=0,",
      "roosting on cell tower" = "",
      "\\(flyover\\)" = "",
      "on Mann's Chapel Road, heard" = "",
      "flying and twittering at" = "",
      "12=0113=0" = "12=0,13=0"
    ))
}

#' takes a list and either extends it with 0s to the max_length or cuts it down
#' @importFrom assertthat assert_that
#' @param x a list of numbers
#' @param max_length the final length that list x should be.
#' @returns x either cut down to the max_length or with 0's added to the end to
#'  get it to the max_length
pad_or_truncate <- \(x, subid, common_name) {
  `if`(
    length(x) == 19,
    { logger::log_warn("{subid} {common_name}: observation details parsed to length == 19.")
      c(x, "")},
    `if`(
      length(x) > 20,
      { logger::log_error("{subid} {common_name}: observation details parsed to length > 20.")
        x[1:20]
      },
      x
    )
  )
}

#' When obs_details splits into a vector of length 20,
#' we assume each position corresponds to a stop (in the correct order)
#' and extract the count accordingly.
parse_lengtheq20 <- \(x) {
  stringr::str_replace_all(x, pattern = "^$", "0") |>
    stringr::str_extract("(?<=\\d{1,3}\\=|^)\\d{1,3}$") |>
    as.integer()
}

#' When obs_details splits into a vector of length less than 20
#' (after `pad_or_truncate`),
#' they are 2 cases to handle:
#' (a) the number in the field corresponds to the *only* stop
#'     at which the count was observed;
#'     in this case, we obtain the count from the route level data.
#' (b) a pattern of i=#,j=#, etc,
#'     where the LHS of "=" the stop numbers
#'     and the RHS is the count
parse_lengthlt20 <- \(x, count, subid) {

  extract_stops <- \(x){
    stringr::str_extract(x, "\\d{1,3}(?=\\s{0,1}\\=)")
  }

  extract_counts <- \(x){
    stringr::str_extract(x, "(?<=\\d{1,3}\\s{0,1}\\=)\\d{1,3}$")
  }

  # Remove any empty strings
  stringr::str_subset(x, pattern = "^$", negate = TRUE) |>
    (\(x) {
      counts <- rep(0, 20)

      `if`(
        length(x) == 1 && all(stringr::str_detect(x, "=", negate = TRUE)),
        # case (a)
        counts[as.integer(x)] <- count,
        # case (b)
        counts[as.integer(extract_stops(x))] <- as.integer(extract_counts(x))
      )

      counts
    })()
}
