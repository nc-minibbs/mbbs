#
# WIP!
#

# if stop_level_species_comments needs to be updated:
# load("data/mbbs.rda")
# process_species_comments(mbbs)

# if stop_level_hist_xls.csv needs to be updated:
# hist_xls_purrr_directories() %>%
#   write.csv(file = "inst/extdata/stop_level_hist_xls.csv", row.names = FALSE)

# if transcribed files needs to be updated:
# update_transcribed_paper()


#' Replaces mbbs route-level data with the more granular stop-level information
#' when available.
#' Works with any mbbs dataset, either the whole mbbs or one county.
#' @importFrom dplyr bind_rows
#' @param mbbs Any mbbs dataset, either the whole survey area or one county
granulate_to_stop <- function(mbbs) {
  # is this function name a little silly? yes hehe! Also specific

  # read in stop-level data sourced from species_comments, hist_xls, and transcribed_paper
  species_comments <- read.csv("inst/extdata/stop_level_species_comments.csv") # contains 0s
  hist_xls <- read.csv("inst/extdata/stop_level_hist_xls.csv") # contains 0s
  transcribed_paper <- read.csv("inst/extdata/stop_level_transcribed_paper.csv") # does NOT contain 0s

  # bind together stop_level data sourced from inst/
  stop_level_external <- bind_rows(hist_xls, species_comments, transcribed_paper) %>%
    mbbs_generate_route_ID()

  routes_with_ext_stop_level <- stop_level_external %>%
    group_by(mbbs_county, route_num, year) %>%
    summarize() %>%
    group_by(mbbs_county, route_num) %>%
    summarize(num_years = n())

  # add in stop_num = 1 for routes that have all stops aside from stop 1 and have 20 records for the route
  # one way to filter for this may be to group by route_ID, year and look for routes where EITHER and stop_num is not 0 or where any common_name appears more than once. These will be routes with stop_level information.
  # stop_level_internal <- mbbs %>%
  #   function to add stop_num where stop_num == 1 is missing
  #   then filter to all rows where !is.na(stop_num)
  #   and that should give all the mbbs rows with stop level information.

  # compile and compare to current mbbs routes - do counts match?
  # where counts DONT match, have .csv where that decision is recorded, and correct one way or another. Usually should default to the starting route-level mbbs. At the least check mbbs$source to see where error is
  # where counts DO match, replace route-level information with stop information in the overall mbbs/mbbs_county dataset that this function is given. mbbs$source == "granulated to stop"
  # add in the stop num = 1 for routes that have all 19 stops aside from stop 1 and have 20 records for the route (like, within a species.)
  beep()
  return(stopsmbbs)
}

#' Runs process_species_comments and saves the data as a .csv
run_process_species_comments <- function() {
  # load in mbbs
  load("data/mbbs.rda")

  species_comments <- process_species_comments(mbbs)

  write.csv(species_comments, "inst/extdata/stop_level_species_comments.csv", row.names = FALSE)
}
