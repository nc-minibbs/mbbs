#
# WIP!
#


#' Replaces mbbs route-level data with the more granular stop-level information
#' when available.
#' Works with any mbbs dataset, either the whole mbbs or one county.
#' @importFrom dplyr bind_rows
#' @importFrom beepr beep
#' @param mbbs Any mbbs dataset, either the whole survey area or one county
granulate_to_stop <- function(mbbs) {
  # is this function name a little silly? yes hehe! Also specific
  print("This function takes a minute to run. Listen for a beep when it's done.")
  stopsmbbs <- bind_rows(
    process_species_comments(mbbs), # get information from routes with stop info from species_comments
    hist_xls_purrr_directories()
  ) # get information from the extdata/stop_level_data files
  # get information from routes with stop info in checklist notes (there's at least one) (add info to that df extracted from when information is by quarter route in notes)
  # get information from the transcription work datasets
  # read in files that end in paper_files.csv, won't have to change that when final file comes in

  # compile and compare to current mbbs routes - do counts match?
  # where counts DONT match, have .csv where that decision is recorded, and correct one way or another. Usually should default to the starting route-level mbbs. At the least check mbbs$source to see where error is
  # where counts DO match, replace route-level information with stop information in the overall mbbs/mbbs_county dataset that this function is given. mbbs$source == "granulated to stop"
  # add in the stop num = 1 for routes that have all 19 stops aside from stop 1 and have 20 records for the route (like, within a species.)
  beep()
  return(stopsmbbs)
}
