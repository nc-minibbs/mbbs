#------------------------------------------------------------------------------#
# Functions for collating the MBBS datasets
#------------------------------------------------------------------------------#

#' Create the stop level dataset
#' 
create_stop_level <- function(config = config) {

  all_ebird <- get_ebird_data()
  stop_ebird <- all_ebird |>
    dplyr::filter(!is.na(stop_num))

  # Waiting for #120
  # stop_xls <- get_stop_level_xls_data()
  # stop_obs <- 

}