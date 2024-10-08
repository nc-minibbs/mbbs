#------------------------------------------------------------------------------#
# Functions for collating the MBBS datasets
#------------------------------------------------------------------------------#

#' Create the stop level dataset
#'
create_stop_level <- function(config = config) {

  all_ebird <- get_ebird_data()
  stop_ebird <- all_ebird |>
    dplyr::filter(!is.na(stop_num))

  # # Waiting for #120
  # stop_xls <- get_stop_level_xls_data()
  # stop_xls
  # stop_obs <-
}

#' Create the route level dataset
#'
create_route_level <- function(stop_level_data, config = config) {

  # Get the historical data.
  # NOTE:
  historical <- get_historical_data()

}

#' Conform taxonomy of MBBS data from different sources
#'
#' @param df a dataset with a `common_name` field
#' @param taxonomy data.frame from `get_ebird_taxonomy`
conform_taxonomy <- function(df, taxonomy) {

  assertthat::assert_that(
    all(unique(df$common_name) %in% taxonomy$common_name),
    msg = "df has common_names that taxonomy does not have."
  )

  dplyr::left_join(
    df,
    taxonomy,
    by = c("common_name")
  ) |>
    (\(x){
      assertthat::assert_that(
        nrow(df) == nrow(x),
        msg = "Conforming taxonomy lost data. This is bad."
      )
    })()
}
