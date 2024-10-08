#------------------------------------------------------------------------------#
# Functions for collating the MBBS datasets
#------------------------------------------------------------------------------#

#' Create the stop level dataset
#'
create_stop_level_0 <- function(config = config) {
  all_ebird <- get_ebird_data()
  stop_ebird <- all_ebird |>
    dplyr::filter(!is.na(stop_num)) |>
    dplyr::select(-submission, -nobservers, -obs_details, -comments)
  # # Waiting for #120
  # stop_xls <- get_stop_level_xls_data()
  # stop_xls
  # stop_obs <-

  stop_ebird
}

create_stop_level <- function(config = config) {
  df <- create_stop_level_0(config = config)

  yrs_in <- dplyr::distinct(df, year, route, stop_num) |>
    arrange(year, route, stop_num)

  logger::log_trace("Preliminary stop-level data has {nrow(df)} observations.")

  df |>
    # For each year that a route was run,
    # add 0 count for those species that were *not* observed
    # in that route / stop / year.
    tidyr::complete(
      tidyr::nesting(
        year, date, time, county, route, route_num, stop_num, lat, lon
      ),
      tidyr::nesting(common_name, sci_name),
      fill = list(count = 0)
    ) |>
    (\(x) {
      # Check that we didn't add observations for route-stop-years
      # not in the input data.

      # browser()
      yrs_out <- dplyr::distinct(x, year, route, stop_num) |>
        arrange(year, route, stop_num)

      assertthat::assert_that(
        identical(yrs_in, yrs_out),
        msg =
          "create_stop_level added or lost year/route/stops and shouldn't have."
      )

      logger::log_trace(
        "Completing stop-level data added {nrow(x) - nrow(df)} 0 counts."
      )
      x
    })()
}

#' Create the route level dataset
#'
create_route_level_0 <- function(stop_level_data, config = config) {
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
