#------------------------------------------------------------------------------#
# R code related to historical MBBS data
# See `docs/data-pipeline.md` for a description of "historical" data.
#------------------------------------------------------------------------------#

#' Column specification for historical data
historical_cols <-
  c(route       = "c",
    date        = "c",
    time        = "c",
    observers   = "c",
    spec_code   = "c",
    common_name = "c",
    count       = "i",
    year        = "i",
    route_num   = "i",
    hab_hm      = "d",
    hab_p       = "d",
    hab_o       = "d",
    hab_b       = "d",
    hab_other   = "d",
    vehicles    = "i")

#' Loads the historical (pre-eBird) MBBS data from disk
#'
#' This function simply loads the csv files
#' in the `historical` data directory
#' as a `data.frame`.
#'
#' @importFrom stringr str_extract
#' @importFrom dplyr bind_rows mutate
#' @importFrom readr read_csv
#' @importFrom lubridate mdy
#' @include config.R
load_historical_data <- function() {
  list.files(config$historical_data_dir) |>
    purrr::map(
      .f = ~ {
        file.path(config$historical_data_dir, .x) |>
          # system.file(package = "mbbs")() |>
          readr::read_csv(
            col_types = historical_cols
          ) |>
          dplyr::mutate(
            county = stringr::str_extract(.x, config$county_pattern)
          )
      }
    ) |>
    dplyr::bind_rows()
}

#' Transform historical (pre-eBird) data
#'
#' @importFrom dplyr select starts_with
#' @include utilities.R
transform_historical_data <- function(historical) {
  historical |>
    dplyr::select(
      # route field in source data is missing for some routes
      # route id is created from county and route_num below
      - "route",
    ) |>
    dplyr::mutate(
      route = make_route_id(county, route_num),
      date   = lubridate::mdy(date),
      time   = as.character(time)
    )
}

#' Get the historical (pre-eBird) data
#'
get_historical_data <- function() {
  load_historical_data() |>
    transform_historical_data() |>
    dplyr::select(
      # Drop the habitat data available for (some) routes in historical data.
      # See: https://github.com/nc-minibbs/mbbs/issues/103
      -dplyr::starts_with("hab")
    )
}
