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

#' Get the historical (pre-eBird) MBBS data
#'
#' This function simply loads the csv files in the `historical` data directory
#' as a `data.frame`.
#'
#' @importFrom stringr str_extract
#' @importFrom dplyr bind_rows mutate
#' @importFrom readr read_csv
#' @importFrom lubridate mdy
#' @include config.R
get_historical_data <- function() {
  list.files(config$historical_data_dir) |>
    purrr::map(
      .f = ~ {
        file.path(config$historical_data_dir, .x) |>
          system.file(package = "mbbs")() |>
          readr::read_csv(
            col_types = historical_cols
          ) |>
          dplyr::mutate(
            county = stringr::str_extract(.x, "orange|chatham|durham"),
            date   = lubridate::mdy(date),
            time   = as.character(time)
          )
      }
    ) |>
    dplyr::bind_rows()
}
