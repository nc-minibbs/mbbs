#------------------------------------------------------------------------------#
# MBBS project R utilities
#------------------------------------------------------------------------------#

#' Create a unique route ID for a county/route
#'
#' @importFrom dplyr if_else
#' @include config.R
make_route_id <- function(county, route_num) {
  assertthat::assert_that(
    all(grepl(config$county_pattern, county)),
    msg = paste("County must match", config$county_pattern)
  )

  f <- \(x) paste(x, sprintf("%02d", route_num), sep = "-")

  dplyr::if_else(
    county == "orange",
    f("orng"),
    dplyr::if_else(county == "chatham",
      f("cthm"),
      f("drhm")
    )
  )
}

#' Check that dates are within the study dates
#'
#' @param x a date vector
#' @importFrom lubridate %within% year ymd
#' @return a logical vector indicating that the date is between 5/15 and 6/30
#' @export
valid_date_range <- function(x) {
  yrs <- sort(unique((year(x))))
  ranges <- purrr::map(
    .x = yrs,
    .f = ~ lubridate::interval(
      lubridate::ymd(paste0(.x, "-05-15")),
      lubridate::ymd(paste0(.x, "-06-30")))
  )
  x %within% ranges
}