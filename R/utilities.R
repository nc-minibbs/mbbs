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
      f("dhrm")
    )
  )
}
