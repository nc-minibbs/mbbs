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

#' UnSpecific common_name exclusions
unspecific_common_names <-
 c("waterfowl sp.",
   "crow sp.",
   "swallow sp.",
   "hawk sp.",
   "Accipiter sp.",
   "Accipiter species",
   "duck sp.",
   "woodpecker sp.",
   "Buteo sp.",
   "passerine sp."
  )

#' Removes non specific observations
exclude_nonspecific_obs <- function(df, source) {
  out <- df |>
    dplyr::filter(
      !(.data$common_name %in% unspecific_common_names)
    )

  n <- nrow(df) - nrow(out)
  if (n > 0) {
    logger::log_info(
      glue::glue("{n} {source} observations were removed due to lack of species specificity.")
    )
  }

  out
}
