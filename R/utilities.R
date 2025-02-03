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
  yrs <- sort(unique((lubridate::year(x))))
  ranges <- purrr::map(
    .x = yrs,
    .f = ~ lubridate::interval(
      lubridate::ymd(paste0(.x, "-05-15")),
      lubridate::ymd(paste0(.x, "-06-30"))
    )
  )
  x %within% ranges
}

#' Conform taxonomy of MBBS data from different sources
#'
#' @param df a dataset with a `common_name` field
#' @param taxonomy data.frame from `get_ebird_taxonomy`
conform_taxonomy <- function(df, taxonomy) {

  df <-
   df |>
   mutate(
    common_name = case_when(
      common_name == "House Wren" ~ "Northern House Wren",
      common_name == "Accipiter sp." ~ "Sharp-shinned/Cooper's Hawk",
      TRUE ~ common_name
    )
   )

  diff <- setdiff(unique(df$common_name), taxonomy$common_name)

  assertthat::assert_that(
    all(unique(df$common_name) %in% taxonomy$common_name),
    msg = paste(paste(diff, collapse = " "),
        "are in df but not in taxonomy")
  )

  dplyr::left_join(
    df,
    taxonomy,
    by = c("common_name")
  ) |>
    (\(x){
      assertthat::assert_that(
        nrow(df) == nrow(x),
        msg = "Data was lost when conforming taxonomy This is bad."
      )
      x
    })()
}
