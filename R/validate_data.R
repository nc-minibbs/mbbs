
#' Check that dates are within the study dates
#'
#' @param x a date vector
#' @return a logical vector indicating that the date is between 5/15 and 6/30
#' @export
validate_date_range <- function(x) {
  yrs <- sort(unique((year(x))))
  ranges <- purrr::map(
    .x = yrs,
    .f = ~ interval(ymd(paste0(.x, "-05-15")), ymd(paste0(.x, "-06-30")))
  )
  x %within% ranges
}

#' Check that the submissions have the correct number of stops.
#'
#' @param dt a `data.frame` imported using `import_ebird_data`
#' @return a `data.frame` with observations that have a number of stops *other*
#'   than 1 or 20
#' @importFrom dplyr distinct group_by summarise filter
#' @export
check_number_of_stops <- function(dt) {
  dt %>%
    distinct(sub_id, year, route_num, stop_num) %>%
    group_by(sub_id, year, route_num) %>%
    summarise(
      stops = n()
    ) %>%
    filter(!(stops %in% c(1, 20)))
}

#' Check that the submissions have the correct number of stops.
#'
#' @param dt a `data.frame` imported using `import_ebird_data`
#' @param year a optional year to filter to
#' @return a `data.frame` with observations that have > 1 submission for a route
#'   in  a year
#' @importFrom dplyr distinct group_by summarise filter arrange
#' @export
check_duplicate_submissions <- function(dt, year = NULL) {
  dt %>%
    {
      x <- .
      if (!is.null(year)) {
        x %>% filter(year == !!year)
      } else {
        x
      }
    } %>%
    group_by(year, mbbs_county, route_num, stop_num) %>%
    distinct(sub_id) %>%
    mutate(n = n()) %>%
    arrange(year, mbbs_county, route_num) %>%
    filter(n > 1)
}


check_missing_routes <- function(dt) {
  # TODO
}
