#------------------------------------------------------------------------------#
# Functions for collating the MBBS datasets
#------------------------------------------------------------------------------#

#' Create the stop level dataset
#'
create_stop_level_0 <- function(ebird, taxonomy, config = config) {
  stop_ebird <- ebird |>
    dplyr::filter(!is.na(stop_num)) |>
    dplyr::select(year, route, stop_num, common_name, sci_name, count, county, route_num)


  # browser()
  stop_xls <- get_stop_level_xls_data() |>
    conform_taxonomy(taxonomy)

  stop_obs <- process_obs_details(ebird) |>
    select(year, route, stop_num, common_name, sci_name, count, county, route_num)

  dplyr::bind_rows(
    stop_ebird |> mutate(source = "ebird"),
    stop_xls |> mutate(source = "observer xls"),
    stop_obs |> mutate(source = "obs details")
  ) |>
    # Validity checks
    (\(df) {
      # Check that there are not duplicate entries
      df |>
        dplyr::group_by(year, route, stop_num, common_name) |>
        dplyr::summarise(
          n = dplyr::n(),
          sources = paste(source, collapse = " & ")
        ) |>
        dplyr::filter(n > 1) |>
        (\(x) {
          logger::log_error(
            paste(
              "Route {x$route}-{x$stop_num} has multiple entries",
              "for {x$common_name}",
              "in {x$year} coming from {x$sources}"
            )
          )
        })()
      df
    })()
}

create_stop_level <- function(ebird, taxonomy, config = config) {
  df <- create_stop_level_0(ebird, taxonomy, config = config)

  yrs_in <- dplyr::distinct(df, year, route, stop_num) |>
    arrange(year, route, stop_num)

  logger::log_trace("Preliminary stop-level data has {nrow(df)} observations.")

  df |>
    # For each year that a route was run,
    # add 0 count for those species that were *not* observed
    # in that route / stop / year.
    tidyr::complete(
      tidyr::nesting(
        year, county, route, route_num, stop_num, source
      ),
      tidyr::nesting(common_name, sci_name),
      fill = list(count = 0)
    ) |>
    (\(x) {
      # Check that we didn't add observations for route-stop-years
      # not in the input data.

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
create_route_level_0 <- function(ebird, stop_level_data, taxonomy, config = config) {
  # Get the historical data.
  historical <- get_historical_data() |>
    select(
      year, common_name, route, route_num, county, count
    ) |>
    conform_taxonomy(taxonomy)

  # browser()

  stop_to_route <- stop_level_data |>
    group_by(year, county, route, route_num, common_name, sci_name) |>
    summarise(count = sum(count)) |>
    select(
      year, common_name, sci_name, route, route_num, county, count
    )

  ebird_no_stop <- ebird |>
    dplyr::filter(is.na(stop_num)) |>
    select(
      year, common_name, sci_name, route, route_num, county, count
    )

  dplyr::bind_rows(
    historical |> mutate(source = "historical"),
    stop_to_route |> mutate(source = "stop-level"),
    ebird_no_stop |> mutate(source = "ebird")
  )
}

create_route_level <- function(ebird, stop_level_data, taxonomy, config = config) {
  df <- create_route_level_0(ebird, stop_level_data, taxonomy, config)

  yrs_in <- dplyr::distinct(df, year, route) |> arrange(year, route)

  df |>
    # For each year that a route was run,
    # add 0 count for those species that were *not* observed
    # in that route / stop / year.
    tidyr::complete(
      tidyr::nesting(
        year, county, route, route_num, source
      ),
      tidyr::nesting(common_name, sci_name),
      fill = list(count = 0)
    ) |>
    ## ADD CHECKS
    (\(x) {
      # Check that we didn't add observations for route-stop-years
      # not in the input data.

      yrs_out <- dplyr::distinct(x, year, route) |> arrange(year, route)

      assertthat::assert_that(
        identical(yrs_in, yrs_out),
        msg =
          "create_stop_level added or lost year/route/stops and shouldn't have."
      )

      logger::log_trace(
        "Completing route-level data added {nrow(x) - nrow(df)} 0 counts."
      )
      x
    })()
}

#' Create the MBBS datasets
#'
create_mbbs <- function(config) {
  taxonomy <- get_ebird_taxonomy()
  ebird <- get_ebird_data()

  stop_level <- create_stop_level(ebird, taxonomy, config)
  route_level <- create_route_level(ebird, stop_level, taxonomy, config)

  list(
    stop_level  = stop_level,
    route_level = route_level
  )
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
      x
    })()
}
