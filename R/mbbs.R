#------------------------------------------------------------------------------#
# Functions for collating the MBBS datasets
#------------------------------------------------------------------------------#

#' Create the stop level dataset (initial step)
#'
#' @param ebird ebird dataset resulting from `get_ebird_data`
#' @param taxonomy taxonomy dataset
#' @param config mbbs configuration
create_stop_level_counts_0 <- function(ebird, taxonomy, config = config) {
  stop_ebird <- ebird |>
    dplyr::filter(!is.na(stop_num)) |>
    dplyr::select(year, route, stop_num,
                  common_name, sci_name, count,
                  county, route_num)

  stop_xls <- get_stop_level_xls_data() |>
    conform_taxonomy(taxonomy)

  stop_obs <- process_obs_details(ebird) |>
    select(year, route, common_name, sci_name, county, route_num, stop_data) |>
    tidyr::unnest(cols = stop_data)

  stop_transcribed <- get_stop_level_transcribed() |>
    select(year, route, stop_num,
           common_name, sci_name, count,
           county, route_num, source)

  logger::log_trace("Combining stop level data.")

  dplyr::bind_rows(
    stop_ebird |> mutate(source = "ebird"),
    stop_xls |> mutate(source = "observer xls"),
    stop_obs |> mutate(source = "obs details"),
    stop_transcribed
  ) |>
    # Validity checks
    (\(df) {
      assertthat::assert_that(
        !anyNA(df$stop_num),
        msg = "Stop data has NA values in stop_num."
      )

      # Check that there are not duplicate entries with different counts
      df |>
        dplyr::group_by(year, route, stop_num, common_name) |>
        dplyr::summarise(
          n = dplyr::n(),
          distinctcounts = n_distinct(count),
          sources = paste(source, collapse = " & ")
        ) |>
        dplyr::filter(n > 1 & distinctcounts > 1) |>
        (\(x) {
          logger::log_error(
            paste(
              "Route {x$route}-{x$stop_num} has multiple entries",
              "with differing counts",
              "for {x$common_name}",
              "in {x$year} coming from {x$sources}"
            )
          )
        })()

      # Log number of duplicate entries with the same count
      df |>
        dplyr::distinct(year, route, stop_num, common_name, count,
          .keep_all = TRUE
        ) |>
        (\(x) {
          logger::log_info(
            paste(
              "Stop-level data contains",
              "{nrow(df) - nrow(x)} duplicated observations",
              "(same count, different sources)"
            )
          )
        })()

      df
    })()
}

#' Main function to create stop level count dataset
#'
#' @inheritParams create_stop_level_0
create_stop_level_counts <- function(ebird, taxonomy, config = config) {
  df <- create_stop_level_counts_0(ebird, taxonomy, config = config)

  yrs_in <- dplyr::distinct(df, year, route, stop_num) |>
    arrange(year, route, stop_num)

  logger::log_trace("Preliminary stop-level data has {nrow(df)} observations.")

  # remove duplicated observations
  df <- df |>
    dplyr::distinct(year, route, stop_num, common_name, count,
      .keep_all = TRUE
    )

  logger::log_trace("Stop-level duplicated observations removed")

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

      yrs_out <- 
        dplyr::distinct(x, year, route, stop_num) |>
        arrange(year, route, stop_num)

      assertthat::assert_that(
        identical(yrs_in, yrs_out),
        msg =
          "create_stop_level added or lost year/route/stops and shouldn't have."
      )

      logger::log_trace(
        paste("Completing stop-level data added",
              "{nrow(x) - nrow(df)}",
              "observations with count of 0.")
      )

      x
    })()
}

#' Create the route level dataset (initial step)
#'
#' @param stop_level_data result of `create_stop_level`
#' @inheritParams create_stop_level_0
create_route_level_counts_0 <- function(ebird, stop_level_data, taxonomy, config = config) {
  logger::log_trace("Computing route level data from stop level")
  stop_to_route <- stop_level_data |>
    group_by(year, county, route, route_num, common_name, sci_name, source) |>
    summarise(count = sum(count)) |>
    select(
      year, common_name, sci_name, route, route_num, county, count, source
    )

  # Compare stop_level and ebird counts
  dplyr::left_join(
    stop_to_route |> dplyr::ungroup() |>
      dplyr::select(common_name, year, route, scount = count, source),
    ebird |> dplyr::filter(is.na(stop_num)) |> dplyr::ungroup() |>
      dplyr::select(common_name, year, route, rcount = count),
    by = c("common_name", "year", "route")
  ) |>
    dplyr::mutate(diff = scount - rcount) |>
    (\(x) {
      logger::log_trace(
        paste(
          "Comparing {nrow(x)}",
          "stop-to-route observations with ebird (without stops)"
        )
      )
      x
    })() |>
    dplyr::filter(diff != 0) |>
    (\(x) {
      if (nrow(x) > 0) {
        logger::log_warn(
          paste(
            "{x$year}-{x$route} had",
            "{x$scount} {x$common_name} aggregrated in stop_level",
            "{x$source}",
            "but {x$rcount} in the ebird checklist."
          )
        )
      }
    })()

  logger::log_trace("Getting ebird data without stop-level information")
  ebird_no_stop <- ebird |>
    dplyr::filter(is.na(stop_num)) |>
    select(
      year, common_name, sci_name, route, route_num, county, count
    ) |>
    (\(df) {
      df |>
        filter(
          !(paste(year, route) 
            %in% paste(stop_to_route$year, stop_to_route$route))
        ) |>
        (\(x) {
          logger::log_trace(
            paste(
              "Removed {nrow(df) - nrow(x)}",
              "ebird observations for route/years",
              "that were also in stop-level data.")
          )
          x
        })()
    })()

  logger::log_trace("Getting historical data")
  historical <- get_historical_data() |>
    select(
      year, common_name, route, route_num, county, count
    ) |>
    conform_taxonomy(taxonomy) |>
    (\(df) {
      df |>
        filter(
          !(paste(year, route) 
            %in% paste(ebird_no_stop$year, ebird_no_stop$route))
        ) |>
        (\(x) {
          logger::log_trace(
            "Removed {nrow(df) - nrow(x)} historical observations for route/years that were also entered in ebird."
          )
          x
        })() |>
        filter(!(paste(year, route) %in% paste(stop_to_route$year, stop_to_route$route))) |>
        (\(x) {
          logger::log_trace(
            "Removed {nrow(df) - nrow(x)} historical observations for route/years that were also in stop-level data."
          )
          x
        })()
    })()

  logger::log_trace("Combining route level data.")

  dplyr::bind_rows(
    historical |> mutate(source = "historical"),
    stop_to_route |> mutate(source = "stop-level"),
    ebird_no_stop |> mutate(source = "ebird")
  )
}

#' Main function to create route level count dataset
#'
#' @inheritParams create_route_level_0
create_route_level_counts <- function(ebird, stop_level_data, taxonomy, config = config) {
  df <- create_route_level_counts_0(ebird, stop_level_data, taxonomy, config)

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
        paste(
          "Completing route-level data added",
          "{nrow(x) - nrow(df)}",
          "observations with count of 0.")
      )
      x
    })()
}

#' Create the MBBS count datasets
#'
#' @export
create_mbbs_counts <- function(ebird_counts, config) {
  taxonomy <- get_ebird_taxonomy()

  stop_level <-
    create_stop_level_counts(ebird_counts, taxonomy, config)
  route_level <-
    create_route_level_counts(ebird_counts, stop_level, taxonomy, config)

  list(
    stop_level  = stop_level,
    route_level = route_level
  )
}

#' Create the MBBS datasets
#'
#' @export
create_mbbs_data <- function(config) {
  ebird <-  get_ebird_data()
  counts <- create_mbbs_counts(ebird$counts, config)

  # TODO:
  # - add back observers
  comments <- ebird$comments |>
    select(-observers) |>
    arrange(year, route, stop_num)
  
  list(
    mbbs_stop_counts   = counts$stop_level,
    mbbs_route_counts  = counts$route_level,
    comments = comments
  )
}

#' Write MBBS datasets
#' @export
write_mbbs_data <- function(config) {
  data <- create_mbbs_data(config)

  purrr::iwalk(
    .x = data,
    .f = ~ write.csv(.x,  file = paste0(.y, ".csv"), row.names = FALSE)
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
        msg = "Data was lost when conforming taxonomy This is bad."
      )
      x
    })()
}
