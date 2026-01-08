#------------------------------------------------------------------------------#
# Functions for collating and exporting the MBBS datasets
#------------------------------------------------------------------------------#

#' Create the stop level dataset (initial step)
#'
#' @param ebird ebird dataset resulting from `get_ebird_data`
#' @param taxonomy taxonomy dataset
#' @param config mbbs configuration
create_stop_level_counts_0 <- function(ebird, taxonomy, config = config) {
  logger::log_trace("Getting ebird stop-level data.")
  stop_ebird <-
    ebird |>
    dplyr::filter(!is.na(stop_num)) |>
    dplyr::select(
      year, route, stop_num,
      common_name, count,
      county, route_num
    )

  logger::log_trace("Getting xls stop-level data.")
  stop_xls <- get_stop_level_xls_data() |>
    conform_taxonomy(taxonomy)

  logger::log_trace("Getting obs-details stop-level data.")
  stop_obs <- process_obs_details(ebird) |>
    select(year, route, common_name, county, route_num, stop_data) |>
    tidyr::unnest(cols = stop_data)

  logger::log_trace("Getting transcribed stop-level data.")
  stop_transcribed <- get_stop_level_transcribed() |>
    select(
      year, route, stop_num,
      common_name, count,
      county, route_num, source
    ) |>
    conform_taxonomy(taxonomy)

  logger::log_trace("Combining stop level data.")

  dplyr::bind_rows(
    stop_ebird |> mutate(source = "ebird"),
    stop_xls |> mutate(source = "observer_xls"),
    stop_obs |> mutate(source = "obs_details"),
    stop_transcribed
  ) |>
    # Validity checks
    (\(df) {
      assertthat::assert_that(
        !anyNA(df$stop_num),
        msg = "Stop data has NA values in stop_num."
      )

      # Check that there are not duplicate entries with different counts
      # LOG CALL REMOVED: we won't be updating/modifying these data further.
      # Surveys with differing counts from different sources are assigned the 'true'
      # values based on handling detailed in docs/data_pipeline.
      # df |>
      #   dplyr::group_by(year, route, stop_num, common_name) |>
      #   dplyr::summarise(
      #     n = dplyr::n(),
      #     distinctcounts = n_distinct(count),
      #     sources = paste(source, collapse = " & ")
      #   ) |>
      #   dplyr::filter(n > 1 & distinctcounts > 1) |>
      #   (\(x) {
      #     logger::log_error(
      #       paste(
      #         "Route {x$route}-{x$stop_num} has multiple entries",
      #         "with differing counts",
      #         "for {x$common_name}",
      #         "in {x$year} coming from {x$sources}"
      #       )
      #     )
      #   })()

      # Log number of duplicate entries with the same count
      # LOG CALL REMOVED: we won't be updating/modifying these data further.
      # No longer need to know how many stop-level data entries had duplicates
      # from two or more sources.
      #   df |>
      #     dplyr::distinct(year, route, stop_num, common_name, count,
      #       .keep_all = TRUE
      #     ) |>
      #     (\(x) {
      #       logger::log_info(
      #         paste(
      #           "Stop-level data contains",
      #           "{nrow(df) - nrow(x)} duplicated observations",
      #           "(same count, different sources)"
      #         )
      #       )
      #     })()
      #
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

  # keep only source of stop-level data
  # using the following order:
  source_preference <-
    c(
      "ebird",
      "obs_details",
      "transcribed_paper",
      "observer_xls"
    )

  df <- df |>
    group_by(route, year) |>
    tidyr::nest() |>
    mutate(
      sources = purrr::map(data, ~ unique(.x$source)),
      data = purrr::map2(
        sources, data,
        ~ if (length(.x) == 1) {
          .y
        } else {
          filter_to <-
            source_preference[
              which.max(c(
                "ebird" %in% .x,
                "obs_details" %in% .x,
                "transcribed_paper" %in% .x,
                "observer_xls" %in% .x
              ))
            ]
          filter(.y, source == filter_to)
        }
      )
    ) |>
    select(-sources) |>
    tidyr::unnest(cols = c(data)) |>
    ungroup()

  logger::log_trace("Stop-level duplicated observations removed")

  df |>
    # For each year that a route was run,
    # add 0 count for those species that were *not* observed
    # in that route / stop / year.
    tidyr::complete(
      tidyr::nesting(
        year, county, route, route_num, stop_num, source
      ),
      tidyr::nesting(common_name),
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
        paste(
          "Completing stop-level data added",
          "{nrow(x) - nrow(df)}",
          "observations with count of 0."
        )
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

  stop_to_route <-
    stop_level_data |>
    group_by(year, county, route, route_num, common_name, source) |>
    summarise(
      nstops = n(),
      count = sum(count)
    ) |>
    select(
      year, common_name,
      route, route_num, county, count,
      nstops, source
    )

  # Compare stop_level and ebird counts
  dplyr::left_join(
    stop_to_route |>
      dplyr::ungroup() |>
      dplyr::select(common_name, year, route, scount = count, source),
    ebird |>
      dplyr::filter(is.na(stop_num)) |>
      dplyr::ungroup() |>
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
    dplyr::filter(diff != 0) #|>
  # LOG CALL REMOVED: we won't be updating/modifying these data further.
  # Surveys with differing counts from different sources are assigned the 'true'
  # values based on handling detailed in docs/data_pipeline.
  # (\(x) {
  #  if (nrow(x) > 0) {
  #    logger::log_warn(
  #      paste(
  #        "{x$year}-{x$route} had",
  #        "{x$scount} {x$common_name} aggregrated in stop_level",
  #        "{x$source}",
  #        "but {x$rcount} in the ebird checklist."
  #      )
  #    )
  #  }
  # })()

  logger::log_trace("Getting ebird data without stop-level information")
  ebird_no_stop <- ebird |>
    dplyr::filter(is.na(stop_num)) |>
    select(
      year, common_name, route, route_num, county, count
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
              "that were also in stop-level data."
            )
          )
          x
        })()
    })()

  logger::log_trace("Getting historical data")
  historical <-
    get_historical_data() |>
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
            paste(
              "Removed {nrow(df) - nrow(x)}",
              "historical observations for route/years",
              "that were also entered in ebird."
            )
          )
          x
        })() |>
        filter(
          !(paste(year, route) %in%
            paste(stop_to_route$year, stop_to_route$route))
        ) |>
        (\(x) {
          logger::log_trace(
            paste(
              "Removed {nrow(df) - nrow(x)}",
              "historical observations for route/years",
              "that were also in stop-level data."
            )
          )
          x
        })()
    })()

  logger::log_trace("Combining route level data.")
  dplyr::bind_rows(
    historical |> mutate(source = "historical"),
    stop_to_route |> mutate(source = "stop-level"),
    ebird_no_stop |> mutate(source = "ebird")
  ) |>
    # If the route data did not come from a stop level summary,
    # then we assume all stops were run.
    # Note that this only affects surveys prior to 2022 when the protocol
    # changed to stop-level data collection in eBird.
    mutate(
      nstops = if_else(is.na(nstops), 20L, nstops)
    )
}

#' Main function to create route level count dataset
#'
#' @inheritParams create_route_level_0
create_route_level_counts <- function(ebird, stop_level_data, taxonomy, config = config) {
  df <- create_route_level_counts_0(ebird, stop_level_data, taxonomy, config)

  yrs_in <-
    dplyr::distinct(df, year, route) |>
    arrange(year, route)

  df |>
    # For each year that a route was run,
    # add 0 count for those species that were *not* observed
    # in that route / stop / year.
    tidyr::complete(
      tidyr::nesting(
        year, county, route, route_num, source, nstops
      ),
      tidyr::nesting(common_name),
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
          "observations with count of 0."
        )
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
    create_stop_level_counts(ebird_counts, taxonomy, config) |>
    # add scientific name
    add_sci_name(taxonomy)

  route_level <-
    create_route_level_counts(ebird_counts, stop_level, taxonomy, config) |>
    # add scientific name
    add_sci_name(taxonomy)

  list(
    stop_level  = stop_level,
    route_level = route_level
  )
}

#' Create survey data
create_survey_data <- function(ebird, route_counts, .config = config) {
  # Ensure survey list is up-to-date
  update_survey_list(ebird, config = .config)

  # Extract data needed from ebird_comments
  comments <- ebird$comments |>
    group_by(year, route) |>
    summarise(
      # Take most common date in case of differences
      date = table(date) |> which.max() |> names(),
      # Flag if any submission is a protocol violation
      protocol_violation = any(violation, na.rm = TRUE)
    ) |>
    ungroup()

  # Get table of surveys
  surveys <- readr::read_csv(
    file = .config$survey_list,
    col_types = readr::cols(
      route = readr::col_character(),
      year = readr::col_integer(),
      obs1 = readr::col_character(),
      obs2 = readr::col_character(),
      obs3 = readr::col_character(),
      standardized_observers = readr::col_skip()
    )
  )

  # Get summaries of counts
  count_summary <- route_counts |>
    group_by(route, year) |>
    summarise(
      total_species = sum(count > 0),
      total_abundance = sum(count),

      # NOTE: the nstops variable here is used to
      # update the protocol violation flag in the next step.
      # Errors in the number of stops (i.e. != 20)
      # should be caught earlier in the pipeline.
      # Using min is hacky of course,
      # but cases of violations of > 20 stops are errors
      # that must be fixed earlier in the pipeline,
      # while < 20 stops may be valid violation.
      nstops = min(nstops)
    )

  # Prepare data for output
  surveys |>
    left_join(count_summary, by = c("route", "year")) |>
    left_join(comments, by = c("route", "year")) |>
    mutate(
      protocol_violation = dplyr::case_when(
        is.na(protocol_violation) ~ FALSE,
        nstops != 20 ~ TRUE,
        TRUE ~ protocol_violation
      )
    ) |>
    select(-nstops)
}

#' Create the MBBS datasets
#'
#' @export
create_mbbs_data <- function(.config = config) {
  ebird <- get_ebird_data()
  counts <- create_mbbs_counts(ebird$counts, config)
  surveys <- create_survey_data(
    ebird = ebird,
    route_counts = counts$route_level,
    .config = .config
  )
  stop_surveys <- create_stop_survey_list(ebird$locations, counts$stop_level)

  comments <- ebird$comments |>
    arrange(year, route, stop_num)

  list(
    mbbs_stops_counts = counts$stop_level,
    mbbs_route_counts = counts$route_level,
    surveys = surveys,
    comments = comments,
    stop_surveys = stop_surveys
  )
}

#' Write MBBS datasets
#'
#' @export
write_mbbs_data <- function(.config = config) {
  dir.create("output")

  log_file <- "output/log.txt"
  logger::log_appender(logger::appender_file(log_file))

  data <- create_mbbs_data(.config)

  purrr::iwalk(
    .x = data,
    .f = ~ {
      write.csv(.x, file = paste0("output/", .y, ".csv"), row.names = FALSE)
    }
  )
}
