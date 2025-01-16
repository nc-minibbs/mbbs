#------------------------------------------------------------------------------#
# Function for updating the survey_events tables
#------------------------------------------------------------------------------#

#' Updates survey_events if needed by rbinding new and updated surveys,
#'   then updates survey_events
#'   Called from import_data after updating the data/mbbs.rda files
#'   Not part of the process_observers workflow.
#' @importFrom dplyr filter group_by summarize ungroup arrange
#'   left_join mutate select ungroup n_distinct cur_group_id
#'   rows_update
#' @importFrom stringr str_to_lower
#' @importFrom utils write.csv
update_survey_events <- function(ebird, path = config$survey_list) {

  # load in survey list
  survey_list <- read.csv(file = path, header = TRUE)

  browser()
  # generate list of new surveys found in ebird
  ebird_surveys <-
    ebird$comments |>
    dplyr::mutate(
      observers = purrr::map_chr(observers, ~ paste(.x, collapse = ""))
    ) |>
    select(route, year, observers)

  # differing observers does not affect this join.
  # Which is good because the survey_list should be the true source of data
  # for observers
  new_surveys <- ebird_surveys |>
    anti_join(survey_list, by = c("route", "year"))

  # add new rows to survey_list if they exist
  # and save the updated list.
  if (nrow(new_surveys) > 0) {
    survey_list <-
      rbind(survey_list, new_surveys) |>
      arrange(route, year)
    write.csv(survey_list, config$survey_list, row.names = FALSE)
    # print message
    cat(nrow(new_surveys), "surveys have been added to survey_list")
  } else { # report that there were no new surveys
    cat("No new surveys to add to survey_list\\n")
  }

  # also need to check if any surveys have been updated, eg.
  # One stop wasn't uploaded for a route before and now has been
  # so that N and S may have changed.
  # We continue to not include observer in the anti_join because
  # survey_list is the ultimate source of information for some older
  # routes where the observers are not in the data
  updated_surveys <-
    surveys %>%
    mutate(
      month = as.integer(month),
      day = as.integer(day)
    ) %>%
    anti_join(survey_list, by = c("route_num", "year", "county", "S", "N"))

  if (nrow(updated_surveys) > 0) {
    survey_list <-
      rows_update(
        survey_list, updated_surveys,
        by = c("route_num", "year", "county")
      )
    write.csv(survey_list, config$survey_list, row.names = FALSE)
    cat(nrow(updated_surveys), "surveys updated on survey_list with new 'S' or 'N'")
  }

  # load in observer table
  observer_table <-
    read.csv(config$main_observer_conversion_table, header = TRUE)
  # Observer table may be updated several times during a year.
  # So we regenerate and update survey_events
  # even when we don't update(by rbinding new columns to) survey_list.

  mbbs_survey_events <-
    left_join(
      survey_list,
      observer_table,
      by = c("county", "route_num", "observers")
    ) %>%
    get_observer_quality() %>%
    group_by(primary_observer) %>%
    mutate(observer_ID = dplyr::cur_group_id()) %>% # add observer ID
    dplyr::ungroup() %>%
    arrange(mbbs_county, route_num, year)

  options(dplyr.summarise.inform = TRUE) # return this to normal

  # save survey_events
  save(mbbs_survey_events, file = system.file("data/mbbs_survey_events.rda", package = "mbbs"))
  cat("\nsurvey_events updated")
  # save a version for testing as well.
  # write.csv(mbbs_survey_events, file = system.file("tests/testthat/update_survey_events_test_cases.csv", package = "mbbs"), row.names = FALSE)
}
