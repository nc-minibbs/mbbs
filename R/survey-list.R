#------------------------------------------------------------------------------#
# Function for updating the survey_list table
#------------------------------------------------------------------------------#

#' Updates survey_list if needed by:
#'   finding any ebird surveys not yet on the survey_list
#'   standardizing obs1/obs2/obs3
#'   adding any new observer matches to the observer_conversion_table,
#'   taking input on new names,
#'   and then adding the now observer-standardized new surveys to survey_list
#' @importFrom dplyr filter group_by summarize ungroup arrange
#'   left_join mutate select ungroup n_distinct cur_group_id
#'   rows_update
#' @importFrom stringr str_to_lower str_trim str_to_title
#' @importFrom utils write.csv
update_survey_list <- function(ebird, config, path = config$survey_list, save = TRUE) {
  # load in survey list
  survey_list <- read.csv(file = path, header = TRUE)

  # generate list of new surveys found in ebird not yet on the survey list
  ebird_surveys <-
    ebird$comments |>
    # filter to only surveys not already on survey_list
    anti_join(survey_list, by = c("route", "year"))

  # flag and stop function if any rows in ebird_surveys have all NA observers
  # and that survey is not represented with a non-NA observer in another row.
  # eg: we're truly missing observer data.
  assertthat::assert_that(
    confirm_observer_NA(ebird_surveys) == FALSE,
    msg =
      paste(
        "While updating survey list",
        "at least one new survey found where there is NO observer information.",
        "Please see logged error message and track down error in ebird."
      )
  )

  # Checks have passed, so filter out surveys with all NA observers
  ebird_surveys <-
    ebird_surveys |>
    filter(!(is.na(obs1) == TRUE &
      is.na(obs2) == TRUE &
      is.na(obs3) == TRUE)) |>
    # Modify the observer strings, all to title case and trim white space
    mutate(
      obs1 = str_trim(str_to_title(obs1)),
      obs2 = str_trim(str_to_title(obs2)),
      obs3 = str_trim(str_to_title(obs3))
    )

  # Add any new observers to the conversion table
  update_observer_table(ebird_surveys, config, save = save)
  # This is called separately
  # to ensure input still gets taken for updating the table

  # check one last time that no rows have all NA observers,
  # like if an error happened when converting based on the
  # observer table
  final_check <- ebird_surveys |>
    filter(
      (is.na(obs1) == TRUE &
        is.na(obs2) == TRUE &
        is.na(obs3) == TRUE)
    )

  assertthat::assert_that(
    nrow(final_check) == 0
  )

  # Standardize obs1/obs2/obs3 based on the conversion table
  ebird_surveys <-
    ebird_surveys |>
    convert_based_on_observer_table(config) |>
    # select only the columns we need
    select(route, year, obs1, obs2, obs3) |>
    # create the standardized_observers column
    dplyr::rowwise() |>
    mutate(
      standardized_observers =
        paste(sort(dplyr::c_across(all_of(c("obs1", "obs2", "obs3")))),
          collapse = ", "
        )
    )

  # Add new surveys to survey list if they exist
  # And save the updated list if save = TRUE
  if (nrow(ebird_surveys) > 0) {
    survey_list <-
      rbind(survey_list, ebird_surveys) |>
      arrange(route, year)
    if (save == TRUE) {
      write.csv(survey_list, config$survey_list, row.names = FALSE)
    }
    # print message
    cat(nrow(ebird_surveys), "surveys have been added to survey_list")
  } else { # report that there were no new surveys
    logger::log_info(
      "No new surveys to add to survey_list"
    )
  }
}

# Takes a df and confirms TRUE if there is a survey where we are
# missing observer information
# called within update_survey_list so we have already removed
# rows with NA observers that have observer data on survey_list
confirm_observer_NA <- function(ebird_surveys) {
  # check for any rows with all NA observers
  all_na_obs <- ebird_surveys %>%
    filter((is.na(obs1) == TRUE &
      is.na(obs2) == TRUE &
      is.na(obs3) == TRUE))

  # if no rows are all NA, go ahead and return 'FALSE'
  if (nrow(all_na_obs) == 0) {
    return(FALSE)
  }

  # do these rows with NA observers have other rows with observer information?
  # If so, we're fine. If not, ERROR and end function here.
  for (i in 1:nrow(all_na_obs)) {
    same_survey_noNA <-
      ebird_surveys |>
      # remove the all_na_rows
      filter(!(is.na(obs1) == TRUE &
        is.na(obs2) == TRUE &
        is.na(obs3) == TRUE)) |>
      filter(
        year == all_na_obs$year[i],
        route == all_na_obs$route[i]
      )

    # If no other ebird_surveys row has the same year/route,
    # ERROR
    if (nrow(same_survey_noNA) == 0) {
      logger::log_error(
        paste(
          "{all_na_obs$route[i]} {all_na_obs$year[i]}",
          "has only NA values for observers",
          "and no corrected record in survey_list.",
          "Likely source of error:",
          "the ebird entry for stop 1 is missing observer information."
        )
      )
      return(TRUE)
      # TRUE represents that we found a genuine case
      # of observer information missing
    }

    # NAs have now been fully evaluated,
    # if we haven't returned yet there are no NAs that are a problem
    return(FALSE) # FALSE represents we found no genuine NAs
  }
}

#' Add any new entries to the observer_conversion_table
#' Does not return anything.
#' @importFrom stringr str_sub str_extract
update_observer_table <- function(ebird_surveys, config, save = TRUE) {
  observer_table <- read.csv(config$observer_conversion_table, header = TRUE)

  # get new names from ebird_surveys
  obs_list <- c(
    ebird_surveys$obs1,
    ebird_surveys$obs2,
    ebird_surveys$obs3
  )

  obs_list <- unique(obs_list[!is.na(obs_list)])

  # if there are no new names, end the function
  if (length(obs_list) == 0) {
    logger::log_info(
      paste(
        "Update_observer_table:",
        "No new observers added to observer conversion table"
      )
    )
    return() # end the function
  }

  # If a name is not yet on the conversion_table
  # take input for the output_name
  input_name <- "example"
  output_name <- "example"
  temp_row <- data.frame(input_name, output_name)
  for (a in 1:length(obs_list)) {
    if (observer_table %>% filter(input_name == obs_list[a]) %>% nrow() > 0) {
      # name is already on the list, do nothing
    } else {
      # name is not already on list, take input for the output name
      # pull the first letter of the input_name so we can check if anything similar
      # is already on the mini_table
      first_letter <- (stringr::str_sub(obs_list[a], start = 1, end = 1))
      pattern <- paste0("^[", toupper(first_letter), tolower(first_letter), "].*")
      # extract matches for that first letter from the mini_table
      letter_matches <- str_extract(observer_table$output_name, pattern)
      letter_matches <- unique(letter_matches[!is.na(letter_matches)])
      # take input
      print("New observer name needs standardizing for the observer_conversion_table:")
      print(obs_list[a])
      print("Here are all standardized observers with the same first letter:")
      print(letter_matches)
      print(paste0("What should <", obs_list[a], "> be converted to? Enter a standardized name:"))
      temp_row$input_name <- obs_list[a]
      temp_row$output_name <- readline(":")

      # add to the observer_table
      observer_table <- rbind(observer_table, temp_row)
    } # end else statement
  } # end loop through the obs_list

  if (save == TRUE) {
    # save the conversion table
    write.csv(observer_table, config$observer_conversion_table, row.names = FALSE)
  }
}


#' Take df, convert columns obs1/obs2/obs3 based on the
#' observer_table and then return the df with corrected columns
convert_based_on_observer_table <- function(ebird_surveys, config) {
  observer_table <- read.csv(config$observer_conversion_table, header = TRUE)

  # convert names to the observer table's output_name (eg, correct typos)
  ebird_surveys2 <- ebird_surveys %>%
    left_join(observer_table, by = c("obs1" = "input_name")) %>%
    mutate(obs1 = output_name) %>%
    dplyr::select(-output_name) %>%
    left_join(observer_table, by = c("obs2" = "input_name")) %>%
    mutate(obs2 = output_name) %>%
    dplyr::select(-output_name) %>%
    left_join(observer_table, by = c("obs3" = "input_name")) %>%
    mutate(obs3 = output_name) %>%
    dplyr::select(-output_name)

  # returns
  ebird_surveys
}
