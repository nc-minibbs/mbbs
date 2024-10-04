#------------------------------------------------------------------------------#
# Functions/workflows for cleaning and processing the observer field
# Updates survey events
#------------------------------------------------------------------------------#

#' Full workflow for processing observers
#' @param mbbs_county mbbs data.frame
#' @param county which county is being processed, orange, durham, or chatham lower case
#' @importFrom dplyr %>%
#' @returns mbbs_county
process_observers <- function(mbbs_county, county) {
  mbbs_county <- mbbs_county %>%
    observers_extractor() %>%
    propogate_observers_across_stops()

  update_observer_table(mbbs_county, county)
  # calling the mini_table update must be done HERE and not within
  # update_observer_table in order to preserve the ability to
  # take input on new observer combos.
  update_mini_observer_table()
  # similarly, standardizing observers must take place after the mini
  # table update, but cannot take place within update_observer_table
  # because the mini_table function cannot be within it and work
  # properly.
  standardize_observer_table()

  mbbs_county # return
}


#' Arranges and saves a new version of the observer table
#' @param observer_table observer table data.frame
#' @param file where the observer table is being saved
#' @importFrom dplyr arrange %>%
#' @importFrom utils write.csv
save_observer_table <-
  function(observer_table,
           file = system.file("/extdata/main_observer_conversion_table.csv", package = "mbbs")) {
    observer_table %>%
      arrange(mbbs_county, route_num) %>%
      write.csv(file, row.names = FALSE)
  }

#' Updates the main observer table when new route
#' + observer combos are present
#' Called from import_data as part of process_obervers() during the processing of new data
#' @param mbbs_county mbbs data.frame, must end in and underscore then the name of the county ie: _durham, _orange, _chatham
#' @param selected_county county that the observer table should be filtered on
#' @param save  TRUE if the updated table should be saved. Set to FALSE when testing
#' @importFrom dplyr filter add_row
#' @importFrom assertthat assert_that
update_observer_table <- function(mbbs_county, selected_county, save = TRUE) {
  # selected county must be one from the mbbs
  assertthat::assert_that(selected_county == "orange" |
    selected_county == "durham" |
    selected_county == "chatham")

  # load the main observer conversion table
  observer_table <- read.csv(system.file("/extdata/main_observer_conversion_table.csv", package = "mbbs"), header = TRUE)

  # load survey events
  survey_list <-
    read.csv(system.file("/extdata/survey_list.csv", package = "mbbs"), header = TRUE) %>%
    select(-S, -N, -month, -day)

  # filter the observer conversion table to just the one specified country
  county_observer_table <-
    observer_table %>%
    filter(mbbs_county == selected_county)

  # generate list of unique route number/observer combinations
  options(dplyr.summarise.inform = FALSE) # suppress dplyr "has grouped outby by"
  rocombos <-
    mbbs_county %>%
    group_by(route_num, observers) %>%
    summarize() # without argument this just gives back the unique combos
  options(dplyr.summarise.inform = TRUE) # return this to normal


  # check for any rocombos not already on the main observer conversion table
  not_present <-
    anti_join(rocombos, county_observer_table,
      by = c("route_num", "observers")
    )

  # flag and stop function if any NAs represent truly missing observer data
  for (i in 1:nrow(not_present)) {
    assertthat::assert_that(
      confirm_observer_NA(
        passed_na_row = not_present[i, ],
        mbbs_county, county_observer_table
      ) == FALSE
    )
  }

  # if all the NAs pass, remove them.
  not_present <-
    not_present %>%
    filter(!is.na(observers))

  # add new observer combos to the main observer table
  if (nrow(not_present) > 0) {
    for (h in 1:nrow(not_present)) {
      # add a new row to the overall observer_table
      observer_table <- observer_table %>%
        dplyr::add_row(
          mbbs_county = selected_county,
          route_num = not_present$route_num[h],
          observers = not_present$observers[h]
        )
    }
  }

  # separate out observers into columns obs1,obs2,obs3
  observer_table[c("obs1", "obs2", "obs3")] <-
    # split into at most 3 strings based off <,and> <,> <and> <&>
    stringr::str_split_fixed(
      observer_table$observers,
      n = 3,
      pattern = ", and |, and|,and|, |,| and | and| & | &|& |&"
    )
  # make corrections:
  # if there's one name but split by comma ie: Driscoll, Tom
  for (w in 1:length(observer_table$observers)) {
    if (stringr::str_detect(
      string = observer_table$observers[w],
      pattern = "^[\\w]+,\\s[\\w]+$"
    )) { # one word comma one word
      # then give obs1 just the whole thing. It's one name.
      observer_table$obs1[w] <- observer_table$observers[w]
      observer_table$obs2[w] <- "blank"
    }
  }

  if (save == TRUE) {
    save_observer_table(observer_table)
  }
}

#' Function to convert obs1 obs2 and obs3 from the main_observer_table
#' to the standard format using mini_observer_table conversions
#' Also creates the standardized observers column.
#' @param save  TRUE if the updated table should be saved. Set to FALSE when testing
#' @importFrom dplyr rowwise mutate c_across all_of
standardize_observer_table <- function(save = TRUE) {
  # read in the latest version of the mini table
  mini_observer_table <-
    read.csv(system.file("/extdata/mini_observer_conversion_table.csv", package = "mbbs"), header = TRUE)

  # read in the latest version of the main table
  observer_table <-
    read.csv(system.file("/extdata/main_observer_conversion_table.csv", package = "mbbs"), header = TRUE)

  # update the main observer table based on mini table.
  # convert obs1 obs2 and obs3 in the main table to their standardized format
  observer_table <- convert_based_on_mini_table(observer_table, mini_observer_table)

  # create standardized_observers
  # Specify the columns to be considered for alphabetical sorting
  obs_columns <- c("obs1", "obs2", "obs3")
  # combine obs1 obs2 and obs3 to create the standardized_observers column.
  observer_table <- observer_table %>%
    rowwise() %>%
    mutate(
      standardized_observers =
        paste(sort(c_across(all_of(obs_columns))), collapse = ", ")
    )

  if (save == TRUE) {
    save_observer_table(observer_table)
  }
}

#' Pull out observers from the checklist_comments column that process_comments missed
#' @param mbbs_county mbbs data.frame
#' @importFrom stringr str_replace_all str_detect
#' @importFrom dplyr mutate case_when
#' @returns mbbs_county the input dataframe with fixed observers
observers_extractor <- function(mbbs_county) {
  # fix unicode
  mbbs_county$checklist_comments <- mbbs_county$checklist_comments %>%
    stringr::str_replace_all("&#61;| =", "=")

  # when checklist comments contain "observer(s)", extract after observer(s) and before a ;
  mbbs_county <- mbbs_county %>%
    mutate(observers = case_when(
      # if observer column exists leave it
      is.na(observers) == FALSE ~ observers,
      # if observer column is NA, extract from comments after observer +before ;
      stringr::str_detect(checklist_comments, ".*[oO]bserver(s)?=") == TRUE ~
        sub(".*[oO]bserver(s)?=", "", checklist_comments) %>% # extract after observer
        {
          sub(";.*", "", .)
        }, # extract before ;
      # if observer column is NA but comments doesn't include observers, leave as is
      stringr::str_detect(checklist_comments, ".*[oO]bserver(s)?=") == FALSE ~ observers
    ))

  mbbs_county
}



#' Function to add observers to route stops 2:20
#' @param mbbs_county mbbs data.frame
#' @importFrom dplyr group_by mutate ungroup
#' @returns mbbs_county input dataframe with the observer column added to all stops
propogate_observers_across_stops <- function(mbbs_county) {
  # group by route and date (all observations on that route, inclusive of stops 1:20 on ebird checklists after 2019) and give all observer columns the same value as whatever column is not NA
  mbbs_county <- mbbs_county %>%
    group_by(route_num, date) %>%
    mutate(observers = observers[!is.na(observers)][1]) %>%
    ungroup()
  # will fill in stops 2:20 with checklist comments like v;3 and won't change data from pre-2019 because all the observations on the same route_num and date will already have the same comments/observer columns

  mbbs_county
}


#' Takes an observer/route combo where observer is NA and throws an error if the
#' survey for that route/year genuinely has no recorded observer either
#' within the mbbs_county dataframe or on the survey_list
#' @importFrom dplyr filter anti_join join_by
#' @importFrom assertthat assert_that
#' @param passed_na_row a dataframe with a single route_num and observer
#' @param mbbs_county an mbbs dataset that's restricted to just one county (as we use route_num which is not distinct between counties)
#' @param county_observer_table a main_observer_table that has already been filtered to just the relevant county
confirm_observer_NA <-
  function(passed_na_row, mbbs_county, county_observer_table) {
    assertthat::assert_that(nrow(passed_na_row) == 1)

    # read in survey list
    survey_list <- read.csv(system.file("/extdata/survey_list.csv", package = "mbbs"), header = TRUE)

    # confirm that the passed_na_row passed is an NA, if it's not just return and exit this function
    if (is.na(passed_na_row$observers) == FALSE) {
      # return("observers not NA") #for testing
      return(FALSE) # return is correct to use, exit function early.
    }

    # since the observer of this rocombo is NA,
    # evaluate if it should be ignored (we're not missing data)
    # or should throw an error (we're missing data)

    # filter to the na rows in mbbs_county that this rocombo represents
    na_rows <- mbbs_county %>% # take county df
      filter(is.na(observers) == TRUE) %>% # filter to the NA rows
      filter(route_num == passed_na_row$route_num) %>% # filter to the NA rows for this route
      anti_join(survey_list, join_by(mbbs_county, route_num, year))
    # only keep any rows with NA observers if that route/year combo is not already
    # represented on the survey list.
    # Years where observers == NA, but that are on the survey_list
    # (and so have an observer) are cut.

    # evaluate if this route has all it's observers on the survey_list
    if (nrow(na_rows) == 0) {
      # the NA observer seen on this passed_na_row has been corrected elsewhere,
      # it's on the survey_list.
      # return("observers corrected elsewhere") #for testing
      return(FALSE)
    } else {
      # this NA observer is not already on the survey list. This is likely because
      # (1). It's a new year of data, observers did not propagate to all the rows,
      # and survey_list gets updated after observer_conversion_table.
      # (2). It's a year of data where it's not on the survey_list and ALL the
      # route's 'observers' column are NA

      # filter mbbs_county to evaluate if ANY row of data from this route/year
      # combo contains an observers value
      # (and will therefore have been caught in a non-NA rowcombos[i])
      non_na_rows <- mbbs_county %>% # take the county df
        filter(year %in% na_rows$year) %>% # filter to the year we've got NA
        filter(route_num == passed_na_row$route_num) %>% # filter to the route
        filter(!is.na(observers)) # filter to any rows where obs is NOT NA

      # if there are NO rows in the mbbs where this route/year combo
      # has a non-NA observer, flag the error
      if (nrow(non_na_rows) == 0) {
        print(paste(
          "ERROR!",
          na_rows$year,
          "route",
          na_rows$route_num,
          "has only NA values for observers and no corrected record in mbbs_survey_events. Likely source of error: the ebird entry for stop 1 is missing observer information."
        ))
        return(TRUE)
      }
      # regardless of if there's an error or not, NA has now been fully evaluated.
      # return("Other row not NA") #for testing
      return(invisible(NULL))
    }
  }


#' Add any new entries to the mini_observer_conversion_table
#' @importFrom dplyr filter anti_join join_by rowwise c_across all_of
#' @importFrom stringr str_split_fixed str_detect str_extract str_sub
#' @importFrom utils write.csv
#' @param observer_table the main observer conversion table. Taken as an argument so the most recent version (potentially one in progress) can be used.
#' @param save TRUE if the updated table should be saved. Set to FALSE when testing
#' @returns an updated version of the main observer conversion table
update_mini_observer_table <- function(
    observer_table = read.csv(system.file("/extdata/main_observer_conversion_table.csv", package = "mbbs"), header = TRUE),
    save = TRUE) {
  # load the mini observer conversion table (for obs1,obs2,obs3)
  mini_observer_table <-
    read.csv(system.file("/extdata/mini_observer_conversion_table.csv", package = "mbbs"), header = TRUE)

  # make new table, get unique obs1, obs2, obs3
  obs_list <- c(observer_table$obs1, observer_table$obs2, observer_table$obs3)
  obs_list <- unique(obs_list[!is.na(obs_list)])

  # take input for the output_name if it's not yet on the mini_conversion_table
  input_name <- "example"
  output_name <- "example"
  temp_row <- data.frame(input_name, output_name)
  for (a in 1:length(obs_list)) {
    if (mini_observer_table %>% filter(input_name == obs_list[a]) %>% nrow() > 0) {
      # name is already on the list, do nothing
    } else {
      # name is not already on list, take input for the output name
      first_letter <- (str_sub(obs_list[a], start = 1, end = 1))
      pattern <- paste0("^[", toupper(first_letter), tolower(first_letter), "].*")
      letter_matches <- str_extract(mini_observer_table$output_name, pattern)
      letter_matches <- unique(letter_matches[!is.na(letter_matches)])
      print("New observer name needs standardizing for the mini_observer_conversion_table:")
      print(obs_list[a])
      print("Here are all standardized observers with the same first letter:")
      print(letter_matches)
      print(paste("What should", obs_list[a], "be converted to? Enter a standardized name:"))
      temp_row$input_name <- obs_list[a]
      temp_row$output_name <- readline(":")

      # add to mini_observer_table
      mini_observer_table <- rbind(mini_observer_table, temp_row)
    }
  }

  if (save == TRUE) {
    # save mini table
    write.csv(mini_observer_table, system.file("/extdata/mini_observer_conversion_table.csv", package = "mbbs"), row.names = FALSE)
  }
}


#' Standardize the names in the obs1, obs2, and obs3 columns
#'    of the main observer table
#' @importFrom dplyr left_join mutate %>% select
#' @param observer_table main_observer_table
#' @param mini_observer_table the mini_observer_table, has only columns
#'    'input_name' and 'output name'
convert_based_on_mini_table <- function(observer_table, mini_observer_table) {
  # add obs1 obs2 and obs3 to the observer_table
  observer_table <- observer_table %>%
    left_join(mini_observer_table, by = c("obs1" = "input_name")) %>%
    mutate(obs1 = output_name) %>%
    dplyr::select(-output_name) %>%
    left_join(mini_observer_table, by = c("obs2" = "input_name")) %>%
    mutate(obs2 = output_name) %>%
    dplyr::select(-output_name) %>%
    left_join(mini_observer_table, by = c("obs3" = "input_name")) %>%
    mutate(obs3 = output_name) %>%
    dplyr::select(-output_name)

  # returns
  observer_table
}

#' Creates a fixed effect (numeric value) of observer quality, which reflects
#' (observer's mean on this route - mean richness of years they are not one of the obs1-3)/
#' (mean richness of years they are not one of the obs1-3) ie:
#' (x-y)/y
#' observer_quality = max(obs1_quality, obs2_quality, obs3_quality, na.rm = TRUE)
#' Corrects for cases where a one-time observer accompanied a more experienced observer
#' and saw a high number of species in a particularly good year (putting their quality
#' above that of the more experienced observer)
#' @importFrom dplyr
#'  group_by summarize filter ungroup left_join first
#'  relocate mutate rename select rowwise case_when n
#' @importFrom tidyr pivot_longer
#' @param mbbs_survey_events a dataframe with the list of survey events, importantly needs to include information about number of species and the observers for each survey
get_observer_quality <- function(mbbs_survey_events) {
  # goal is to create a fixed effect of observer quality

  # table of average n species seen on each route
  S_average_route <-
    mbbs_survey_events %>%
    group_by(mbbs_county, route_num) %>%
    summarize(
      route_meanS = mean(S),
      n_surveys_route = n()
    ) %>%
    ungroup()

  # summary of number of mean(S) across routes for each observer,
  # + n surveys they've done
  observer_average <- mbbs_survey_events %>%
    # obs1/obs2/obs3 don't matter now
    tidyr::pivot_longer(obs1:obs3, values_to = "obs") %>%
    filter(!is.na(obs)) %>%
    # group by just how many times the observer has surveyed at all
    group_by(obs) %>%
    summarize(
      obs_meanS = mean(S),
      n_surveys_obs = n()
    ) %>%
    ungroup()

  # Calculate proportion deviation from mean species of other observers
  # on the route for each observer
  observer_average_route <-
    mbbs_survey_events %>%
    tidyr::pivot_longer(obs1:obs3, values_to = "obs") %>%
    filter(!is.na(obs)) %>%
    group_by(mbbs_county, route_num, obs) %>%
    summarize(
      obsroute_meanS = mean(S),
      n_surveys_obsroute = n()
    ) %>%
    ungroup() %>%
    # left join dfs we created above
    left_join(S_average_route, by = c("mbbs_county", "route_num")) %>%
    left_join(observer_average, by = c("obs")) %>%
    relocate(
      n_surveys_obsroute,
      n_surveys_obs,
      .after = "obs_meanS"
    ) %>%
    # meanS on route in years not run by that row's observer,
    # back calculated with means,
    # essentially
    # removing the observer's proportion of contribution towards the route_meanS
    mutate(
      non_focal_obsroute_meanS =
        ((route_meanS * n_surveys_route) -
          (obsroute_meanS * n_surveys_obsroute)) /
          (n_surveys_route - n_surveys_obsroute)
    )

  # use all the information we have about an observer to calculate how they do
  # compared to all other observers on the mbbs.
  # On each survey they've done, calculate their performance as follows:
  # (obsroute_year_S(route, year, observer) - non_focal_obsroute_meanS)
  # / non_focal_obsroute_meanS
  # If an observer has 5 route_years they've run, they will have 5 rows of data
  # and each row will contain a comparison score of how well they did that year
  # on that route
  # compared to every other year that route has been done by someone else.
  # To get their total performance or observer quality across all years and routes
  # group by observer and average their route_year observer qualities.
  # This method also produces a value where routes the observer has run more
  # frequently have more impact on their observer quality.
  # and we have more information about their performance than a single datapoint
  # for each route they've participated in.
  observer_year_average_route <-
    mbbs_survey_events %>%
    tidyr::pivot_longer(obs1:obs3, values_to = "obs") %>%
    filter(!is.na(obs)) %>%
    group_by(mbbs_county, route_num, obs, year) %>%
    summarize(
      obsroute_year_S = S
    ) %>%
    # left join the above dataframes bc we need some of that info
    left_join(observer_average_route, by = c("obs", "mbbs_county", "route_num")) %>%
    mutate( # create comparison score within the route year
      obs_yr_rt_quality =
        (obsroute_year_S - non_focal_obsroute_meanS) / non_focal_obsroute_meanS,
      obs_yr_rt_quality =
        ifelse(is.nan(obs_yr_rt_quality), 0, obs_yr_rt_quality)
    )

  # group by observer and take the mean quality from all their route-years
  # to get the average quality of each observer
  # proportion of species each observer observes relative
  # to what other people observe on their route(s)
  # using data from ALL their years of observations (each route-year combo)
  # instead of taking the average in each route and then averaging that (prev way)
  # which (prev way) gave equal influence to a route run once and a route run 15x
  # in (prev way) determining obs_quality
  # Interpretation is as follows:
  # a -0.08 observer quality means that observer on average sees 8% fewer
  # species on their route(s) compared to the average number of species
  # seen on their route(s) in years run by other observers
  observer_quality <-
    observer_year_average_route %>%
    group_by(obs) %>%
    summarize(
      obs_quality = mean(obs_yr_rt_quality),
      n_surveys_obs = first(n_surveys_obs)
    ) %>%
    ungroup()

  # assign observer_quality based on the performance of the top observer
  mbbs_survey_events <-
    mbbs_survey_events %>%
    # add obs1_deviation
    left_join(
      observer_quality,
      by = c("obs1" = "obs")
    ) %>%
    mutate(
      obs1_quality = obs_quality,
      obs1_nsurveys = n_surveys_obs
    ) %>%
    select(-c(obs_quality, n_surveys_obs)) %>%
    # add obs2_deviation
    left_join(observer_quality, by = c("obs2" = "obs")) %>%
    mutate(
      obs2_quality = obs_quality,
      obs2_nsurveys = n_surveys_obs
    ) %>%
    select(-c(obs_quality, n_surveys_obs)) %>%
    # add obs3 deviation
    left_join(observer_quality, by = c("obs3" = "obs")) %>%
    mutate(
      obs3_quality = obs_quality,
      obs3_nsurveys = n_surveys_obs
    ) %>%
    select(-c(obs_quality, n_surveys_obs)) %>%
    rowwise() %>%
    # Get the maximum obsquality between obs1, obs2, obs,
    # and which column it comes from
    mutate(
      # observer quality is max btwn the three obs deviations
      observer_quality =
        max(obs1_quality, obs2_quality, obs3_quality,
          na.rm = TRUE
        ),
      # record which observer was the best
      max_qual_observer =
        which.max(c(obs1_quality, obs2_quality, obs3_quality)),
      observer_quality = case_when(
        # if there's only one observer
        # don't change obs_quality
        (sum(is.na(c(obs1, obs2, obs3)) == FALSE) == 1) ~ observer_quality,
        # if obs 1 is the best observer but only has one survey across all routes
        # take max of obs2 and obs3
        (max_qual_observer == 1 & obs1_nsurveys == 1) ~ suppressWarnings(max(obs2_quality, obs3_quality, na.rm = TRUE)),
        # if obs 2 is the best observer but only has one survey across all routes
        # take max of obs1 and obs3
        (max_qual_observer == 2 & obs2_nsurveys == 1) ~ suppressWarnings(max(obs1_quality, obs3_quality, na.rm = TRUE)),
        # if obs 3 is the best observer but only has one survey across all routes
        # take max of obs1 and obs2
        (max_qual_observer == 3 & obs3_nsurveys == 1) ~ suppressWarnings(max(obs1_quality, obs2_quality, na.rm = TRUE)),
        # if none of the other statements are true, leave obs_quality the same
        TRUE ~ observer_quality
      )
    ) %>%
    ungroup() %>%
    # assign primary observer and observer ID based on the top observer
    mutate(primary_observer = case_when(
      max_qual_observer == 1 ~ obs1,
      max_qual_observer == 2 ~ obs2,
      max_qual_observer == 3 ~ obs3
    )) %>%
    dplyr::relocate(primary_observer, .before = "obs1") %>%
    group_by(primary_observer) %>%
    mutate(observer_ID = cur_group_id()) %>%
    ungroup()

  # return
  mbbs_survey_events
}
