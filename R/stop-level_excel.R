#------------------------------------------------------------------------------#
# Functions/workflows for processing stop level data
# in excel provided by participants
#------------------------------------------------------------------------------#

#' Takes a file directory that contains
#' Folders of historical .xls
#' and processes them all into one df
#' @param directory directory containing containing historical xls files.
#' @include config.R
#' @returns vector of filenames of stop level files
list_stop_level_files <- function(directory = config$stop_level_data_dir) {
  # get list of folders in directory
  list.files(directory, recursive = TRUE) |>
    # only keep xls files in stops directories
    stringr::str_subset("\\_stops.*\\.xls$") |>
    (\(x) file.path(directory, x))()
}

#' Takes a file directory that contains
#' Folders of historical .xls
#' and processes them all into one df
#' @param directory directory containing containing historical xls files.
#' @include config.R
#' @returns vector of filenames of stop level files
get_stop_level_xls_data <- function(directory = config$stop_level_data_dir) {
  list_stop_level_files(directory) |>
    purrr::map_dfr(process_stop_level_xls)
}

#' Process stop-level excel sheets
#' They should all have the same format,
#' if it is in the wrong format
#' there will be errors
#' @importFrom dplyr %>% mutate relocate select matches across
#' @importFrom readxl read_excel
#' @importFrom stringr str_extract
#' @param file name of xls file to process
#' @returns hist_xls each row is a species with columns s1:s20
#'    containing the count at each stop
process_stop_level_xls <- function(file) {

  readxl::read_excel(
      path = file,
      col_types = rep("text", 25),
      col_names = c("species_code", paste("s", 1:20, sep = ""),
                    "sum", "sequence", "species_code2", "common_name"),
      skip = 5,
      .name_repair = "unique_quiet"
    ) |>
    # Filters to rows where the *first* column is a species code
    # (four capital letters OR acsp - "Accipiter sp.")
    dplyr::filter_at(1, (\(x) stringr::str_detect(x, "^[A-Z]{4}$|acsp"))) |>
    # Correct common names
    dplyr::mutate(
      common_name = str_replace_all(common_name,
                    c("Rock Dove" = "Rock Pigeon",
                      "^Whip-poor-will$" = "Eastern Whip-poor-will"))
    ) |>
    # Remove extraneous rows, keep only common name and stops
    dplyr::select(common_name, dplyr::matches("s[0-9]+")) |>
    # Pivot data from wide to long
    tidyr::pivot_longer(
      cols = s1:s20,
      names_to = "stop_num",
      names_prefix = "s",
      values_to = "count"
    ) |>
    # Parse counts as integers
    dplyr::mutate(
      as_tibble(extract_info_from_filename(file)),
      stop_num = as.integer(stop_num),
      route    = make_route_id(county, route_num),
      count = as.integer(
        case_when(
          count %in% c("x", "X") ~ "1",
          is.na(count)           ~ "0",
          TRUE                   ~ count))
    ) |>
    stop_level_xls_checks()

  # TODO...
  #   hist_xls_add_species_and_route_info() %>%

}

#' Pull mbbs_county, route_num, and year
#' out of a historical MBBS_XL
#' route_num MUST BE the first number in the filename
#' year MUST BE the second number in the filename
#' @importFrom stringr str_extract str_extract_all
#' @param filename a filename that contains the county, route num, and year
extract_info_from_filename <- function(filename) {
  county <- tolower(str_extract(filename, "[cC]hatham|[dD]urham|[oO]range"))
  nums <- str_extract_all(filename, "[0-9]+", simplify = TRUE)

  list(
    county = county,
    route_num = as.integer(nums[1]),
    year = as.integer(nums[2])
  )
}


#' Adds in the expected species and route info
#' To better match the usual processed mbbs data format
#' @importFrom dplyr left_join select
#' @param hist_xls a dataframe from a historical .xls
#' @param mbbs_survey_events rda of all surveys that have been run on the mbbs
#' @returns the hist_xls with added columns c(mbbs_county, route_num, year, observers, tax_order, sci_name)
hist_xls_add_species_and_route_info <- function(hist_xls, mbbs_survey_events = "data/mbbs_survey_events.rda") {
  # bring in other dfs
  taxonomy <- get_ebird_taxonomy()
  load(mbbs_survey_events)
  mbbs_survey_events <- mbbs_survey_events %>%
    dplyr::select(mbbs_county, route_num, year, observers)

  # add columns
  hist_xls <- hist_xls %>%
    left_join(taxonomy, by = "common_name") %>%
    left_join(mbbs_survey_events, by = c("mbbs_county", "route_num", "year"))
}


### Tests ###
#' A set of integrity checks
#' These do not check the validity of the data.
#' @importFrom purrr walk
#' @importFrom assertthat assert_that
#' @importFrom dplyr group_by summarize n
#' @param df a df from an .xls that has just been processed through
#'   the rest of the hist_xls_process_xls function
stop_level_xls_checks <- function(df) {
  # check for missing values where there shouldn't be.
  purrr::walk(
    .x = c(
      "common_name", "mbbs_county", "year", "route_num",
      "stop_num", "count", "route_ID"
    ),
    .f = ~ {
      assertthat::assert_that(
        !anyNA(df[[.x]]),
        msg = sprintf("Found NA %s values; there shouldn't be.", .x)
      )
    }
  )

  # check for missing sci_name values, except for "Accipiter species"
  # hist_xls_sci_name_check(df)

  # check that all species have exactly 20 rows (20 stops)
  entries <-
    df %>%
    dplyr::group_by(common_name) %>%
    dplyr::summarize(n_stops = n())

  purrr::walk(
    .x = c("n_stops"),
    .f = ~ {
      assertthat::assert_that(
        all(entries[[.x]] == 20),
        msg = sprintf("At least one species does not have 20 stops")
      )
    }
  )

  # return
  df
}

#' Defined separately from run_checks for testing purposes
#' Tests that all rows except for where common_name is
#' 'Accipiter species' have a non-NA sci_name column
#' @importFrom assertthat assert_that
#' @param df a df from an .xls that has just been processed through
#'   the rest of the hist_xls_process_xls function
hist_xls_sci_name_check <- function(df) {
  # check that all rows (except accipiter spp.) have sci_name
  assertthat::assert_that(
    !anyNA(df[df$common_name != "Accipiter species", ]$sci_name),
    msg = sprintf("Found NA sci_name values; there shouldn't be.")
  )
}
