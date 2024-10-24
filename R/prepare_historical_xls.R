#' Takes a file directory that contains
#' Folders of historical .xls
#' and processes them all into one df
#' @importFrom purrr map_dfr
#' @param directory the mbbs directory that has folders containing historical xls files.
#' @returns stopsmbbs a df of all the point count information from .xls files in all folders in the directory. Each row is a count of a species at a stop
hist_xls_purrr_directories <- function(directory = "inst/extdata/stop_level_data") {
  # get list of folders in directory
  filenames <- list.files(directory)
  # only keep folders ending in _stops
  filenames <- filenames[str_detect(filenames, "\\_stops$")]
  # add in the rest of the file path
  filenames <- paste(directory, filenames, sep = "/")
  # create a df combining all the .xls data in all the folders in the directory
  stopsmbbs <- purrr::map_dfr(filenames, hist_xls_purrr_files_in_directory)
}

#' Takes a file directory of historical .xls
#' files, and processes them,
#' returning a dataframe that contains
#' all the information from that directory's files
#' broken down to the stop level.
#' @importFrom purrr map_dfr
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>%
#' @param directory a directory that has folders containing historical
#'    xls files. Has a default for testing purposes
#' @returns stopsmbbs a df of all the point count information from .xls files
#'    in the directory. Each row is a count of a species at a stop
hist_xls_purrr_files_in_directory <- function(directory = "inst/extdata/stop_level_data/Pippen_Durham_stops") {
  # list directory files
  filenames <- list.files(directory)
  # keep only .xls files
  filenames <- filenames[str_detect(filenames, "\\.xls$")]
  # add in the rest of the path
  filenames <- paste(directory, filenames, sep = "/")
  # process all xls in the directory
  stopsmbbs <- purrr::map_dfr(filenames, hist_xls_process_xls)

  # return
  stopsmbbs
}


#' Process historical excel sheets
#' They should all have the same format,
#' if it is in the wrong format
#' there will be errors
#' Call this function with purrr to process multiple files into one df
#' @importFrom dplyr %>% mutate relocate select matches across
#' @importFrom readxl read_excel
#' @importFrom stringr str_extract
#' @importFrom purrr map_dfr
#' @param filenames  a list of historical mbbs .xls files within a folder
#' @returns hist_xls each row is a species with columns s1:s20
#'    containing the count at each stop
hist_xls_process_xls <- function(filenames) {
  # cut down the filename to the unique information about the route
  filename <- str_extract(filenames, "_stops/.*")

  hist_xls <-
    read_excel(filenames, .name_repair = "unique_quiet") %>%
    hist_xls_filter_to_species_code_rows() %>%
    hist_xls_rename_columns() %>%
    mutate(sequence = as.integer(sequence)) %>% # ensure sequence is number
    dplyr::relocate(common_name, .before = species_code) %>% # readability
    hist_xls_correct_common_names() %>%
    # remove extraneous rows, keep only common name and stops
    dplyr::select(common_name, dplyr::matches("s[0-9]+")) %>%
    mutate(hist_xls_extract_county_num_year_from_filename(filename),
      source = "prep_sld, hist xls",
      route_num = as.integer(route_num)
    ) %>%
    mutate(across(s1:s20, ~ ifelse(is.na(.), 0, .))) %>% # change NAs to 0
    # change x and X to 1, following Haven Wiley's example
    purrr::map_dfr(., ~ ifelse(. == "x", 1, .)) %>%
    purrr::map_dfr(., ~ ifelse(. == "X", 1, .)) %>%
    # ensure counts are integers, not characters
    mutate(across(s1:s20, as.integer)) %>%
    hist_xls_add_species_and_route_info() %>%
    # pivot
    pivot_longer(
      cols = s1:s20,
      names_to = "stop_num", names_prefix = "s", values_to = "count"
    ) %>%
    mutate(stop_num = as.integer(stop_num))

  hist_xls_run_checks(hist_xls)
}

#' Filters to only rows where the first column is a species code
#' that is four capital letters
#' or acsp - "Accipiter sp."
#' @importFrom stringr str_detect
#' @importFrom dplyr filter
#' @param x a dataframe from a historical .xls
hist_xls_filter_to_species_code_rows <- \(x){
  x %>%
    filter(str_detect(.[[1]], "^[A-Z][A-Z][A-Z][A-Z]$|acsp"))
}


#' Rename the columns of a historical xls
#' @importFrom dplyr %>%
#' @param x a dataframe from a historical .xls
hist_xls_rename_columns <- \(x){
  x %>%
    `colnames<-`(c(
      "species_code", paste("s", 1:20, sep = ""), "sum",
      "sequence", "species_code2", "common_name"
    ))
}


#' Corrects species common_names to
#' taxonomic standard
#' @param x a dataframe from a historical .xls
#' @importFrom stringr str_replace_all
hist_xls_correct_common_names <- \(x){
  x %>%
    mutate(
      common_name = str_replace(common_name, "Rock Dove", "Rock Pigeon"),
      common_name = str_replace(common_name, "^Whip-poor-will$", "Eastern Whip-poor-will")
    )
}

#' Pull mbbs_county, route_num, and year
#' out of a historical MBBS_XL
#' route_num MUST BE the first number in the filename
#' year MUST BE the second number in the filename
#' @importFrom stringr str_extract str_extract_all
#' @param filename a filename that contains the county, route num, and year
hist_xls_extract_county_num_year_from_filename <- function(filename) {
  mbbs_county <-
    str_extract(filename, "[cC]hatham|[dD]urham|[oO]range") %>%
    tolower()

  nums <- str_extract_all(filename, "[0-9]+")[[1]]
  route_num <- nums[1] %>%
    as.integer()
  year <- nums[2] %>%
    as.integer()

  survey_information <- data.frame(mbbs_county, route_num, year)
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
    mbbs_generate_route_ID() %>%
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
hist_xls_run_checks <- function(df) {
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
  hist_xls_sci_name_check(df)

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
