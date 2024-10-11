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
list_stop_level_xls_files <- function(directory = config$stop_level_xls_dir) {
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
get_stop_level_xls_data <- function(directory = config$stop_level_xls_dir) {
  list_stop_level_xls_files(directory) |>
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
    col_names = c(
      "species_code", paste("s", 1:20, sep = ""),
      "sum", "sequence", "species_code2", "common_name"
    ),
    .name_repair = "unique_quiet"
  ) |>
    # Filters to rows where the *first* column is a species code
    # (four capital letters)
    dplyr::filter_at(1, (\(x) stringr::str_detect(x, "^[A-Z]{4}$"))) |>
    # Correct common names
    dplyr::mutate(
      common_name = stringr::str_replace_all(
        common_name,
        c(
          "Rock Dove" = "Rock Pigeon",
          "^Whip-poor-will$" = "Eastern Whip-poor-will"
        )
      )
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
      route = make_route_id(county, route_num),
      count = as.integer(
        case_when(
          count %in% c("x", "X") ~ "1",
          is.na(count) ~ "0",
          TRUE ~ count
        )
      )
    ) |>
    stop_level_xls_checks()
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
