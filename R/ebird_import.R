#------------------------------------------------------------------------------#
# Functions/workflows for loading ebird data from file(s)
#------------------------------------------------------------------------------#

#' Create a data.frame listing available eBird files
#' @include config.R
list_ebird_files <- function() {
  list.files(config$ebird_data_dir) |>
    (\(x) {
      dplyr::tibble(
        file   = x,
        county = stringr::str_extract(x, "Durham|Orange|Chatham"),
        export_date = stringr::str_extract(x, "\\d{8}")
      )
    })() |>
    dplyr::group_by(county) |>
    dplyr::mutate(
      latest = export_date == max(export_date)
    )
}

#' Column specification for ebird data
#' @importFrom readr cols col_character col_skip col_number col_integer col_date
#'                   col_time
ebird_cols <- cols(
  `Submission ID`          = col_character(),
  `Common Name`            = col_character(),
  `Scientific Name`        = col_character(),
  `Taxonomic Order`        = col_skip(),
  Count                    = col_character(),
  `State/Province`         = col_skip(),
  County                   = col_skip(),
  `Location ID`            = col_skip(),
  Location                 = col_character(),
  Latitude                 = col_number(),
  Longitude                = col_number(),
  Date                     = col_date(format = "%Y-%m-%d"),
  Time                     = col_time(),
  Protocol                 = col_skip(),
  `Duration (Min)`         = col_skip(),
  `All Obs Reported`       = col_skip(),
  `Distance Traveled (km)` = col_skip(),
  `Area Covered (ha)`      = col_skip(),
  `Number of Observers`    = col_integer(),
  `Breeding Code`          = col_skip(),
  `Observation Details`    = col_character(),
  `Checklist Comments`     = col_character(),
  `ML Catalog Numbers`     = col_skip()
)

#' Load the *most rececnt* ebird export per county
#' (i.e. MBBS ebird account)
#' into an R `data.frame`.
#' according the `ebird_cols` specification.
#' Additionally renames the variables.
load_ebird_data <- function() {
  # Get the latest file per county
  files <-
    list_ebird_files() |>
    dplyr::filter(latest)

  f <- \(x) {
    # file.path(config$ebird_data_dir, x)
    system.file(file.path(config$ebird_data_dir, x), package = "mbbs")
  }

  purrr::map2_dfr(
    .x = files$file,
    .y = files$county,
    .f = ~ {
      readr::read_csv(
        file = f(.x),
        col_types = ebird_cols,
      ) |>
        dplyr::rename(
          submission  = `Submission ID`,
          location    = Location,
          common_name = `Common Name`,
          sci_name    = `Scientific Name`,
          count       = Count,
          lat         = Latitude,
          lon         = Longitude,
          date        = Date,
          time        = Time,
          nobservers  = `Number of Observers`,
          obs_details = `Observation Details`,
          comments    = `Checklist Comments`
        )
    }
  )
}

#' Get exclusions
#' @return a character vector of submission ids to exclude
get_exclusions <- function() {
  yaml::read_yaml(config$excluded_submissions)
}

#' Exclude submissions
#' @param ebird data.frame loaded from `load_ebird_data`
#' @param exclusions character vector of submission ids to
exclude_submissions <- function(ebird, exclusions) {
  dplyr::filter(ebird, !(.data$submission %in% exclusions))
}

#' Removes subspecies, subgroup, or domestic type designations from the common
#' and scienfic name columns of an ebird csv
#' @inheritParams exclude_submissions
filter_ebird_data <- function(ebird) {
  ebird |>
    dplyr::filter(
      # remove highly non-specific observations.
      .data$sci_name != "Passeriformes sp."
    ) |>
    exclude_submissions(get_exclusions())
}

#' Parse the character count vector into an integer vector
#' NOTE: This parser maps the ebird "X" (meant to indicate presence)
#'       into 1.
#'       As as 20241001 there are only 11 observations that use "X",
#'       the most recent in 2012;
#'       most of the species in this case are reasonably uncommon.
#'
#' @param count character vector of ebird counts
parse_count <- function(count) {

  message(
    glue::glue("There are {n} observations that use X",
               n = sum(count == "X"))
  )

  as.integer(ifelse(count == "X", "1", count))
}

#' Parse the character sci_name vector
#' @param sci_name character vector of ebird scientific names
#' @importFrom stringr word fixed
parse_sci_name <- function(sci_name) {
  stringr::word(sci_name, 1, 2)
}

#' Parse the character common_name vector
#' @param common_name character vector of ebird common names
parse_common_name <- function(common_name) {
  stringr::word(common_name, 1, sep = stringr::fixed(" ("))
}

#' Parse a route's county from the ebird location name
#' @param location character vector of ebird locations
parse_county <- function(location) {
  location |>
    stringr::str_extract("[Oo]range|[Cc]hatham|Chatman|[Dd]urham") |>
    stringr::str_replace("Chatman", "Chatham") |>
    tolower()
}

#' Parse a route's number from the ebird location name
#' @param location character vector of ebird locations
parse_route_num <- function(location) {
  as.integer(stringr::str_match(location, "[0-1]{0,1}[0-9]{1}"))
}

#' Parse a stop number from the ebird location name
#' @param location character vector of ebird locations
parse_stop_num <- function(location) {
  # Getting stop from numbers at end (this is fragile):
  as.integer(stringr::str_extract(location, "([0-9]{1,2}$)"))
}

#' Transformation of the ebird data
#' @include utilities.R
transform_ebird_data <- function(ebird) {
  ebird |>
    dplyr::mutate(
      count       = parse_count(count),
      sci_name    = parse_sci_name(sci_name),
      common_name = parse_common_name(common_name),
      county      = parse_county(location),
      route_num   = parse_route_num(location),
      stop_num    = parse_stop_num(location),
      year        = lubridate::year(date),
      route       = make_route_id(county, route_num)
    ) |>
    # Drop location now that county/route_num/stop_num is extractedd
    dplyr::select(-location)
}

#' A basic set of import integrity checks
#' These do not check the validity of the data.
#' @param dt ebird data.frame
#' @importFrom glue glue
#' @importFrom dplyr distinct summarise mutate group_by pull filter
#' @keywords internal
ebird_import_checks <- function(dt) {
  # Check for missing values where there shouldn't be.
  purrr::walk(
    .x = c("date", "county", "route_num", "route", "count"),
    .f = ~ {
      assertthat::assert_that(
        !anyNA(dt[[.x]]),
        msg = sprintf("Found NA %s values; there shouldn't be.", .x)
      )
    }
  )

  # Check that routes have exactly 1 or 20 submissions.
  dt |>
    distinct(.data$year, .data$county, .data$route_num, .data$stop_num) |>
    group_by(.data$year, .data$county, .data$route_num) |>
    dplyr::summarise(
      n = dplyr::n(),
      flag = !(.data$n %in% c(1, 20))
    ) |>
    (\(x) {
      problems <-
        x[x$flag, ] |>
        dplyr::mutate(
          desc = glue::glue("{county}, {year}, {route_num}")
        ) |>
        dplyr::pull(.data$desc) |>
        paste0(collapse = "\n * ")

      if (any(x$flag)) {
        warning(sprintf(
          "The following year/route don't have either 1 or 20 checklists:\n %s",
          problems
        ))
      }
    } )()

  dt
}

#' Gets the ebird data
#' This does *not* include parsing of the comments into
#' habitat, observers, etc.
get_ebird_data <- function() {
  load_ebird_data() |>
    filter_ebird_data() |>
    transform_ebird_data() |>
    ebird_import_checks()
}
