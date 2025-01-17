#------------------------------------------------------------------------------#
# Functions/workflows for loading ebird data from file(s)
#------------------------------------------------------------------------------#

#' Create a data.frame listing available eBird files
#' @include config.R
list_ebird_files <- function(path = config$ebird_data_dir) {
  list.files(path) |>
    (\(x) {
      dplyr::tibble(
        file = x,
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
ebird_cols <- readr::cols(
  `Submission ID`          = readr::col_character(),
  `Common Name`            = readr::col_character(),
  `Scientific Name`        = readr::col_character(),
  `Taxonomic Order`        = readr::col_skip(),
  Count                    = readr::col_character(),
  `State/Province`         = readr::col_skip(),
  County                   = readr::col_skip(),
  `Location ID`            = readr::col_skip(),
  Location                 = readr::col_character(),
  Latitude                 = readr::col_number(),
  Longitude                = readr::col_number(),
  Date                     = readr::col_date(format = "%Y-%m-%d"),
  Time                     = readr::col_time(),
  Protocol                 = readr::col_skip(),
  `Duration (Min)`         = readr::col_skip(),
  `All Obs Reported`       = readr::col_skip(),
  `Distance Traveled (km)` = readr::col_skip(),
  `Area Covered (ha)`      = readr::col_skip(),
  `Number of Observers`    = readr::col_integer(),
  `Breeding Code`          = readr::col_skip(),
  `Observation Details`    = readr::col_character(),
  `Checklist Comments`     = readr::col_character(),
  `ML Catalog Numbers`     = readr::col_skip()
)

#' Load the *most recent* ebird export per county
#' (i.e. MBBS ebird account)
#' into an R `data.frame`.
#' according the `ebird_cols` specification.
#' Additionally renames the variables.
#'
#' NOTE: readr::read_csv gives warnings upon parsing the ebird CSVs.
#'       As best I (BS) can tell as of 20241001,
#'       the problems stem from non-regularity in the ebird exports.
#'       Sometimes, columns are missing from a row.
#'       In other words,
#'       the ebird csv can have different numbers of columns per row!
#'       This issue does not seem to affect our data,
#'       but this is something to review.
load_ebird_data <- function(path = config$ebird_data_dir) {
  # Get the latest file per county
  files <-
    list_ebird_files(path = path) |>
    dplyr::filter(latest)

  logger::log_info("Loading ebird data from the following exports:")
  logger::log_info("{files$county}: {files$file}")

  purrr::map2_dfr(
    .x = files$file,
    .y = files$county,
    .f = ~ {
      qread <- purrr::quietly(readr::read_csv)
      qread(
        file = file.path(path, .x),
        col_types = ebird_cols
      ) |>
        (\(x) {
          if (!is.null(x$warnings)) {
            logger::log_warn(x$warnings)
          }
          x$result
        })() |>
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
get_exclusions <- function(file = config$excluded_submissions) {
  yaml::read_yaml(file)
}

#' Exclude submissions
#' @param ebird data.frame loaded from `load_ebird_data`
#' @param exclusions character vector of submission ids to exclude
exclude_submissions <- function(ebird, exclusions) {
  out <- ebird |>
    dplyr::filter(!(.data$submission %in% exclusions))

  logger::log_info(
    glue::glue("{n} observations were removed by excluded submissions.",
      n = nrow(ebird) - nrow(out)
    )
  )

  out
}

#' Removes subspecies, subgroup, or domestic type designations from the common
#' and scienfic name columns of an ebird csv
#' @inheritParams exclude_submissions
#' @include utilities.R
filter_ebird_data <- function(ebird) {
  ebird |>
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
  logger::log_info(
    glue::glue("{n} observations had count of \"X\" and were set to count of 1",
      n = sum(count == "X")
    )
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
  as.integer(stringr::str_extract(trimws(location), "([0-9]{1,2}$)"))
}

#' Transformation of the ebird data to conform to format
#' downstream consumers expect
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

#' Transformation of the ebird data to count data
#' @include utilities.R
compute_ebird_counts <- function(ebird) {
  ebird |>
    # Handle cases where stripping common name modifier
    # in parse_common_name creates two records for the same species.
    # e.g. when a checklist has observations for both
    # Mallard and Mallard (Domestic type)
    dplyr::group_by(
      submission,
      common_name,
      sci_name,
      county,
      route,
      route_num,
      stop_num,
      date,
      year,
      obs_details
    ) |>
    dplyr::summarise(
      count = sum(count)
    ) |>
    dplyr::ungroup()
}


#' Get deviations
#' @return a list of deviations
get_deviations <- function(file = config$stop_deviations) {
  yaml::read_yaml(file)
}

#' Identify stops to include from the list of deviations.
#' @return a data.frame of year/county/route/stop_num which need to be added
#'         to the ebird data
stops_to_include <- function(deviations) {
  deviations |>
    purrr::keep(.p = ~ length(.x$stops_nobirds) > 0) |>
    purrr::map_dfr(
      ~ .x[names(.x) %in% c("year", "date", "county", "route", "stops_nobirds")] |>
        dplyr::as_tibble() |>
        dplyr::rename(
          route_num = route,
          stop_num = stops_nobirds
        )
    ) |>
    dplyr::mutate(
      date = lubridate::ymd(date),
      year = lubridate::year(date),
      county = tolower(county),
      route = make_route_id(county, route_num = route_num)
    )
}

#' Adds stops that were surveyed,
#' but no birds were observed.
handle_deviations <- function(ebird, deviations) {
  add <- stops_to_include(deviations) |>
    # Need to add some species with a 0 count.
    # 0 count for other species will be added downstream.
    dplyr::mutate(
      common_name = "Northern Cardinal",
      sci_name = "Cardinalis cardinalis",
      count = 0L
    )

  logger::log_info(
    "Deviation: Adding observation for {add$year} {add$route} {add$stop_num}"
  )

  dplyr::bind_rows(ebird, add)
}

#' A basic set of import integrity checks
#' These do not check the validity of the data.
#' @param dt ebird data.frame
#' @importFrom glue glue
#' @importFrom dplyr distinct summarise mutate group_by pull filter
#' @include utilities.R
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

  # Check that submissions after 2022 have stop num
  dt |>
    dplyr::filter(year > 2022) |>
    dplyr::distinct(
      .data$submission, .data$year, .data$route, .data$stop_num
    ) |>
    dplyr::mutate(
      flag = is.na(.data$stop_num)
    ) |>
    dplyr::filter(flag) |>
    (\(x) {
      if (any(x$flag)) {
        logger::log_error(
          "x$submission} ({x$year}/{x$route}) is missing stop number"
        )
      }
    })()

  # Check that routes have exactly 1 or 20 submissions.
  dt |>
    dplyr::distinct(
      .data$year, .data$route, .data$stop_num
    ) |>
    dplyr::group_by(.data$year, .data$route) |>
    dplyr::summarise(
      n = dplyr::n(),
      flag = !(.data$n %in% c(1, 20))
    ) |>
    (\(x) {
      problems <-
        x[x$flag, ] |>
        dplyr::mutate(
          desc = glue::glue("{route}, {year}")
        )

      if (any(x$flag)) {
        logger::log_warn(
          "{problems$desc} had {problems$n} checklists, not 1 or 20 checklists."
        )
      }
    })()

  # Check for duplicate submissions (> 1 submission in year)
  dt |>
    dplyr::group_by(
      .data$year, .data$route, .data$stop_num, .data$common_name
    ) |>
    dplyr::summarise(
      n = dplyr::n(),
      flag = n > 1
    ) |>
    (\(x) {
      problems <-
        x[x$flag, ] |>
        dplyr::mutate(
          desc = glue::glue("{route}-{stop_num}, {year}, {common_name}")
        ) |>
        dplyr::pull(.data$desc)

      if (any(x$flag)) {
        logger::log_warn("{problems}: had more than 1 submission")
      }
    })()

  # Check that submissions are within date range
  dt |>
    dplyr::distinct(
      .data$submission, .data$date
    ) |>
    dplyr::filter(!(valid_date_range(date))) |>
    (\(x) {
      logger::log_error("{x$submission}: {x$date} is outside study dates")
    })()

  dt
}

#' Gets the ebird data
#' This does *not* include:
#' * aligning common_names to standard taxonomy
#' @export
get_ebird_data <- function() {
  load_ebird_data() |>
    filter_ebird_data() |>
    transform_ebird_data() |>
    (\(x){
      list(
        counts = x |>
          compute_ebird_counts() |>
          handle_deviations(deviations = get_deviations()) |>
          ebird_import_checks(),
        comments = x |>
          comment_workflow()
      )
    })()
}
