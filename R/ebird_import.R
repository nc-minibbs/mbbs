#------------------------------------------------------------------------------#
# Functions/workflows for loading ebird data from file(s)
#------------------------------------------------------------------------------#

#' Create a data.frame listing available eBird files
list_ebird_files <- function() {
  list.files(config$ebird_data_dir) |>
    (\(x) {
      dplyr::tibble(
        county = stringr::str_extract(x, "Durham|Orange|Chatham"),
        export_date = stringr::str_extract(x, "\\d{8}")
      )
    })()
}

#' Load ebird exports into R
#' 
load_ebird_data <- function() {
  list_ebird_files()
  # |>
    # (\(x) x[which.max(as.integer(stringr::str_extract(x, "\\d{4}")))])() |>
    # (\(x) file.path(config$taxonomy_data_dir, x))() |>
    # (\(x) system.file(x, package = "mbbs"))() |>
}


#' Renames and selects columns from data.frame
#' created from the ebird csv export.
#'
#' @param ebird a data.frame imported from an ebird export.
#' @export
rename_ebird_data <- function(ebird) {
  dplyr::select(
    ebird,
    sub_id             = .data$Submission.ID,
    common_name        = .data$Common.Name,
    sci_name           = .data$Scientific.Name,
    tax_order          = .data$Taxonomic.Order,
    count_raw          = .data$Count,
    state              = .data$State.Province,
    loc                = .data$Location,
    locid              = .data$Location.ID,
    lat                = .data$Latitude,
    lon                = .data$Longitude,
    date               = .data$Date,
    time               = .data$Time,
    protocol           = .data$Protocol,
    distance_traveled  = .data$Distance.Traveled..km.,
    area_covered       = .data$Area.Covered..ha.,
    all_obs            = .data$All.Obs.Reported,
    breed_code         = .data$Breeding.Code,
    checklist_comments = .data$Checklist.Comments,
    species_comments   = .data$Observation.Details
  )
}

#' Removes subspecies, subgroup, or domestic type designations from the common
#' and scienfic name columns of an ebird csv
#'
#' @inheritParams rename_ebird_data
rename_subspecies <- function(ebird) {
  dplyr::mutate(dt,
    # dropping subspecies or domestic type designations
    sci_name = stringr::word(sci_name, 1, 2),
    # dropping subspecies and subgroup designations
    common_name = stringr::word(common_name, 1, sep = stringr::fixed(" ("))
  )
}

#' Filters ebird csv export.
#'
#' @inheritParams rename_ebird_data
filter_ebird_data <- function(ebird) {
  dplyr::filter(
    ebird,
    # remove highly non-specific observations.
    .data$sci_name != "Passeriformes sp."
  )
}

#' A basic set of import integrity checks
#' These do not check the validity of the data.
#' @param dt ebird data.frame
#' @importFrom glue glue
#' @importFrom dplyr distinct summarise mutate group_by pull filter
#' @keywords internal
run_import_checks <- function(dt) {
  # Check for missing values where there shouldn't be.
  purrr::walk(
    .x = c("date", "mbbs_county", "route_num", "count"),
    .f = ~ {
      assertthat::assert_that(
        !anyNA(dt[[.x]]),
        msg = sprintf("Found NA %s values; there shouldn't be.", .x)
      )
    }
  )

  # Check for >1 county in an ebird import
  assertthat::assert_that(
    length(unique(dt$mbbs_county)) == 1,
    msg = "The ebird dt should contain only a single MBBS county's data."
  )

  # Check that routes have exactly 1 or 20 non-owling submissions.
  # TODO: 2020 and after should have 20 submissions
  dt %>%
    distinct(.data$year, .data$mbbs_county, .data$route_num, .data$stop_num) %>%
    group_by(.data$year, .data$mbbs_county, .data$route_num) %>%
    dplyr::summarise(
      n = dplyr::n(),
      flag = !(.data$n %in% c(1, 20))
    ) %>%
    {
      x <- .
      probs <-
        x[x$flag, ] %>%
        mutate(
          desc = glue::glue("{mbbs_county}, {year}, {route_num}")
        ) %>%
        pull(.data$desc) %>%
        paste0(collapse = "\n * ")

      if (any(x$flag)) {
        warning(sprintf(
          "The following year/route don't have either 1 or 20 checklists:\n %s",
          probs
        ))
      }
    }

  dt
}

#' Get exclusions
#' @return a character vector of submission ids to exclude
get_exclusions <- function() {
  yaml::read_yaml(config$excluded_submissions)
}

#' Exclude submissions
#'
#' @param exclusions character vector of submission ids to
#' @inheritParams rename_ebird_data
#' @importFrom  dplyr filter
#' @return a `data.frame` without the exclusions
#' @export
exclude_submissions <- function(ebird, exclusions) {
  dplyr::filter(ebird, !(.data$sub_id %in% exclusions))
}


#' Import an ebird export into R
#' @param path path/to/ebird_export.csv
#' @param run_checks run integrity checks or not?
#' @importFrom dplyr left_join as_tibble if_else mutate select
#' @importFrom stringr str_match
#' @importFrom utils read.csv
#' @export
import_ebird_data <- function(path, run_checks = TRUE) {
  read.csv(path, stringsAsFactors = FALSE) %>%
    # TODO: revisit read_csv; can't get past parse errors.
    rename_ebird_data() %>%
    rename_subspecies() %>%
    filter_ebird_data() %>%
    ## Process comments ##
    left_join(
      comment_workflow(.),
      by = c("sub_id")
    ) %>%
    mutate(
      # See: https://github.com/nc-minibbs/mbbs/issues/14
      count = if_else(.data$count_raw == "X", "1", .data$count_raw),
      count = as.integer(.data$count),
      date = lubridate::ymd(.data$date),
      year = lubridate::year(.data$date),

      # Get county from location and clean up.
      mbbs_county = str_extract(.data$loc, "[Oo]range|[Cc]hatham|Chatman|[Dd]urham"),
      mbbs_county = str_replace(.data$mbbs_county, "Chatman", "Chatham"),
      mbbs_county = tolower(.data$mbbs_county),
      route_num = as.integer(str_match(.data$loc, "[0-1]{0,1}[0-9]{1}")),
      # Getting stop from numbers at end (this is fragile):
      stop_num = as.integer(str_extract(.data$loc, "([0-9]{1,2}$)")),

      # TODO: Flag the pre-dawn "owling" submissions
      # is_owling =
    ) %>%
    exclude_submissions(get_exclusions()) %>%
    {
      x <- .
      if (run_checks) {
        x %>% run_import_checks()
      } else {
        x
      }
    } %>%
    as_tibble()
}
