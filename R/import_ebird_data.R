
#' Renames and selects columns from data.frame created from the ebird csv export.
#'
#' @param dt a data.frame imported from an ebird export.
#' @export
rename_ebird_data <- function(dt) {
  dplyr::select(
    dt,
    sub_id      = Submission.ID,
    common_name = Common.Name,
    sci_name    = Scientific.Name,
    tax_order   = Taxonomic.Order,
    count_raw   = Count,
    state       = State.Province,
    county      = County,
    loc         = Location,
    locid       = Location.ID,
    lat         = Latitude,
    lon         = Longitude,
    date        = Date,
    time        = Time,
    protocol    = Protocol,
    distance_traveled = Distance.Traveled..km.,
    area_covered = Area.Covered..ha.,
    all_obs     = All.Obs.Reported,
    breed_code  = Breeding.Code,
    checklist_comments = Checklist.Comments
  )
}

#' Filters ebird csv export.
#'
#' @param dt a data.frame imported from an ebird export.
#' @export
filter_ebird_data <- function(dt) {
  dt %>%
    dplyr::filter(
      # remove highly non-specific observations.
      sci_name != "Passeriformes sp."
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
    distinct(year, mbbs_county, route_num, stop_num) %>%
    group_by(year, mbbs_county, route_num) %>%
    dplyr::summarise(
      n = dplyr::n(),
      flag = !(n %in% c(1, 20))
    ) %>%
    {
      x <- .
      probs <-
        x[x$flag, ] %>%
        mutate(
          desc = glue::glue("{mbbs_county}, {year}, {route_num}")
        ) %>%
        pull(desc) %>%
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
#' @param path path/to/exclusions_map.yml
#' @return a character vector of submission ids to exclude
#' @export
get_exclusions <- function(path = "inst/excluded_submissions.yml") {
  yaml::read_yaml(path)
}

#' Exclude submissions
#'
#' @param dt ebird `data.frame`
#' @param exclusions character vector of submission ids to
#' @importFrom  dplyr filter
#' @importFrom stringr str_extract str_replace
#' @return a `data.frame` without the exclusions
#' @export
exclude_submissions <- function(dt, exclusions) {
  dt %>%
    filter(!(sub_id %in% exclusions))
}

#' Import an ebird export into R
#' @param path path/to/ebird_export.csv
#' @param run_checks run integrity checks or not?
#' @importFrom dplyr left_join as_tibble if_else mutate
#' @importFrom stringr str_match
#' @importFrom utils read.csv
#' @export
import_ebird_data <- function(path, run_checks = TRUE) {
  read.csv(path, stringsAsFactors = FALSE) %>%
    # TODO: revisit read_csv; can't get past parse errors.
    rename_ebird_data() %>%
    filter_ebird_data() %>%
    ## Process comments ##
    left_join(
      comment_workflow(.),
      by = c("sub_id")
    ) %>%
    mutate(
      # See: https://github.com/nc-minibbs/mbbs/issues/14
      count = if_else(count_raw == "X", "1", count_raw),
      count = as.integer(count),
      date = lubridate::ymd(date),
      year = lubridate::year(date),

      # Get county from location and clean up.
      mbbs_county = str_extract(loc, "[Oo]range|[Cc]hatham|Chatman|[Dd]urham"),
      mbbs_county = str_replace(mbbs_county, "Chatman", "Chatham"),
      mbbs_county = tolower(mbbs_county),
      route_num = as.integer(str_match(loc, "[0-1]{0,1}[0-9]{1}")),
      # Getting stop from numbers at end (this is fragile):
      stop_num = as.integer(str_extract(loc, "([0-9]{1,2}$)")),

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
