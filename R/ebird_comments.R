#------------------------------------------------------------------------------#
# Functions/workflows for cleaning and processing comments
# in eBird data
#------------------------------------------------------------------------------#

#' Clean up the eBird comments
#' @param comments a `character` vector of eBird comments
#' @importFrom stringr str_replace_all
clean_comments <- function(comments) {
  # change unicode equals and = to ==
  stringr::str_replace_all(comments, "&#61;|=", "==") |>
    trimws() |>
    # Add additional as needed
    identity()
}

#' Split comment ebird comments at ";"
#' @param comments `ebird` comments
split_comments <- function(comments) {
  stringr::str_split(comments, ";") |>
    purrr::map(~ unique(trimws(.x)))
}

#' Workflow for preprocessing eBird comments
#' @inheritParams clean_comments
#' @importFrom magrittr "%>%"
preprocess_comments <- function(comments) {
  comments |>
    clean_comments() |>
    split_comments() |>
    identity() # replace with addition steps as need
}

#' Parser specification for ebird checklist comments
comment_spec <- list(
  habitat = list(
    field = "([Hh]abitat|[Hh])\\s{0,5}==",
    data = "(?<=([Hh]abitat|[Hh])\\s{0,5}==).*",
    post = trimws,
    default = NA_character_
  ),
  notes = list(
    field = "^note(s)?==",
    data = "(?<=(note(s)?==)).*",
    post = \(x) x,
    default = NA_character_
  ),
  observers = list(
    field = "^[Oo]bserver(s)?\\s{0,5}==",
    data = "(?<=([Oo]bserver(s)?\\s{0,5}==)).*",
    post = \(x) list(trimws(stringr::str_split_1(x, ","))),
    default = list(NA_character_)
  ),
  vehicles = list(
    field = "^([Vv]ehicle(s)?|[Vv]|[Cc]ars|vehicles passing|TOTAL VEHICLES PASSING)\\s{0,5}==",
    data = "[\\d]{1,3}",
    post = as.integer,
    default = NA_integer_
  ),
  weather = list(
    field = "^[Ww]eather\\s{0,5}==",
    data = "(?<=([Ww]eather\\s{0,5}==)).*",
    post = trimws,
    default = NA_character_
  )
)

#' Parse a *single* submission comment
#'
#' NOTE: This only does an *initial* parsing of habitat data;
#'       i.e. it only extracts data to the right of (e.g.) "habitat==".
#'
#' @param x character vector of `ebird` of *single* submission's comments
parse_single_comment <- \(x) {
  purrr::imap(
    .x = comment_spec,
    .f = ~ {
      hold <- stringr::str_subset(x, .x$field)

      assertthat::assert_that(
        length(hold) <= 1,
        msg = glue::glue("Detected >1 entry for {field} in comment", field = .y)
      )

      `if`(
        length(hold > 0),
        .x$post(str_extract(hold, .x$data)),
        .x$default
      )
    }
  )
}

#' Parse all stop-level comments
#' @param x list of `ebird` comments
parse_comments <- \(x) {
  purrr::map(x, parse_single_comment)
}

#' PostProcess comments
#' @param x list of list(vehicle, ...) obtained after `parse_comments`
postprocess_comments <- \(x) {
  purrr::map_dfr(x, tibble::as_tibble_row) |>
    mutate(
      obs1 = trimws(purrr::map_chr(observers, ~ .x[1])),
      obs2 = trimws(purrr::map_chr(observers, ~ .x[2])),
      obs3 = trimws(purrr::map_chr(observers, ~ .x[3]))
    ) |>
    select(-observers)
}

#' Full workflow for processing comments
#' @param ebird a `data.frame` of imported eBird counts
#' @importFrom dplyr distinct mutate select distinct
#' @return a `data.frame` with one row per submission ID in `ebird`
#' @export
comment_workflow <- function(ebird) {
  ebird |>
    dplyr::distinct(submission, date, year, route, stop_num, comments) |>
    dplyr::mutate(
      comments |>
        preprocess_comments() |>
        parse_comments() |>
        postprocess_comments()
    ) |>
    # Flag protocol violations
    group_by(route, date) |>
    mutate(
      violation =
      # Any of the following
      # * Survey not within valid date range
        !(valid_date_range(date)) |
          # * not all the submissions on the same date
          !(all(date == date[1]))
    ) |>
    ungroup()
}
