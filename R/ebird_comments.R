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

#' Split comment ebird comment data
#' into those with stop-level and those without
#' @param ebird `ebird` data
split_comment_cases <- function(ebird) {
  ebird |>
    dplyr::group_split(
      stop_level = !is.na(stop_num)
    )
}

#' Split comment ebird comments at ";"
#' @param comments `ebird` comments
split_comments <- function(comments) {
  stringr::str_split(comments, ";") |>
    purrr::map(trimws)
}

#' Workflow for preprocessing eBird comments
#' @inheritParams clean_comments
#' @importFrom magrittr "%>%"
preprocess_comments <- function(comments) {
  comments |>
    clean_comments() |>
    identity() # replace with addition steps as need
}

#' Parser specification for ebird stop-level checklist comments
#' Used by `parse_stop_level_comment`
stop_level_spec <- list(
  habitat = list(
    field = "([Hh]abitat|[Hh])\\s{0,5}==",
    data = "(?<=([Hh]abitat|[Hh]\\s{0,5}==)).*",
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
    post = \(x) trimws(stringr::str_split_1(x, ",")),
    default = NA_character_
  ),
  vehicles = list(
    field = "^([Vv]ehicle(s)?|[Vv]|[Cc]ars)\\s{0,5}==",
    data = "[\\d]",
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

#' Parse a *single* stop-level comment
#' @param x character vector of `ebird` of *single* submission's comments
parse_single_stop_comment <- \(x) {
  purrr::imap(
    .x = stop_level_spec,
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
parse_stop_comments <- \(x) {
  purrr::map(x, parse_single_stop_comment)
}

# Workaround for "Undefined global functions or variables" CRAN check
globalVariables(c(
  ".",
  "sub_id",
  "comments",
  "vehicles"
))

#' Workflow for processing eBird comments
#' @inheritParams clean_comments
#' @importFrom magrittr "%>%"
#' @importFrom purrr transpose map_dfr
#' @importFrom dplyr as_tibble
process_comments <- function(comments) {
  comments %>%
    {
      dt <- .
      purrr::map(
        .x =
          list(
            observers = extract_observers,
            vehicles  = extract_vehicles,
            weather   = extract_weather,
            notes     = extract_notes
          ),
        .f = ~ .x(dt)
      )
    } %>%
    purrr::transpose() %>%
    purrr::map_dfr(dplyr::as_tibble) %>%
    identity() # Add additional steps as needed
}

#' Workflow for postprocessing eBird comments
#' @inheritParams clean_comments
#' @importFrom dplyr mutate
postprocess_comments <- function(comments) {
  comments |>
    dplyr::mutate(
      vehicles = as.integer(vehicles)
    ) %>%
    identity() # Add additional steps as needed
}

#' Full workflow for processing comments
#' @param ebird a `data.frame` of imported eBird counts
#' @importFrom dplyr distinct mutate select distinct
#' @return a `data.frame` with one row per submission ID in `ebird`
#' @export
comment_workflow <- function(ebird) {
  ebird |>
    dplyr::distinct(
      "submission",
      "comments"
    ) |>
    dplyr::mutate(
      comments |>
        preprocess_comments() |>
        process_comments() |>
        postprocess_comments()
    ) |>
    dplyr::select(-"comments")
}
