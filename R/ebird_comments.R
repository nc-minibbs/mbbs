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
    # Add additional as needed
    identity()
}

#' Create a function for extracting data in the eBird comments field
#'
#' @param field_pattern a regex defining the valid pattern for the name of the
#'      field (e.g. observer) from eBird comments
#' @param data_pattern a regex defining the valid pattern for the field's data
#' @param delimiter the delimiter between field: data entries. Defaults to ";"
#' @param post a `function(x, ...)` to apply to the resulting strings
#' @return a `function(comments, ...)` where `comments` is a ``character` vector
#'    of MBBS checklist comments and returns a `list` of strings where data has
#'    been extracted according to the provided regular expression.
#' @importFrom stringr str_extract_all
make_comment_extractor <- function(field_pattern, data_pattern, delimiter = ";",
                                   post = function(x, ...) {
                                     ifelse(length(x) == 0, NA_character_, x)
                                   }) {
  valid_pattern <- sprintf(
    "(?<=(%s==){1}\\s{0,5})%s(?=%s)",
    field_pattern, data_pattern, delimiter
  )

  function(comments, ...) {
    stringr::str_extract_all(comments, pattern = valid_pattern) |>
      purrr::map(~ post(.x, ...))
  }
}

#' Extract observer from eBird comments
#' @inheritParams clean_comments
#' @param ... additional arguments passed to the `post` function
extract_observers <- make_comment_extractor("observer(s)?", "[A-Za-z\\s,]*")

#' Extract vehicles from eBird comments
#' @inheritParams clean_comments
#' @param ... additional arguments passed to the `post` function
extract_vehicles <- make_comment_extractor("vehicle(s)?", "[\\d]*")

#' Extract notes from eBird comments
#' @inheritParams clean_comments
#' @param ... additional arguments passed to the `post` function
extract_weather <- make_comment_extractor("weather", "[A-Za-z\\s\\d,]*")

# Extract notes from eBird comments
# @export
# TODO: habitat is a bit complicated
# extract_habitat <- make_comment_extractor("habitat(@\\d{1,2}(R|L))?", "[BHMPSOW\\s\\d,]+")

#' Extract notes from eBird comments
#' @inheritParams clean_comments
#' @param ... additional arguments passed to the `post` function
extract_notes <- make_comment_extractor("note(s)?", "[A-Za-z\\s\\d,]*")

#' Workflow for preprocessing eBird comments
#' @inheritParams clean_comments
#' @importFrom magrittr "%>%"
preprocess_comments <- function(comments) {
  comments |>
    clean_comments() |>
    identity() # replace with addition steps as need
}

# Workaround for "Undefined global functions or variables" CRAN check
globalVariables(c(
  ".",
  "sub_id",
  "checklist_comments",
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
  ebird %>%
    dplyr::distinct(
      "sub_id",
      "checklist_comments"
    ) %>%
    dplyr::mutate(
      checklist_comments |>
        preprocess_comments() |>
        process_comments() |>
        postprocess_comments()
    ) %>%
    dplyr::select(- "checklist_comments")
}
