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
  purrr::map_dfr(x, tibble::as_tibble_row)
}

#'
#' @include utilities.R
parse_habitat_stop_level <- \(submission, habitat) {
  habitat |>
    toupper() |>
    stringr::str_extract_all(habitat_codes) |>
    unlist() |>
    unique()
}

parse_habitat_route_level <- \(submission, habitat) {
  habitat |>
    # Extract all number followed by habitat code
    str_extract_all("\\d+|B|H|M|P|S|O|W") |>
    # Concatenate them all
    unlist() |> paste0(collapse = "") |>
    # Split at the (stop) numbers
    str_split_1("\\d+") |>
    # Remove empty strings
    (\(x) x[ x != ""])() |>
    (\(x) {
      if(length(x) != 20) {
        print(habitat)
        logger::log_error(
          "{submission}: habitat comments do not parse to 20 stops"
        )
        return(list(error  = x, result = NULL))
      }
      out <-
        vapply(
            x,
            FUN = \(x) { paste0(str_unique(str_split_1(x, "")), collapse = "") },
            FUN.VALUE = character(1),
            USE.NAMES = FALSE)

      list(error = NULL, result = out)
    })()
}

#'
parse_habitat <- \(submission, stop_num, habitat) {
  purrr::pmap(
    .l = list(s = submission, stop = stop_num, habitat = habitat),
    .f = \(s, stop, habitat) {
      if (is.na(habitat) || habitat %in% c("no change", "no changes", "unchanged")) {
        return(habitat)
      }

      `if`(
        is.na(stop),
        parse_habitat_route_level(submission = s, habitat = habitat),
        `if`(
          stop == 1 && nchar(habitat) > 10,
          parse_habitat_route_level(submission = s, habitat = habitat),
          parse_habitat_stop_level(submission = s, habitat = habitat)
        )
      )
    }
  )
}

#' Full workflow for processing comments
#' @param ebird a `data.frame` of imported eBird counts
#' @importFrom dplyr distinct mutate select distinct
#' @return a `data.frame` with one row per submission ID in `ebird`
#' @export
comment_workflow <- function(ebird) {
  ebird |>
    dplyr::distinct(submission, year, route, stop_num, comments) |>
    dplyr::mutate(
      comments |>
        preprocess_comments() |>
        parse_comments() |>
        postprocess_comments()
    )
}
