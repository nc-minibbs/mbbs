#------------------------------------------------------------------------------#
# R code related to taxonomy used in the MBBS project
#------------------------------------------------------------------------------#

#' Get the ebird taxonomy dataset
#'
#' Gets the most recent ebird taxonomy dataset available in the project repo.
#' NOTE: to update the taxonomy to the latest version of the eBird taxonomy,
#' files must be manually downloaded.
#' See revelant information in `docs/data-pipeline.md`.
#'
#' @importFrom dplyr select
#' @importFrom stringr str_extract
#' @include config.R
get_ebird_taxonomy <- function(path = config$taxonomy_data_dir) {
  list.files(path) |>
    (\(x) x[which.max(as.integer(stringr::str_extract(x, "\\d{4}")))])() |>
    (\(x) file.path(path, x))() |>
    read.csv() |>
    dplyr::select(
      sci_name = "SCI_NAME",
      common_name = "PRIMARY_COM_NAME"
    ) |>
    mutate(
      common_name = dplyr::case_when(
        common_name == "Accipitrine hawk sp. (former Accipiter sp.)" ~
          "Accipitrine hawk sp.",
        TRUE ~ common_name
      )
    )
}

#' Conform taxonomy of MBBS data from different sources
#' to the eBird taxonomy
#'
#' @param df a dataset with a `common_name` field
#' @param taxonomy data.frame from `get_ebird_taxonomy`
conform_taxonomy <- function(df, taxonomy) {
  df <-
    df |>
    mutate(
      common_name = dplyr::case_when(
        common_name == "House Wren" ~ "Northern House Wren",
        # common_name == "Accipiter sp." ~ "Sharp-shinned/Cooper's Hawk",
        common_name == "Accipiter sp." ~ "Accipitrine hawk sp.",
        TRUE ~ common_name
      )
    )

  diff <- setdiff(unique(df$common_name), taxonomy$common_name)

  assertthat::assert_that(
    all(unique(df$common_name) %in% taxonomy$common_name),
    msg = paste(
      paste(diff, collapse = " "),
      "are in df but not in taxonomy"
    )
  )

  df
}

#' Add scientific name
#'
#' @param df a dataset with a `common_name` field
#' @param taxonomy data.frame from `get_ebird_taxonomy`
add_sci_name <- function(df, taxonomy) {
  dplyr::left_join(
    df,
    taxonomy,
    by = c("common_name")
  ) |>
    (\(x){
      assertthat::assert_that(
        nrow(df) == nrow(x),
        msg = "Data was lost when conforming taxonomy. This is bad."
      )
      x
    })()
}
