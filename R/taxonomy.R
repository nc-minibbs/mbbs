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
    )
}
