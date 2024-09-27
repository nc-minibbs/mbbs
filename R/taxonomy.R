#' Gets the ebird taxonomy dataset
#'
#' Gets the most recent ebird taxonomy dataset available in the project repo.ger
#'
#' @importFrom dplyr select
#' @importFrom stringr str_extract
#' @include config.R
get_ebird_taxonomy <- function() {
  list.files(config$taxonomy_data_dir) |>
    (\(x) x[which.max(as.integer(stringr::str_extract(x, "\\d{4}")))])() |>
    (\(x) file.path(config$taxonomy_data_dir, x))() |>
    (\(x) system.file(x, package = "mbbs"))() |>
    read.csv() |>
    dplyr::select(
      tax_order = "TAXON_ORDER",
      sci_name = "SCI_NAME",
      common_name = "PRIMARY_COM_NAME"
    )
}
