#' Gets the ebird taxonomy dataset
#' @importFrom dplyr select
#' @importFrom stringr str_sub
#' @export
get_ebird_taxonomy <- function() {
  # select the file that's from the lastest version year (ie: ebird_taxonomy_v2022)
  latest_taxonomy <-
    list.files("inst/taxonomy") %>%
    max(stringr::str_sub(-8, -5))
  read.csv(paste("inst/taxonomy/", latest_taxonomy, sep = "")) %>%
    dplyr::select(
      tax_order = .data$TAXON_ORDER,
      sci_name = .data$SCI_NAME,
      common_name = .data$PRIMARY_COM_NAME
    )
}

#' Clean the species names of the data scraped from the old MBBS website
#'
#' @param dt a data.frame
#' @importFrom dplyr everything
clean_common_names <- function(dt) {
  dt %>%
    mutate(
      common_name_c = trimws(.data$common_name),
      common_name_c = str_replace_all(
        .data$common_name_c,
        c(
          "\\n" = " ",
          "  " = " ",
          "^Am |^Amer " = "American ",
          "Fc$" = "Flycatcher",
          "B-g" = "Blue-gray",
          "^Br-" = "Brown-",
          "Nuth$" = "Nuthatch",
          "^Car " = "Carolina ",
          "^Com " = "Common ",
          "^Double-cr " = "Double-crested ",
          "^E " = "Eastern ",
          "^Eur " = "European ",
          "^Gr " = "Great ",
          "^La " = "Louisiana ",
          "^N " = "Northern ",
          "Nutchatch" = "Nuthatch",
          "^Red-wing " = "Red-winged ",
          "^Ruby-thr " = "Ruby-throated ",
          "Sparow" = "Sparrow",
          "Swal$" = "Swallow",
          "Swall$" = "Swallow",
          "Swallo$" = "Swallow",
          "^Yellow-thr " = "Yellow-throated ",
          "^White-br " = "White-breasted ",
          "-Poor-Will" = "-poor-will",
          "Will's-Widow" = "will's-widow",
          "Great Horned \r Owl" = "Great Horned Owl",
          "House\r Wren" = "House Wren"
        )
      ),
      common_name_c = case_when(
        common_name_c == "Accipiter species" ~ "Accipiter sp.",
        common_name_c == "Rock Dove" ~ "Rock Pigeon",
        common_name_c == "unident. duck" ~ "duck sp.",
        common_name_c == "Unidentified Accipiter Hawk" ~ "Accipiter sp.",
        common_name_c == "Unidentified Accipter" ~ "Accipiter sp.",
        common_name_c == "unidentified hawk" ~ "hawk sp.",
        common_name_c == "Whip-poor-will" ~ "Eastern Whip-poor-will",
        TRUE ~ common_name_c
      )
    ) %>%
    select(-.data$common_name, -.data$spec_code) %>%
    select(common_name = .data$common_name_c, everything())
}

#' Add route info to data scraped from old MBBS site
#'
#' @param dt a data.frame
#' @param county the name of the county to merge
add_route_info <- function(dt, county) {
  dt %>%
    mutate(route_num = as.integer(.data$route_num)) %>%
    select(-.data$route) %>%
    left_join(
      filter(mbbs_routes, .data$mbbs_county == county),
      by = "route_num"
    )
}

#' A function that combines and aligns MBBS data scraped from the old website
#' with ebird data.
#'
#' This is one big function that does a lot of data munging!
#'
#' @param mbbs_site_dt a `data.frame` containing MBBS data scraped from the old
#'    website for a single county.
#' @param ebird_dt a `data.frame` imported using `import_ebird_data`
#'    for a single county
#' @param ebird_taxonomy a `data.frame` containing common and scientific names
#'    of ebird taxonomy
#' @importFrom dplyr case_when row_number full_join right_join
#' @return a `list` containing `site` (processed "website" data) and `ebird`
#'    (processed ebird data)
#' @export
prepare_mbbs_data <- function(ebird_dt, mbbs_site_dt, ebird_taxonomy) {
  mbbs_county <- unique(ebird_dt$mbbs_county)

  if (is.null(mbbs_site_dt)) {
    return(
      list(
        site = data.frame(),
        ebird = ebird_dt
      )
    )
  }

  mbbs_site_dt <-
    mbbs_site_dt %>%
    clean_common_names() %>%
    left_join(
      ebird_taxonomy,
      by = "common_name"
    ) %>%
    add_route_info(county = mbbs_county) %>%
    mutate(
      source = "preebird",
      mbbs_county = mbbs_county
    )

  list(
    site = mbbs_site_dt,
    ebird = ebird_dt
  )
}

#' Combine MBBS site data with eBird data
#'
#' @param x the output of `prepare_mbbs_data`
#' @param at_year the year which to begin using ebird data
#' @importFrom dplyr bind_rows
#' @export
combine_site_ebird <- function(x, at_year = 2010) {
  if (nrow(x$site) == 0 || is.null(x$site)) {
    return(
      x$ebird %>% filter(.data$year >= !!at_year)
    )
  }

  bind_rows(
    x$ebird %>% filter(.data$year >= !!at_year),
    x$site %>% filter(.data$year < !!at_year)
  )
}

#' Add route_ID column
#'
#' @param x the output of combine_site_ebird
#' @importFrom dplyr mutate case_when
mbbs_generate_route_ID <- function(x) {
  x <- x %>%
    mutate(route_ID = .data$route_num + case_when(
      .data$mbbs_county == "orange" ~ 100L,
      .data$mbbs_county == "durham" ~ 200L,
      .data$mbbs_county == "chatham" ~ 300L
    ))
}
