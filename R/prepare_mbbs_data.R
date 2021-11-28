#' Gets the ebird taxonomy dataset
#' @importFrom readr read_csv
#' @export
get_ebird_taxonomy <- function() {
  readr::read_csv("https://www.birds.cornell.edu/clementschecklist/wp-content/uploads/2021/08/eBird_Taxonomy_v2021.csv") %>%
    select(
      tax_order = TAXON_ORDER,
      sci_name = SCI_NAME,
      common_name = PRIMARY_COM_NAME
    )
}

#' Clean the species names of the data scraped from the old MBBS website
#' 
#' @param dt a data.frame
#' @importFrom dplyr everything
clean_common_names <- function(dt) {
  dt %>%
    mutate(
      common_name_c = trimws(common_name),
      common_name_c = str_replace(common_name_c, "\\n", " "),
      common_name_c = str_replace(common_name_c, "  ", " "),
      common_name_c = str_replace(common_name_c, "^Am |^Amer ", "American "),
      common_name_c = str_replace(common_name_c, "Fc$", "Flycatcher"),
      common_name_c = str_replace(common_name_c, "B-g", "Blue-gray"),
      common_name_c = str_replace(common_name_c, "^Br-", "Brown-"),
      common_name_c = str_replace(common_name_c, "Nuth$", "Nuthatch"),
      common_name_c = str_replace(common_name_c, "^Car ", "Carolina "),
      common_name_c = str_replace(common_name_c, "^Com ", "Common "),
      common_name_c = str_replace(common_name_c, "^Double-cr ", "Double-crested "),   
      common_name_c = str_replace(common_name_c, "^E ", "Eastern "),
      common_name_c = str_replace(common_name_c, "^Eur ", "European "),
      common_name_c = str_replace(common_name_c, "^Gr ", "Great "),
      common_name_c = str_replace(common_name_c, "^La ", "Louisiana "),
      common_name_c = str_replace(common_name_c, "^N ", "Northern "),
      common_name_c = str_replace(common_name_c, "Nutchatch", "Nuthatch"),
      common_name_c = str_replace(common_name_c, "^Red-wing ", "Red-winged "),
      common_name_c = str_replace(common_name_c, "^Ruby-thr ", "Ruby-throated "),
      common_name_c = str_replace(common_name_c, "Sparow", "Sparrow"),
      common_name_c = str_replace(common_name_c, "Swal$", "Swallow"),
      common_name_c = str_replace(common_name_c, "Swall$", "Swallow"),
      common_name_c = str_replace(common_name_c, "Swallo$", "Swallow"),
      common_name_c = str_replace(common_name_c, "^Yellow-thr ", "Yellow-throated "),
      common_name_c = str_replace(common_name_c, "^White-br ", "White-breasted "),
      common_name_c = str_replace(common_name_c, "-Poor-Will", "-poor-will"),
      common_name_c = str_replace(common_name_c, "Will's-Widow", "will's-widow"),
      common_name_c = case_when(
        common_name_c == "Accipiter species" ~ "Accipiter sp.",
        common_name_c == "Rock Dove" ~ "Rock Pigeon",
        common_name_c == "unident. duck" ~ "duck sp.",
        common_name_c == "Unidentified Accipiter Hawk" ~ "Accipiter sp.",
        common_name_c == "Unidentified Accipter" ~ "Accipiter sp.",
        common_name_c == "unidentified hawk" ~ "hawk sp.",
        common_name_c == "Whip-poor-will"   ~ "Eastern Whip-poor-will",
        TRUE  ~ common_name_c
      )
    ) %>%
    select(-common_name, -spec_code) %>%
    select(common_name = common_name_c, everything())
}

#' Add route info to data scraped from old MBBS site
#' 
#' @param dt a data.frame
#' @param county the name of the county to merge
add_route_info <- function(dt, county){
  dt %>%
    mutate(route_num = as.integer(route_num)) %>%
    select(-route) %>%
    left_join(filter(mbbs_routes, mbbs_county == county), by = "route_num")
  
}

#' A function that combines and aligns MBBS data scraped from the old website
#' with ebird data.
#' 
#' This is one big function that does a lot of data munging!
#' 
#' @param mbbs_site_dt a `data.frame` containing MBBS data scraped from the old
#'    website for a single county.
#' @param ebird_dt a `data.frame` imported using `import_ebird_data` for a single
#'    county
#' @param ebird_taxonomy a `data.frame` containing common and scientific names
#'    of ebird taxonomy
#' @importFrom dplyr case_when row_number full_join right_join
#' @return a `list` containing `site` (processed "website" data) and `ebird` 
#'    (processed ebird data)
#' @export
prepare_mbbs_data <- function(ebird_dt, mbbs_site_dt, ebird_taxonomy){
  
  mbbs_county <- unique(ebird_dt$mbbs_county)
  
  if (is.null(mbbs_site_dt) ){
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
      ebird_taxonomy, by = "common_name"
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
combine_site_ebird <- function(x, at_year = 2009){
  if (nrow(x$site) == 0 || is.null(x$site)) { 
    return(
      x$ebird %>% filter(year >= !! at_year)
    )
  }
  
  bind_rows(
    x$ebird %>% filter(year >= !! at_year),
    x$site  %>% filter(year <  !! at_year)  
  )
}