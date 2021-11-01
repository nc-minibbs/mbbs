#' A function that combines and aligns MBBS data scraped from the old website
#' with ebird data.
#' 
#' This is one big function that does a lot of data munging!
#' 
#' @param mbbs_site_dt a `data.frame` containing MBBS data scraped from the old
#'    website for a single county.
#' @param ebird_dt a `data.frame` imported using `import_ebird_data` for a single
#'    county
#' @importFrom dplyr case_when row_number full_join right_join
#' @return a `list` containing `site` (processed "website" data) and `ebird` 
#'    (processed ebird data)
#' @export
prepare_mbbs_data <- function(ebird_dt, mbbs_site_dt){
  
  # browser()
  mbbs_county <- unique(ebird_dt$mbbs_county)
  
  if (is.null(mbbs_site_dt) ){
    return(
      list(
        site = data.frame(),
        ebird = ebird_dt
      )
    )
  }
  
  ## 1. HARD CODE Clean up ####
  mbbs_site_dt <- 
    mbbs_site_dt %>%
    mutate(
      common_name = case_when(
        # 2001 route 6 records "White-br Nuthatch" for two different WBNU and BHNU
        # Changing the BHNU rocord to the correct common name
        year == 2001 & common_name == "White-br Nuthatch" & spec_code == "BHNU" ~ "Brown-headed Nuthatch",
        
        # The following two changes assume that the species code is correct
        year == 2001 & common_name == "Scarlet Tanager"  &  spec_code == "SUTA" ~ "Summer Tanager",
        year == 2003 & common_name == "Wood Duck" & spec_code == "CAGO" ~ "Canada Goose",
        TRUE ~ common_name)
    )

  ## 2. Align species names and codes ####
  
  ebird_names <- 
    ebird_dt %>%
    distinct(common_name, sci_name, tax_order)
  
  all_mbbs_names <- 
    mbbs_site_dt %>%
    distinct(common_name, spec_code) %>%
    # Do Clean up to dedupe within mbbs names
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
      common_name_c = str_replace(common_name_c, "^E ", "Eastern "),
      common_name_c = str_replace(common_name_c, "^Eur ", "European "),
      common_name_c = str_replace(common_name_c, "^Gr ", "Great "),
      common_name_c = str_replace(common_name_c, "^La ", "Louisiana "),
      common_name_c = str_replace(common_name_c, "^N ", "Northern "),
      common_name_c = str_replace(common_name_c, "^Red-wing ", "Red-winged "),
      common_name_c = str_replace(common_name_c, "^Ruby-thr ", "Ruby-throated "),
      common_name_c = str_replace(common_name_c, "Sparow", "Sparrow"),
      common_name_c = str_replace(common_name_c, "Swal$", "Swallow"),
      common_name_c = str_replace(common_name_c, "Swall$", "Swallow"),
      common_name_c = str_replace(common_name_c, "Swallo$", "Swallow"),
      common_name_c = str_replace(common_name_c, "^Yellow-thr ", "Yellow-throated "),
      common_name_c = str_replace(common_name_c, "^White-br ", "White-breasted "),
      common_name_c = str_replace(common_name_c, "-Poor-Will", "-poor-will"),
      common_name_c = str_replace(common_name_c, "Will's-Widow", "will's-widow")
    ) %>%
    # Adjust common names to match ebird common name
    mutate(
      common_name_c = case_when(
        common_name_c == "Accipiter species" ~ "hawk sp.",
        common_name_c == "unidentified hawk" ~ "hawk sp.",
        common_name_c == "Rock Pigeon"      ~ "Rock Pigeon (Feral Pigeon)",
        common_name_c == "Rock Dove"        ~ "Rock Pigeon (Feral Pigeon)",
        common_name_c == "Whip-poor-will"   ~ "Eastern Whip-poor-will",
        TRUE  ~ common_name_c
      )
    ) %>%
    arrange(common_name_c)

  # Remove any duplicated common names that have entries both with and without codes
  cleaned_mbbs_names <- 
    all_mbbs_names %>%
    distinct(common_name_c, spec_code) %>%
    mutate(missing_spec = is.na(spec_code) * 1) %>%
    group_by(common_name_c) %>%
    arrange(common_name_c, missing_spec) %>%
    filter(row_number() == 1) %>%
    ## Add missing species codes
    mutate(
      spec_code_c = case_when(
        # Getting codes from 
        # ~~http://www.wec.ufl.edu/birds/SurveyDocs/species_list.pdf~~
        # https://www.birdpop.org/docs/misc/Alpha_codes_eng.pdf
        common_name_c == "American Woodcock" ~ "AMWO",
        common_name_c == "Black-and-white Warbler" ~ "BAWW",
        common_name_c == "Blue-headed Vireo" ~ "BHVI",
        common_name_c == "Broad-winged Hawk" ~ "BWHA",
        common_name_c == "Cliff Swallow" ~ "CLSW",
        common_name_c == "Common Nighthawk"  ~ "CONI",
        common_name_c == "Dickcissel" ~ "DICK",
        common_name_c == "Double-crested Cormorant" ~ "DCCO",
        common_name_c == "Eastern Screech-Owl" ~ "EASO",
        common_name_c == "Glossy Ibis" ~ "GLIB",
        common_name_c == "Loggerhead Shrike" ~ "LOKI",
        common_name_c == "Northern Waterthrush" ~ "NOWA",
        common_name_c == "Osprey" ~ "OSPR",
        common_name_c == "Prothonotary Warbler" ~ "PROW",
        common_name_c == "Ring-billed Gull" ~ "RBGU",
        common_name_c == "Sharp-shinned Hawk" ~ "SSHA",
        common_name_c == "Tree Swallow" ~ "TRES",
        common_name_c == "hawk sp." ~ "UNHA",
        common_name_c == "Warbling Vireo" ~ "WAVI",
        common_name_c == "Willow Flycatcher" ~ "WIFL", 
        common_name_c == "Yellow Warbler" ~ "YWAR",
        TRUE ~ spec_code
      )
    )
  
  # Combine ebird and site names and clean up
  bird_names <- 
    full_join(
      ebird_names, 
      cleaned_mbbs_names %>% select(common_name_c, spec_code = spec_code_c), 
      by = c("common_name" = "common_name_c")) %>%
    mutate(
      # add missing sci names
      sci_name = case_when(
        common_name == "Northern Waterthrush" ~ "Parkesia noveboracensis",
        common_name == "Willow Flycatcher" ~ "Empidonax traillii",
        TRUE ~ sci_name
      ),
      # Based on http://www.birds.cornell.edu/clementschecklist/download/ 
      tax_order =  case_when(
        common_name == "Northern Waterthrush" ~ 31631L,
        common_name == "Willow Flycatcher" ~ 15668L,
        TRUE ~ tax_order
      ),
      # add spec_codes not in ebird
      spec_code = case_when(
        common_name == "Mallard (Domestic type)" ~ "MALL", 
        common_name == "Bald Eagle" ~ "BAEA",
        common_name == "Horned Lark" ~ "HOLA",
        common_name == "Common Grackle (Purple)" ~ "COGR",
        common_name == "Blackpoll Warbler" ~ "BLPW",
        common_name == "Chuck-will's-widow " ~ "CWWI",
        common_name == "Great Egret" ~ "GREG",
        common_name == "Warbling Vireo" ~ "WAVI",
        common_name == "Osprey" ~ "OSPR",
        common_name == "Black Vulture" ~ "BLVU",
        common_name == "hawk sp." ~ "UNHA",
        common_name == "Kentucky Warbler" ~ "KEWA",
        common_name == "Yellow Warbler" ~ "YEWA",
        common_name == "Loggerhead Shrike" ~ "LOKI",
        TRUE ~ spec_code
      )
    )

  # Update mbbs_site_dt names
  mbbs_names_link <- 
    cleaned_mbbs_names %>% 
    select(-missing_spec) %>%
    left_join(
      bird_names %>% select(-spec_code),
      by = c("common_name_c" = "common_name")
    ) %>%
    select(-spec_code) %>%
    right_join(
      all_mbbs_names, 
      by = "common_name_c"
    )
  
  cleaned_mbbs_site_dt <- 
    mbbs_site_dt %>%
    left_join(mbbs_names_link, by = c("common_name", "spec_code")) %>%
    select(-common_name, -spec_code) %>%
    select(everything(), common_name = common_name_c, spec_code = spec_code_c)

  # Update ebird names
  ebird_dt <- 
    ebird_dt %>%
    left_join(bird_names, by = c("common_name", "sci_name", "tax_order")) %>%
    mutate(source = "ebird")
  
  ## 3. Align route information ####
  routes <- 
    ebird_dt %>%
    # In 2020 counts were started to be recorded per stop; whereas this dataset
    # is to align routes recorded in the aggregated route format.
    filter(year < 2020) %>%
    distinct(route_num, lat, lon) %>%
    # TODO: in the case there are multiple lat/lon for a route we have to pick one
    #       maybe better to have single source of truth dataset that contains 
    #       all route/stop information?
    group_by(route_num) %>%
    filter(row_number() == 1)
  
  cleaned_mbbs_site_dt <- 
    cleaned_mbbs_site_dt %>%
    mutate(route_num = as.integer(route_num)) %>%
    select(-route) %>%
    distinct() %>%
    left_join(routes, by = "route_num") %>%
    mutate(
      source = "mbbs_site",
      mbbs_county = mbbs_county
    )
  
  list(
    site = cleaned_mbbs_site_dt,
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