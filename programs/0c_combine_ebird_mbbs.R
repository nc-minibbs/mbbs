#------------------------------------------------------------------------------#
#  TITLE: Combine MBBS ebird and website data 
#   DATE: 20181216
#   PROG: B. Saul
#   DESC: Combine MBBS ebird and website data. Removing duplicates and cleaning
#         as necessary
#------------------------------------------------------------------------------#

## 1. HARD CODE Clean up ####

mbbs_site_dt <- mbbs_site_dt %>%
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

ebird_names <- ebird_dt %>%
  distinct(common_name, sci_name, tax_order)

all_mbbs_names <- mbbs_site_dt %>%
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
    common_name_c = str_replace(common_name_c, "^Yellow-thr ", "Yellow-throated "),
    common_name_c = str_replace(common_name_c, "^White-br ", "White-breasted "),
    common_name_c = str_replace(common_name_c, "-Poor-Will", "-poor-will"),
    common_name_c = str_replace(common_name_c, "Will's-Widow", "will's-widow")
  ) %>%
  # Adjust common names to match ebird common name
  mutate(
    common_name_c = case_when(
      common_name_c == "Accipiter species" ~ "hawk sp.",
      common_name_c == "Rock Pigeon"      ~ "Rock Pigeon (Feral Pigeon)",
      common_name_c == "Rock Dove"        ~ "Rock Pigeon (Feral Pigeon)",
      common_name_c == "Whip-poor-will"   ~ "Eastern Whip-poor-will",
      TRUE  ~ common_name_c
    )
  ) %>%
  arrange(common_name_c)

# Remove any duplicated common names that have entries both with and without codes
cleaned_mbbs_names <- all_mbbs_names %>%
  distinct(common_name_c, spec_code) %>%
  mutate(missing_spec = is.na(spec_code) * 1) %>%
  group_by(common_name_c) %>%
  arrange(common_name_c, missing_spec) %>%
  filter(row_number() == 1) %>%
  ## Add missing species codes
  mutate(
    spec_code_c = case_when(
      # Getting codes from http://www.wec.ufl.edu/birds/SurveyDocs/species_list.pdf
      common_name_c == "American Woodcock" ~ "AMWO",
      common_name_c == "Black-and-white Warbler" ~ "BAWW",
      common_name_c == "Blue-headed Vireo" ~ "BHVI",
      common_name_c == "Broad-winged Hawk" ~ "BWHA",
      common_name_c == "Common Nighthawk"  ~ "CONI",
      common_name_c == "Double-crested Cormorant" ~ "DCCO",
      common_name_c == "Eastern Screech-Owl" ~ "EASO",
      common_name_c == "Glossy Ibis" ~ "GLIB",
      common_name_c == "Northern Waterthrush" ~ "NOWA",
      common_name_c == "Osprey" ~ "OSPR",
      common_name_c == "Prothonotary Warbler" ~ "PROW",
      common_name_c == "Tree Swallow" ~ "TRES",
      common_name_c == "Yellow Warbler" ~ "YWAR",
      TRUE ~ spec_code
    )
  )

# Combine ebird and site names and clean up
bird_names <- full_join(ebird_names, 
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
      TRUE ~ spec_code
    )
  )

# Update mbbs_site names

mbbs_names_link <- cleaned_mbbs_names %>% 
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

cleaned_mbbs_site_dt <- mbbs_site_dt %>%
  left_join(mbbs_names_link, by = c("common_name", "spec_code")) %>%
  select(-common_name, -spec_code) %>%
  select(everything(), common_name = common_name_c, spec_code = spec_code_c)

# Update ebird names
ebird_dt <- ebird_dt %>%
  left_join(bird_names, by = c("common_name", "sci_name", "tax_order"))


## 3. Update route information ####

routes <- ebird_dt %>%
  distinct(route, route_num, lat, lon)

cleaned_mbbs_site_dt <- cleaned_mbbs_site_dt %>%
  select(-route) %>%
  left_join(routes, by = "route_num")

## 4. Subset source data by year ####
# TODO: deal with habitat
ebird_dt_gt2008 <- ebird_dt %>%
  filter(year > 2008) %>%
  select(-county, -state, -loc) %>%
  mutate(source = "ebird") %>%
  # Per Haven (2018-12-28): use most recent ebird entry if a route/year has multiple entries
  group_by(year, route_num) %>%
  filter(sub_id == max(sub_id)) %>%
  ungroup()


mbbs_site_dt_lt2009 <- cleaned_mbbs_site_dt %>%
  filter(year < 2009) %>%
  mutate(source = "mbbs_site")

## 5. Combine sources ####

mbbs_dt <- bind_rows(mbbs_site_dt_lt2009, ebird_dt_gt2008)

## 6. QC checks #### 

## There should be no more than 12 unique routes in a year

mbbs_dt %>%
  distinct(sub_id, year, route_num) %>%
  group_by(year, route_num) %>%
  tally() %>% 
  filter(n > 1)

## There should be no more than 1 record for a species in a route for year

mbbs_dt %>%
  group_by(year, route_num, common_name) %>%
  tally() %>% 
  filter(n > 1)


## 7. Save results ####

saveRDS(mbbs_dt, file = "data/mbbs_dt.rds")

rm(list = ls())
