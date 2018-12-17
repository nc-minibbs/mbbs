#------------------------------------------------------------------------------#
#  TITLE: Import MBBS ebird data 
#   DATE: 20181216
#   PROG: B. Saul
#   DESC: Import MBBS data downloaded from the ebird site in MyEBirdData.csv
#------------------------------------------------------------------------------#
library(dplyr)
library(stringr)
library(lubridate)

## Read data ####
dt <- read.csv("data/MyEBirdData.csv", stringsAsFactors = FALSE)
dt$obs_id <- 1:nrow(dt)

## Clean up comments ####

comments <- dt %>% distinct(Submission.ID, Checklist.Comments)

missing_strings <- c("UNAVAIL", "none", "Unavail", "UNAVIL", "XXXX", 
                     "NOT RECORDED", ":", "\\?")

comment_dt <- str_replace_all(comments$Checklist.Comments, "&#61;", "==") %>%
  str_replace_all(c("Observers"    = "observers",
                    "observer\\s+" = "observers",
                    "note\\s+"      = "notes",
                    "WEATHER AND OBSERVERS NOT AVAILABLE" = "",
                    "VEHICLES PASSING = "           = "vehicles==",
                    "TOTAL vehicles\\s+"            = "vehicles",
                    "; Unavail."                    = "Unavail",
                    "habitat@2 L"                   = "habitat@2L",
                    ", R==B"                        = "habitat@2R == B", # submission id S46103457
                    "habitat@ 3L"                   = "habitat@3L",
                    "habitat@10RO"                  = "habitat@10R== O",
                    "habitat@10RB"                  = "habitat@10R== B",
                    "habitat@17LO"                  = "habitat@17L== O",
                    "habitat@13LM/B"                = "habitat@13L== M/B",
                    "habitat@10RB"                  = "habitat@10R== B",
                    " 8R==B"                        = "habitat@8R==B",
                    " 9L==S"                        = "habitat@9L==S",
                    "habitat@7LB,RB"                = "habitat@7L== B ; habitat@7L== B",
                    "habitat@RO/M"                  = "", # Don't know what this one means
                    " = 39"                         = "39",
                    "habitat 7:"                    = "habitat == 7")) %>%
  str_replace_all(c("OBSERVERS;"                    = "observers==", 
                    "OBSERVERS ;"                   = "observers==", 
                    "OBSERVERS"                     = "observers==",
                    "observer:"                     = "observers==",
                    "OBSERVERS; Unavail"            = "observers==UNAVAIL",
                    "WEATHER"                       = "weather==",
                    "vehicles:"                     = "vehicles==",
                    "TOTAL VEHICLES PASSING"        = "vehicles==",
                    "NOTES on unusual circumstances" = "notes==",
                    "NOTES"                         = "notes==",
                    "NOTES :"                       = "notes==",
                    "notes none"                    = "notes==none",
                    "70's cloudy"                   = "weather==70's cloudy",
                    "no changes in habitat"         = "notes==no changes in habitat",
                    "then partly cloudy;"           = "then partly cloudy,",
                    "vehicles==4,habitat@11L"       =  "vehicles==4; habitat@11L"
                    )) %>%
  # Any semicolons after notes replace with comma
  purrr::map(.f = function(x){
    any_notes <- str_locate(x, "notes==")
    if(!is.na(any_notes) && any_notes[2] >= any_notes[1]){
      temp <- str_split(x, "\\s")[[1]]

      clean_up_which <- (which(str_detect(temp, "notes=="))):length(temp)
      temp[clean_up_which] <- str_replace(temp[clean_up_which], ";", ",") 
      x <- paste0(temp, collapse = " ")
    }
    x
  }) %>% 
  stringr::str_split(";") %>%
  purrr::map(trimws) %>%
  # a Bit more clean up
  purrr::map(.f = ~ str_replace(.x, "TOTAL vehicles", "vehicles")) %>%
  purrr::map(.f = ~ str_replace(.x, "observer==", "observers==")) %>%
  purrr::map(.f = ~ str_replace(.x, "note=+", "notes==")) %>%
  purrr::map(.f = ~ str_replace(.x, "habitat =+", "habitat==")) %>%
  # Get rid of `Mini Breeding Bird Survey` text
  purrr::map(.f = ~.x[!str_detect(.x, "^Mini Breeding Bird Survey, Orange Co")]) %>%
  # Convert to data.frame
  purrr::map_dfr(.f = function(x) {
    hold <- str_split(x, "=+")
    hold <- purrr::map_dfc(hold, function(z){
      temp        <- data_frame(trimws(z[2]))
      names(temp) <- z[1]
      temp
    })
  }) %>%
  select(-V1, -UNAVAIL.) %>%
  mutate(
    Submission.ID = comments$Submission.ID
  ) 

clean_missing <- function(x){
  x <- str_replace_all(x, paste0(missing_strings, collapse = "|"), NA_character_)
  if_else(x == "", NA_character_, x)
}

not_habitat_dt <- comment_dt %>%
  select(-contains("habitat")) %>%
  rowwise() %>%
  mutate_all(
    .funs = funs(clean_missing)
  ) %>%
  mutate(
    vehicles = as.numeric(str_replace_all(vehicles, "[A-Za-z]", ""))
  ) %>%
  # Not sure what this variable is for
  select(-hawksp)


# TODO : clean up habitat data
# habitat_dt <- comment_dt %>%
#   select(Submission.ID, contains("habitat")) %>%
#   tidyr::gather(
#     key = "var", value = "val", -Submission.ID, na.rm= TRUE
#   )
# 
# habitat_dt %>%
#   mutate(
#     var = case_when(
#     var == "habitat" ~ "",
#     TRUE             ~ str_replace(var, "habitat@", "")
#   )) %>% View()

## Put things back together ####

ebird_dt <- dt %>%
  select(
    obs_id,
    sub_id      = Submission.ID,
    common_name = Common.Name,
    sci_name    = Scientific.Name,
    tax_order   = Taxonomic.Order,
    count       = Count,
    state       = State.Province,
    county      = County,
    loc         = Location,
    lat         = Latitude,
    long        = Longitude,
    date        = Date,
    time        = Time,
    distance    = Distance.Traveled..km.,
    all_obs     = All.Obs.Reported,
    breed_code  = Breeding.Code,
    species_comment = Species.Comments
  ) %>%
  left_join(
    not_habitat_dt, by = c("sub_id" = "Submission.ID")
  ) %>%
  mutate(
    count = as.numeric(count),
    date  = as.Date(date, format = "%m-%d-%Y"),
    year  = year(date),
    route = str_replace(loc, "MBBS, NC, Orange Co, ", ""),
    route_num = str_extract(route, "[1-9][0-9]*")
  ) 
  


