#------------------------------------------------------------------------------#
#  Prepare analysis data from MBBS site
#------------------------------------------------------------------------------#

library(mbbs)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sparkline)
library(DT)
library(ggiraph)

pre_dt <- mbbs %>% 
  filter(!is.na(spec_code)) 

analysis_species <- pre_dt %>%
  group_by(common_name, year) %>%
  summarise(dummy = sum(count) > 1) %>%
  group_by(common_name) %>%
  summarise(nyears = sum(dummy)) %>%
  # include species only observed in at least 3 years
  filter(nyears > 2)

analysis_dt <- pre_dt %>%
  right_join(analysis_species, by = "common_name") 

analysis_dt_grouped <- analysis_dt %>%
  group_by(year, common_name) %>%
  summarise(count = mean(count)) %>%
  ungroup() %>%
  complete(
    nesting(common_name),
    year = full_seq(year, 1), 
    fill = list(count = 0)
  ) 

model_dt <- analysis_dt %>%
  mutate(time = year - min(year)) %>%
  group_by(common_name, sci_name, spec_code) %>%
  tidyr::nest() %>%
  mutate(
    gee_model = purrr::map(data, ~ gee_model(.x)),
    log_rate  = purrr::map_dbl(
      .x = gee_model,
      .f = ~ .x$estimate[.x$term == "time"]
    ),
    log_rate_se  = purrr::map_dbl(
      .x = gee_model,
      .f = ~ .x$std.error[.x$term == "time"]
    ),
    rate         = exp(log_rate) - 1,
    rate_lo      = exp(log_rate - qt(.975, df = 11) * log_rate_se) - 1,
    rate_hi      = exp(log_rate + qt(.975, df = 11) * log_rate_se) - 1,
    significant  = !(rate_lo < 0 & rate_hi > 0)
  )
