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

mbbs_results <- analysis_dt_grouped %>%
  # filter(
  #   common_name %in% c("Northern Cardinal", "Tufted Titmouse", "Wood Thrush")
  # ) %>%
  group_by(common_name) %>%
  tidyr::nest(.key = "data_grouped") %>% 
  left_join(
    model_dt, by = c("common_name")
  ) %>% 
  left_join(
    select(mbbs_species, -sci_name, -spec_code), by = "common_name"
  ) %>%
  mutate(
    # lm             = purrr::map(data_grouped, ~ lm(count ~ year, data = .x)),
    # rate_of_change = purrr::map_dbl(lm, ~ coef(.x)[2]),
    # rate_of_change = round(rate_of_change, 3), 
    rate_color     = case_when(
      rate < 0 & significant  ~ "#d7191c",
      rate < 0 & !significant ~ "#fdae61",
      rate > 0 & !significant ~ "#abd9e9",
      rate > 0 & significant  ~ "#2c7bb6"
    ),
    rate  = purrr::map2_chr(
      .x = rate,
      .y = rate_color, 
      .f = ~ as.character(htmltools::span(
        class = "text-center",
        style = sprintf("color:%s", .y),
        round(.x, 3)))),
    sparkline      = purrr::map_chr(data_grouped, ~ plot_sparkline(.x$count)),
    trend_plot     = purrr::map2(
      .x = data,
      .y = data_grouped,
      .f = ~ plot_trend(.x, .y) %>%
        convert_img(width = 6.5, height = 5) %>%
        htmltools::img(src = .)),
    details        = purrr::pmap(
      .l = list(x = common_name, y = sci_name, z = trend_plot),
      .f = function(x, y, z) {as.character(create_details(x, y, z)) }
    )
  ) 

