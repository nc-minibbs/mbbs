### SCRATCH!!!!! ####

mbbs_dt <- readRDS(file = "data/mbbs_dt.rds")

library(ggplot2)
library(tidyr)


mbbs_dt %>%
  distinct(year, route_num) %>%
  mutate(dummy = 1) %>%
  tidyr::spread(key = "year", value = "dummy", fill = 0)

analysis_dt <- mbbs_dt %>%
  group_by(year, common_name, sci_name) %>%
  summarise(count = mean(count)) %>%
  ungroup() %>%
  complete(
   nesting(common_name, sci_name),
   year = full_seq(year, 1), 
   fill = list(count = 0)
  )


plot_dt <- function(dt){
  ggplot(
    data = dt,
    aes(x = year, y = count, group = sci_name)
  ) + geom_line() +
    geom_point(size = 1) + 
    scale_y_continuous(
      "Mean count per route",
      expand = c(0, 0),
      limits = c(0, max(dt$count) + 1)
    ) + 
    scale_x_continuous(
      "Year",
      expand = c(.01, 0),
      breaks = seq(2000, 2018, by = 2) 
    ) + 
    stat_smooth(method = "lm", se = FALSE) +
    theme_classic() + 
    theme()
}

analysis_dt %>% filter(common_name == "Acadian Flycatcher") %>%
  plot_dt()

```{r}
show_plots <- function(.prelabel){
  plots <- paste0(list.files("img", full.names = TRUE))
  plots <- plots[grepl(.prelabel, plots)]
  
  replacements <- c("", "")
  names(replacements) <- c("\\.png", sprintf("img/%s_", .prelabel)) 
  names(plots) <- str_replace_all(plots, replacements)
  
  bsselect(plots, type = "img", selected = "As_ppm_m75", 
           frame_height = "200",
           live_search = TRUE, show_tick = TRUE)
}
```

show_plots("epoxy_nacre_free_scale")