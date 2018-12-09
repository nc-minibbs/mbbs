
library(ggplot2)
library(tidyr)
yearly <- analysis_dt %>%
  group_by(year, common_name, sci_name) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  complete(
   nesting(common_name, sci_name),
   year = full_seq(year, 1), 
   fill = list(count = 0)
  )

ggplot(
  data = yearly %>% filter(common_name == "Eastern Whip-poor-will"),
  aes(x = year, y = count, group = sci_name)
) + geom_line() +
  stat_smooth()
