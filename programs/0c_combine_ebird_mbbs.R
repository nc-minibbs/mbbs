#------------------------------------------------------------------------------#
#  TITLE: Combine MBBS ebird and website data 
#   DATE: 20181216
#   PROG: B. Saul
#   DESC: Combine MBBS ebird and website data. Removing duplicates and cleaning
#         as necessary
#------------------------------------------------------------------------------#

head(ebird_dt)

head(old_mbbs_dt)

eb <- ebird_dt %>%
  distinct(year, route_num) %>%
  arrange(year, route_num) %>%
  mutate(ebird = TRUE)

mb <- old_mbbs_dt %>% ungroup() %>%
  distinct(year, route_num) %>%
  arrange(year, route_num) %>%
  mutate(mbbs_site = TRUE)

full_join(eb, mb, by = c("year", "route_num")) %>%
  mutate(
    ebird = if_else(is.na(ebird), FALSE, ebird),
    mbbs_site = if_else(is.na(mbbs_site), FALSE, mbbs_site)
  ) 

test <- inner_join(ebird_dt, old_mbbs_dt, by = c("year", "route_num", "common_name"))
test %>%
  select(common_name, route.x, year, count.x, count.y) %>%
  filter(count.x != count.y) %>%
  select(year, route.x, common_name, count.x, count.y) %>%
  arrange(year, route.x)

ebird_dt %>%  filter(route_num == 5, year == 2009) %>%
  select(sub_id, year, route, lat, long, common_name, count) %>%
  arrange(year,common_name)

test %>%
  filter(route_num == 5, year == 2009) %>%
  select(year, route.x, common_name, count.x, count.y) 
## QC checks #### 

## There should be no more than 12 unique routes in a year

ebird_dt %>%
  distinct(sub_id, year, route_num) %>%
  group_by(year, route_num) %>%
  tally() %>% 
  filter(n > 1)

## There should be no more than 1 record for a species in a route for year