# Script to download and clean MBBS data directly from eBird

orange_rawdata = read.csv('inst/extdata/MyEBirdData_Orange_20210316.csv', header = T) 

# Change a few instances where Count == 'X' to 1 (MALL, NOBO, E. Whip-poor-will, EAKI, PRWA, KEWA) all from Budnitz/Schultz 2010-2012
orange_rawdata$Count[orange_rawdata$Count == 'X'] = 1
orange_rawdata$Count = as.numeric(orange_rawdata$Count)

orange = orange_rawdata %>%
  mutate(County = "Orange",
         RouteString = str_extract(Location,"Route [0-9]+"),
         Route = as.numeric(substr(RouteString, nchar(RouteString)-1, nchar(RouteString))),
         Year = as.numeric(substr(Date, 1, 4)))


#####################################################################################

if (0) { # Checking and investigating mwhere there are multiple checklists in the same year

duplicate_checklists_by_route_year = orange %>% 
  group_by(Route, Year) %>% 
  summarize(n_checklists = n_distinct(Submission.ID)) %>% 
  filter(!n_checklists %in% c(1, 20))

# Function to check whether species abundances are the same between two checklists

compare_checklists = function(checklist1, checklist2, df) {
  c1 = filter(df, Submission.ID == checklist1) %>%
    select(Common.Name, Count) %>%
    filter(Count > 0) %>%
    arrange(Common.Name) %>%
    rename(Count1 = Count)
    
  c2 = filter(df, Submission.ID == checklist2) %>%
    select(Common.Name, Count) %>%
    filter(Count > 0) %>%
    arrange(Common.Name) %>%
    rename(Count2 = Count)
    
  
  joined = full_join(c1, c2) %>%
    mutate(checklist1 = checklist1, checklist2 = checklist2)
  
  return(joined)
}

duplicate_checklist_comparison = c()
for (i in 1:nrow(duplicate_checklists_by_route_year)) {
  
  checklists = orange %>%
    filter(Route == duplicate_checklists_by_route_year$Route[i], Year == duplicate_checklists_by_route_year$Year[i]) %>%
    distinct(Submission.ID) 
  
  tmp = compare_checklists(checklists[1,1], checklists[2,1], orange) %>% 
    mutate(Route = duplicate_checklists_by_route_year$Route[i], 
           Year = duplicate_checklists_by_route_year$Year[i])

  duplicate_checklist_comparison = rbind(duplicate_checklist_comparison, tmp)
  
}
duplicate_checklist_comparison$sameN = duplicate_checklist_comparison$Count1 == duplicate_checklist_comparison$Count2


# Conduct pairwise comparisons of all checklists and quantify the difference
# in summed absolute differences in abundance by species. Values close to 0 
# imply duplicates or close duplicates.
orange_wide = pivot_wider(orange, id_cols = Common.Name, names_from = Submission.ID, values_from = Count)

orange_wide[is.na(orange_wide)] = 0

sum_abs_diff = data.frame(checklist1 = rep(NA, 43956), checklist2 = rep(NA, 43956), diff = rep(NA, 43956))

n = 1

for (i in 2:(ncol(orange_wide)-1)) {
  
  for (j in (i+1):(ncol(orange_wide))) {
    
    sum_abs_diff$checklist1[n] = names(orange_wide)[i]
    sum_abs_diff$checklist2[n] = names(orange_wide)[j]
    
    sum_abs_diff$diff[n] = sum(abs(orange_wide[i] - orange_wide[j]))
    
    n = n + 1
    
  }
  
}

sum_abs_diff = sum_abs_diff %>% arrange(diff)

# Examining duplicate_checklist_comparison reveals the following duplicate checklist ID's that can be removed:
# 
# Route 5: S6659296, S23439664
# Route 10: 2005, S6659658
#
# as well as the following paired checklists which are problematic and should be independently investigated:
#
# Route 4: 2005, S6659351/S6659606 (of these two, I get the impression of one list by an experienced birder,
#          and one list by a less experienced birder; I would trust S6659351 and drop S6659606); 
# Route 5: 2005, S23455267/S6659305/S6659365 (only difference is BLGR; keep S23455267 (5 BLGR, Will Cook's checklist)
#                & drop S6659305, S6659365 (7 BLGR); none of them have comments
#          2007, S23439679/S6653243 (keep S6653243, drop S23439679); 
#          2008, S23439662/S6659533 (keep S23439662 which has unexpected Glossy Ibis rec, reported to The Chat, drop S6659533); 
#          2009, S23439661/S6659581 (keep S23439661 which has everything the same as the other checklist, plus a few more species, drop S6659581);
# Route 6: 2008, S6653290/S8323435 (notes from S8323435: "note=woodcocks flew over our heads at our second stop at about 5:45am",
#          one checklist includes the woodcock, the other does not; I assume the woodcock did not fly over during the 3-minute count,
#          but this could be confirmed with Robin Moran / Judy Murray; in that case drop S8323435;
#          other difference is # vehicles 51 vs 102--weird); 
# Route 12: 2013, S14314705/S14314588; 2014, S18727169/S18727091; 2015, S24083367/S24083590;
#           2016, S46295712	S46295165; 2018, S46295712/S46295165
#           (all of these for Route 12 likely represent the 3 extra pre-dawn stops for owling which 
#           are filtered out by grepl("night birds preceding", Checklist.Comments))

} # end if(0)

###################################################################################

sub_ids_to_drop = c('S6659296', 'S23439664', 'S6659658', 'S6659606', 'S6659305', 'S6659365', 'S23439679', 'S6659533', 'S6659581', 'S8323435')

orange_dups_removed = orange %>%
  # Filter out nighttime pre-count surveys for the 15 minutes prior to the beginning of the official survey start time;
  # these were most regularly done by Haven Wiley who used the phrase "night birds preceding a regular MBBS survey",
  # although in 2020 he used the term "pre-counts". Note that other checklists refer to "pre-count" birds, but just
  # list those birds observed in the Checklist.Comments field. Thus the checklists with "pre-count" (singular) should not be filtered.
  filter(!(grepl("night birds preceding", Checklist.Comments))) %>%
  filter(!Submission.ID %in% sub_ids_to_drop) %>% 
  group_by(County, Route, Date, Year, Common.Name, Scientific.Name, Taxonomic.Order) %>%
  summarize(Ntot = sum(Count, na.rm = T)) %>%
  filter(Ntot > 0) %>%
  ungroup()

# There should be no more than 12 unique routes in a year
# NOTE: data from 1999-2003 have not been fully entered into eBird.
# Need to use data from the MBBS website for early years for now.
orange_dups_removed %>%
  distinct(Year, Route) %>%
  group_by(Year) %>%
  tally() 

## There should be no more than 1 record for a species in a route for year

orange_dups_removed %>%
  group_by(Year, Route, Common.Name) %>%
  tally() %>% 
  filter(n > 1)

# Simplify columns to merge, filter for data from 2019+
orange1920 = orange_dups_removed %>%
  filter(Year >= 2019) %>%
  rename(date = Date, 
         year = Year,
         route_num = Route,
         common_name = Common.Name,
         tax_order = Taxonomic.Order,
         count = Ntot) %>%
  select(date, year, route_num, common_name, tax_order, count)

# Read in the .csv file with combined MBBS website-scraped data plus
# eBird 2009-2018 data. Then bind recent eBird data.
mbbs = read.csv('inst/extdata/mbbs_data_20190127.csv', header = T) %>%
  select(date, year, route_num, common_name, tax_order, count) %>%
  bind_rows(orange1920)



mean_per_year = mbbs %>%
  group_by(year, common_name) %>%
  summarize(meanCount = mean(count, na.rm = T))

plotSpecies = function(speciesname, regLine = TRUE, ...) {
  tmp = filter(mean_per_year, common_name == speciesname)
  
  plot(tmp$year, tmp$meanCount, type = 'l', 
       xlab = 'Year', ylab = 'Count', 
       las = 1, main = speciesname, ...)
  
  if (regLine) {
    lm1 = lm(meanCount ~ year, data = tmp)
    abline(lm1)
  }
}
