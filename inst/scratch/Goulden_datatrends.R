#Generate plots of all species, break them out by county

#load libraries
library(dplyr)
library(ggplot2)
library(stringr)
library(ggpubr)
library(beepr)

#pull in the datasets
load("data/mbbs_orange.rda")
load("data/mbbs_durham.rda")
load("data/mbbs_chatham.rda")

#fix common names so there's no like Mallad + Mallad (Domestic type), and those are all just Mallard instead
mbbs_orange$common_name <- str_split(mbbs_orange$common_name, fixed(" ("), simplify = TRUE)[,1]
mbbs_durham$common_name <- str_split(mbbs_durham$common_name, fixed(" ("), simplify = TRUE)[,1]
mbbs_chatham$common_name <- str_split(mbbs_chatham$common_name, fixed(" ("), simplify = TRUE)[,1]


#fix all scientific names so there's no like, cardinalis [cardinalis group] and those are all just cardinalis instead. !!Issue still to fix: removing the last word when it's like dominica dominica/stoddardi 
#issue actually already fixed in import_ebird_data - you need to rerun making the data for the year. But first fix the common name issue there as well. 
mbbs_orange$sci_name <- str_split(mbbs_orange$sci_name, fixed(" ("), simplify = TRUE)[,1]  
mbbs_orange$sci_name <- str_split(mbbs_orange$sci_name, fixed(" ["), simplify = TRUE)[,1]  

mbbs_durham$sci_name <- str_split(mbbs_durham$sci_name, fixed(" ("), simplify = TRUE)[,1]  
mbbs_durham$sci_name <- str_split(mbbs_durham$sci_name, fixed(" ["), simplify = TRUE)[,1] 

mbbs_chatham$sci_name <- str_split(mbbs_chatham$sci_name, fixed(" ("), simplify = TRUE)[,1]  
mbbs_chatham$sci_name <- str_split(mbbs_chatham$sci_name, fixed(" ["), simplify = TRUE)[,1] 


#create mbbs_all
mbbs_all <- bind_rows(mbbs_orange, mbbs_chatham, mbbs_durham)
#problem! 16 observations in Wake county instead of Durham, didn't cause problems before but let's lump those back into durham
mbbs_all <- mbbs_all %>%
  mutate(county = str_replace(county, "Wake", "Durham"))  %>%
  mutate(
    # The route number is not unique within the study
    # (only within a county).
    # Here's a silly way to create distinct ID for routes
    # across county
    county_factor = case_when(
      mbbs_county == "orange" ~ 1,
      mbbs_county == "durham" ~ 20,
      mbbs_county == "chatham" ~ 40,
    ),
    route_num = route_num + county_factor #keep as route_num to be consistent with format and colun names in the individual county plots
  )


#now, let's get a list of all species seen in any of the counties
species_list_allcounties <- unique(mbbs_all$common_name)
#great, we've trimmed down from 144 to 129, and removed the extraneous info, which should now lump Mallard and Mallard (Domestic type) together in our analysis. Scientific names should also all be the same, previously filtered for. We'll add this filter back to the main ebird pull filtering code later
#let's make blank variables so we can know if something's errored out at some point
p1 <- plot(1:10)
p2 <- plot(1:10)
p3 <- plot(1:10)
p4 <- plot(1:10)

#generate the pdf that we'll print to
pdf(paste("inst/scratch/speciesplots_allcounties",".pdf", sep = ""), width = 12)

#run the loop to create the plots for all species with more than 10 observations
for(b in 1:length(species_list_allcounties)) {
  
  species_in_use <- species_list_allcounties[b]
  
  #check if species has more than 10 observations, if not, exclude it
  if (nrow(mbbs_all %>% filter(common_name == species_in_use)) > 10) {
    print(plot_one_species_county_breakdown(species_in_use))
  }
  
}
beep()

dev.off()

#####################
#
# FUNCTIONS
#
###############################

#####Function to plot one species in three counties + across all counties
plot_one_species_county_breakdown <- function (species) {
  
  p1 <- plot_one_species_one_county(mbbs_orange, species, "Orange")
  p2 <- plot_one_species_one_county(mbbs_durham, species, "Durham")
  p3 <- plot_one_species_one_county(mbbs_chatham, species, "Chatham")
  p4 <- plot_one_species_one_county(mbbs_all, species, "All Counties")
  
  ggarrange(p1,p2,p3,p4, nrow = 1)

}

#####Function to plot just one species from one county 
plot_one_species_one_county <- function(mbbs_county, species, county_name) {
  
  
    #get the mean number seen across all routes, this code accounts for that there are multiple stops per route 2020 onwards with the [summarize(sum = sum(count)) %>% group_by(year) %>% summarize(mean = mean(sum))] rather than just using [%>% summarize(mean = mean(count)] 

#We do need to also add a check for the number of routes run each year, bc if a route wasn't run that needs to be incorporated 
    ave <- mbbs_county %>% filter(common_name == species) %>% group_by(year, route_num) %>% summarize(sum = sum(count)) %>% group_by(year) %>% summarize(mean = mean(sum)) #average is mean by year
    
    route_totals <- mbbs_county %>% filter(common_name == species) %>% group_by(year, route_num) %>% summarize(sum = sum(count)) #count in each route each year
    
    #plot
    #plot each route, plot average across route (line + points), add trend line (lm), add species name and county to top of graph
    #mbbs_county %>% filter(common_name == species) %>%  ggplot(aes(x = year, y = count)) +
           # xlim(1999,2022) +
           # geom_line(aes(group = route_num), color = "grey") +
          #  geom_line(data = ave, aes(x = year, y = mean)) +
           # geom_point(data = ave, aes(x = year, y = mean)) +
          #  stat_smooth(method = "lm", se = FALSE) + 
           # labs(title=paste(species), subtitle = county_name) +
            #theme_bw()
    
    route_totals %>% ggplot(aes(x=year, y=sum)) +
      xlim(1999,2022) +
      geom_line(aes(group = route_num), color = "grey") +
      geom_line(data = ave, aes(x = year, y = mean)) +
      geom_point(data = ave, aes(x = year, y = mean)) +
      stat_smooth(method = "lm", se = FALSE) + 
      labs(title=paste(species), subtitle = county_name) +
      theme_bw()

}

plot_all_species_in_county <- function(mbbs_county) {
  
  #get county name
  county <- mbbs_county$county[1]
  #generate list of all species to loop through
  species_list <- unique(mbbs_county$common_name)
  #create variable to hold the in use species
  in_use_species <- "EMPTY"
  
  #generate the pdf that we'll print to
  pdf(paste("inst/scratch/speciesplots",county,".pdf", sep = ""))
  
  #create for loop to go through all the species
  for(a in 1:length(species_list)) {
    
    #get in use species
    in_use_species <- species_list[a]
    
    #get the mean number seen across all routes
    ave <- mbbs_county %>% filter(common_name == in_use_species) %>% group_by(year) %>% summarize(mean = mean(count))
    
    #plot
    #plot each route, plot average across route (line + points), add trend line (lm), add species name and county to top of graph
    print(mbbs_county %>% filter(common_name == in_use_species) %>%  ggplot(aes(x = year, y = count)) +
            xlim(1999,2022) +
            geom_line(aes(group = route_num), color = "grey") +
            geom_line(data = ave, aes(x = year, y = mean)) +
            geom_point(data = ave, aes(x = year, y = mean)) +
            stat_smooth(method = "lm", se = FALSE) + 
            labs(title=paste(in_use_species), subtitle = county) +
            theme_bw())
    
  }
  
  dev.off()
  
}



#alrighty, now let's work on creating a dataset from which we can incorporate 0's [where a route was run but the species was not counted on the route at all) into our averages. 
#we'll need mbbs_all and lets use species_list_allcountries since we've already got that
#mbbs_all
#species_list_allcounties
allroutes <- unique(mbbs_all$route_num) #remember: routes have been modified 
#this is not sorted chronologically, do not worry
allyears <- unique(mbbs_all$year)

#y number of years
y <- length(allyears)
#r number of routes
r <- length(allroutes)
#s number of species
s <- length(species_list_allcounties)

#now let's create a dataframe of unique combinations of route and year
routeyear <- unique(mbbs_all[,c('year', 'route_num')]) %>% arrange(year)

#create dataframe with species repeated in every year and route that's been run.
routeyearspecies <- data.frame(route = rep(routeyear$route_num, s), year = rep(routeyear$year, s), species = rep(species_list_allcounties, each = nrow(routeyear)))

#Now, we'll join together the routeyearspecies dataset with the mbbs_all in order to create those 0's where the route was run, but the species was not seen

left_join(routeyearspecies, mbbs_alll, by)

