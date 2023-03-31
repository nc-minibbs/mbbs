library(dplyr)
library(stringr)

#yeah, if ur fixing the comment processer to pull observer correctly, just run the whole database upload again. It's literally fine to do that as many times as needed, we're on a different branch and we'll just merge once this is all updated. 

#check observers in each year
load("data/mbbs_orange.rda")
load("data/mbbs_chatham.rda")
load("data/mbbs_durham.rda")

#easiest part - check year/route/stop1 has observer, if yes give that same observer to all the other year/route/stops
#let's make a function that can do this for just county - the processing runs on each county individualy so that's the way to go
#for data QC:
#     flag if observer doesn't show up on the conversion table?
#     flag is observer is blank
#for analysis QC:
#     flag if the route was only run by that observer in one year


#read in observer table
observer_table <- read.csv("inst/extdata/observer_conversion_table.csv", header = T)


#create test.csv
test <- mbbs_durham[1:50,]
#create new observers to add into table
test$observers[45] <- "Example Observers"
test$observers[14] <- "Example Test Observers"
test$observers[17] <- "Example Observers2"
test$observers[48] <- "Example Observers2"
#3.8.2023
test_durham <- test
#pass df


update_observer_table <- function(mbbs_county) {
  
  #load the observer conversion table
  observer_table <- read.csv("inst/extdata/observer_conversion_table.csv", header = T)
  
  #pull the county from the name of the df that was passed to the function
  selected_county <- sub('.*_', '', deparse(substitute(mbbs_county)))
  
  #lowercase county just in case naming format changes or something
  selected_county <- tolower(selected_county)
  
  #check if county is on the observer table list
  #check_county_on_list ###not implemented yet
  
  #filter the observer conversion table to just one county
  county_observer_table <- observer_table %>% filter(county == selected_county)
  
  #generate list of unique route number/observer combinations from the mbbs_county dataframe
  rocombos <- as.data.frame(unique(mbbs_county[c("route_num","observers")]))
  
  #check if each row of the newobsrtcombos is already on the conversion table
  for(i in 1:nrow(rocombos)) {
    
    #filter observer table to same route and name
    if(county_observer_table %>% filter(route == rocombos$route_num[i]) %>% filter(observers == rocombos$observers[i]) %>% nrow() > 0) { } #route/observer combo already on table, do nothing
    else { #this route/observer combo is not already on the conversion table
      
      #print border
      print("------------------------------------------------")
      
      #print the new route/observer combo
      print(paste("New route/observer combo:",list(rocombos[i,])))
      
      #filter mbbs_county to route, table(observer,year)
      print("Survey history")
      route_mbbs_county <- mbbs_county %>% filter(route_num == rocombos$route_num[i])
      print(table(route_mbbs_county$observers, route_mbbs_county$year))
      
      #print that route's current conversion table
      print("Current conversion table for this route:")
      print(county_observer_table %>% filter(route == rocombos$route_num[i]))
      
      #reprint the new route/observer combo
      print(paste("New route/observer combo:",list(rocombos[i,])))
      
      #take input on what the new conversion should be
      cat("\nWhat should the new primary observer be?:
      Type QUIT to save and exit function,
      TYPE !QUIT to exit function w/o saving,
      Type NA to not add to conversion table") #change wording
      new_primaryobs <- readline(":")
      
      if(new_primaryobs == "QUIT") {save_observer_table(observer_table); 
                                    return("Function Ended")}
      if(new_primaryobs == "!QUIT") {return("Function Ended")}
      if(new_primaryobs == "NA") {}#do nothing
      else {
      #add new row to overall observer_table
      observer_table <- observer_table %>% 
        add_row(county = selected_county,
                route = rocombos$route_num[i], 
                observers = rocombos$observers[i],
                primary_observer = new_primaryobs)
      #update the county_observer_table so the new info shows up if more than one new observer is going to be added to the route
      county_observer_table <- observer_table %>% filter(county == selected_county)
      } #end else statement about adding a new primary observer or not
      
      #give user the option to edit a row that already exists in that route
      # edit_row <- readline("Do you want to edit an existing row for this route?
      #                       Type Y to edit,
      #                       Type anything else to move to next new route/obersever combo")
      # if(edit_row == "Y") {
      #   edit_row <- readline("Type the full observers column to be changed:")
      #   #check that observers column exists, quit if it doesn't
      #   if(county_observer_table %>% filter(route == rocombos$route_num[i]) %>% 
      #                                filter(observers == edit_row) %>% nrow() > 0) {
      #   new_primaryobs <- readline("What should the new primary observer be?:")
      #   #mutate that row
      #   observer_table <- observer_table %>% mutate(
      #                     primary_observer = replace(primary_observer, 
      #                                                county == selected_county & #match county
      #                                                route == rocombos$route_num[i] & #& route
      #                                                observers == edit_row, #& observers
      #                                                new_primaryobs)) #replace w/new primaryobs
      #   } else {print("Cannot find observers as typed, moving to next new route/observer combo" )}
      
    } #end else statement about this observer/route combo not being on the conversion table
  }  #end for loop (done going through all the rocombos)
  
  #save updated version of observer conversion table
  save_observer_table(observer_table)
  print("No more new route/observer combos. Done!")
} #end function

#function to arrange + save the observer table - CURRENTLY SAVES TO TESTING
save_observer_table <- function(observer_table) {
  observer_table <- observer_table %>% arrange(county, route)
  write.csv(observer_table, 
            "inst/extdata/observer_conversion_table.csv", row.names = F)
}
###############!!!!!!!!!!!!!!!!!!!!TESTING
  

update_observer_table(test_durham)



############### Writing something to extract observer comment specifically from the places they're missing
##OOOOH okay, error is that this is overwriting old observers - because the mbbs data from the old website doesn't HAVE an observer checklist column. So either a) only fill in if checklist_comments exists OR b) if observer already has info leave as is (I think this is the better solution)
#but HMMMMMMMMMMMM shouldn't be like....blank filling for the others?
observers_extractor <- function(mbbs_county) {
  
  #fix unicode 
  mbbs_county$checklist_comments <- mbbs_county$checklist_comments %>%
                                    str_replace_all(c("&#61;"), "=")
  
  #when checklist comments contain "observer(s)", extract after observer(s) and before a ;
  mbbs_county <- mbbs_county %>% mutate(
    observers = case_when(
    is.na(observers) == FALSE ~ observers, #if observer column exists leave it
    str_detect(checklist_comments,".*[oO]bserver(s)?=") == TRUE ~
      sub(".*[oO]bserver(s)?=", "", checklist_comments) %>% #extract after observer
      {sub(";.*", "", .)}, #extract before ;
    str_detect(checklist_comments,".*[oO]bserver(s)?=") == FALSE ~ observers
  ))
  
  return(mbbs_county)
}

load("data/mbbs_orange.rda")
mbbs_orange <- observers_extractor(mbbs_orange)
update_observer_table(mbbs_chatham)

############# Function to add observers to the other route stops based on route_num stop_num =1
propogate_observers_across_stops <- function(mbbs_county) {
  mbbs_county <-  mbbs_county %>% group_by(route_num, date) %>% mutate(observers = max(observers, na.rm  = T))
  return(mbbs_county)
}
#could do it based on date as well? If it's the same route_num, same county, same date... then stop_num 1's ingo gets populated across? if there is a stop 1...
table(mbbs_orange$route_num, mbbs_orange$stop_num) #orange has stops for every route

mbbs_county <- mbbs_orange %>% filter(route_num == 12)
mbbs_county <- observers_extractor(mbbs_county)
z <- propogate_observers_across_stops(mbbs_county)

nstops <- 1:20
nroutes <- 1:length(unique(mbbs_county$route_num))

#issue is this doesn't take into account the stop_num - only stop 1 should have observers but still, might not be propogating properly
z <- mbbs_county %>% group_by(route_num, date) %>% mutate(observers = max(observers, na.rm  = T)) %>% arrange(date)
#actually, looks like that works great! spot check more later
  
  
  mutate(observers = 
  case_when(is.na(stop_num) == FALSE ~ filter(mbbs_county, stop_num == 1 & route_num == 1 & date == .$date) %>% select(observers)))

#if I was going to for loop it
#go thru whole dataset, go to route_level, go to date
#for stop_num = 2:20, observers gets stop_num == 1 $ observers

dplyr::filter(stop_num == 1 & route_num == 1 & date == date)

mbbs_orange %>% filter(route_num == 1 & stop_num == 1)



#####NOT IN USE RN
############### 3.22.2023 Writing something to extract observer comment specifically from the places they're missing
test_durham <- mbbs_durham
#let's use test_durham

test_durham$checklist_comments <- test_durham$checklist_comments %>% 
  str_replace_all(c("&#61;"), "=")



###SOMETHING BEAUTIFUL
x <- test_durham %>% mutate(
  observers = case_when(
    str_detect(checklist_comments,".*[oO]bserver(s)?=") == TRUE ~ sub(".*[oO]bserver(s)?=", "", checklist_comments) %>% {sub(";.*", "", .)}
  )
  #With pipe your data are passed as a first argument to the next function, so if you want to use it somewhere else you need to wrap the next line in {} and use . as a data "marker".
) %>% select(observers)
print(n = 200,x)
###SOMETHING BEAUTIFUL

str_detect(test_durham$checklist_comments,".*[oO]bserver(s)?=")

test_durham$checklist_comments2 <- sub(".*[oO]bserver(s)?=", "", test_durham$checklist_comments)# Extract characters after pattern
sub(";.*", "", test_durham$checklist_comments2)
#need a check to make sure observers is a field at all
###############################


##function to check if the county is on the list, not working right now
check_county_on_list <- function() { 
  #return error if county is not on the observer table list 
  if(nrow(filtered_observer_table) == 0){
    print(paste("County for this datafile is ", "\"", selected_county,"\"", " If the county is not ", list(unique(observer_table$county)), " then this function may not work as intended", sep = ""))
    
    response <- readline("Type 
    Y to acknowledge and continue anyway, 
    S to specify county, 
    or anything else to abort:")
    if(response == "S"){selected_county <- readline("Type name of county:"); return()}
    if(response == "Y"){ print(paste("County is", selected_county))}
    else{ return(print("Aborted")); return()}
  }
}


####CREATING SURVEY EVENTS 
#######3.28.2023
#process observers then -> 
load("data/mbbs_orange.rda")
load("data/mbbs_chatham.rda")
load("data/mbbs_durham.rda")
observer_table <- read.csv("inst/extdata/observer_conversion_table.csv")
mbbs_chatham <- process_observers(mbbs_chatham, "Chatham")
mbbs_orange <- process_observers(mbbs_orange, "Orange")
mbbs_durham <- process_observers(mbbs_durham, "Durham")

survey_events <- rbind(mbbs_chatham, mbbs_durham, mbbs_orange) %>%
  filter(count > 0 | count_raw > 0) %>%
  mutate(county = case_when(
    mbbs_county == "orange" ~ "Orange",
    mbbs_county == "durham" ~ "Durham",
    mbbs_county == "chatham" ~ "Chatham")) %>% 
  group_by(county, route_num, year) %>% 
  mutate(county = str_replace(county, "Wake", "Durham")) %>%
  summarize(S = n_distinct(common_name), 
            N = sum(count),
            observers = observers[!is.na(observers)][1]) %>%
  left_join(observer_table, by = c("county", "route_num" = "route", "observers"))

#fix NA counties
#rename wake to durham in code above
#save mbbs_all as rda in /data
#county = case_when mbbs_county orange ~ Orange - need to fix this elsewhere


check_county_on_list()

#get the county name, if we just want to be able to pass (mbbs_county) rather than (mbbs_county, selected_county) - figure out deparse(substitute) and if you can use it here.. deparse(quote(mbbs_county)) is ALWAYS going to return "mbbs_county" never the name of the variable that the function's been passed

ok <- function(mbbs_county){
  selected_county <- sub('.*_', '', deparse(substitute(mbbs_county))) #turn mbbs_county into a string, and then split that string to extract only what's after the _
  return(selected_county)
  
}
ok(test_durham)
#generate list of unique route numbers + observers
#check if on conversion table

# Check that routes have exactly 1 or 20 non-owling submissions.
# TODO: 2020 and after should have 20 submissions
dt %>%
  distinct(year, mbbs_county, route_num, stop_num) %>%
  group_by(year, mbbs_county, route_num) %>%
  dplyr::summarise(
    n = dplyr::n(),
    flag = !(n %in% c(1, 20))
  ) %>%
  {
    x <- .
    probs <-
      x[x$flag, ] %>%
      mutate(
        desc = glue::glue("{mbbs_county}, {year}, {route_num}")
      ) %>%
      pull(desc) %>%
      paste0(collapse = "\n * ")
    
    if (any(x$flag)) {
      warning(sprintf(
        "The following year/route don't have either 1 or 20 checklists:\n %s",
        probs
      ))
    }
  }

dt

