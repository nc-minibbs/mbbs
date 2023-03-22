library(dplyr)
library(stringr)

#old <- read.csv("inst/extdata/orange_1999-2009_from_website.csv")
#all old data has observers

#pull up R/process_comments.R and import_ebird_data.R
#run functions in there
#grab an ebird file ie any one of the csvs "inst/extdata/MyEBirdData_Orange_20220913.csv"
#use the comment_workflow() to check out things went

test <- read.csv( "inst/extdata/MyEBirdData_Orange_20220913.csv", header = T)
test <- test %>% import_ebird_data()
test <- test %>% rename_ebird_data() 
comment_workflow(test) #this isn't working yet, getting an "'Str_replace_all' is not an exported object from 'namespace:stringr'" error when trying to run it. Admittedly I'm running things all out of order, should maybe run it as
#yeah, if ur fixing the comment processer to pull observer correctly, just run the whole database upload again. It's literally fine to do that as many times as needed, we're on a different branch and we'll just merge once this is all updated. 

#check observers in each year
load("data/mbbs_orange.rda")
obs_only <- mbbs_orange %>% select(year, route_num, stop_num, notes, checklist_comments, observers, mbbs_county)
#easiest part - check year/route/stop1 has observer, if yes give that same observer to all the other year/route/stops
#let's make a function that can do this for just county - the processing runs on each county individualy so that's the way to go
#for data QC:
#     flag if observer doesn't show up on the conversion table?
#     flag is observer is blank
#for analysis QC:
#     flag if the route was only run by that observer in one year


#read in observer table
observer_table <- read.csv("inst/extdata/observer_conversion_table.csv", header = T)
#observer_table <- observer_table %>% arrange(county, route)
#write.csv(observer_table, "inst/extdata/observer_conversion_table.csv", row.names = F)


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
      cat("\nWhat should the new primary observer be?:\n
      Type QUIT to save and exit function,\n
      TYPE !QUIT to exit function w/o saving,\n
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
  
} #end function

#function to arrange + save the observer table - CURRENTLY SAVES TO TESTING
save_observer_table <- function(observer_table) {
  observer_table <- observer_table %>% arrange(county, route)
  write.csv(observer_table, 
            "inst/extdata/observer_conversion_table_TESTING.csv", row.names = F)
}
###############!!!!!!!!!!!!!!!!!!!!TESTING
  

update_observer_table(test_durham)



############### Writing something to extract observer comment specifically from the places they're missing
extract_observers_2 <- function(mbbs_county) {
  
  #fix unicode 
  mbbs_county$checklist_comments <- mbbs_county$checklist_comments %>%
                                    str_replace_all(c("&#61;"), "=")
  
  #when checklist comments contain "observer(s)", extract after observer(s) and before a ;
  mbbs_county <- mbbs_county %>% mutate(
    observers = case_when(
    str_detect(checklist_comments,".*[oO]bserver(s)?=") == TRUE ~
      sub(".*[oO]bserver(s)?=", "", checklist_comments) %>% #extract after observer
      {sub(";.*", "", .)} #extract before ;
  ))
  
  return(mbbs_county)
  }


############# Function to add observers to the other route stops based on route_num stop_num =1
#could do it based on date as well? If it's the same route_num, same county, same date... then stop_num 1's ingo gets populated across? if there is a stop 1...
table(test_durham$route_num, test_durham$stop_num)



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

