#------------------------------------------------------------------------------#
# Functions/workflows for cleaning and processing the observer field
#------------------------------------------------------------------------------#

#' Arranges and saves a new version of the observer table 
#' @param observer_table observer table data.frame
#' @importFrom dplyr arrange
save_observer_table <- function(observer_table, file = "inst/extdata/observer_conversion_table.csv") {
  observer_table %>% 
    arrange(county, route) %>%
    write.csv(file, row.names = FALSE)
}


#' Interactive program to update the observer table when new route 
#' + observer combos are present
#' @param mbbs_county mbbs data.frame, must end in and underscore then the name of the county ie: _durham, _orange, _chatham
#' @param selected_county county that the observer table should be filtered on
#' @importFrom dplyr filter add_row
update_observer_table <- function(mbbs_county, selected_county) {
  
  #load the observer conversion table
  observer_table <- read.csv("inst/extdata/observer_conversion_table.csv", header = T)
  
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
      
      if(new_primaryobs == "QUIT") {save_observer_table(observer_table); #save and quit
        return("Function Ended")}
      if(new_primaryobs == "!QUIT") {return("Function Ended")} #quit without saving
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



#' Pull out observers from the checklist_comments column that process_comments missed
#' @param mbbs_county mbbs data.frame
#' @importFrom stringr str_replace_all str_detect
#' @importFrom dplyr mutate case_when
#' @export
observers_extractor <- function(mbbs_county) {
  
  #fix unicode 
  mbbs_county$checklist_comments <- mbbs_county$checklist_comments %>%
    str_replace_all("&#61;| =", "=")
  
  #when checklist comments contain "observer(s)", extract after observer(s) and before a ;
  mbbs_county <- mbbs_county %>% mutate(
    observers = case_when(
      is.na(observers) == FALSE ~ observers, #if observer column exists leave it
      #if observer column is NA, extract from comments after observer and before ;
      str_detect(checklist_comments,".*[oO]bserver(s)?=") == TRUE ~
        sub(".*[oO]bserver(s)?=", "", checklist_comments) %>% #extract after observer
        {sub(";.*", "", .)}, #extract before ;
      #if observer column is NA but comments doesn't include observers, leave as is
      str_detect(checklist_comments,".*[oO]bserver(s)?=") == FALSE ~ observers
    ))
  
  return(mbbs_county)
}



#' Function to add observers to route stops 2:20
#' @param mbbs_county mbbs data.frame
#' @importFrom dplyr group_by mutate
#' @export
propogate_observers_across_stops <- function(mbbs_county) {
  #group by route and date (all observations on that route, inclusive of stops 1:20 on ebird checklists after 2019) and give all observer columns the same value as whatever column is not NA
  mbbs_county <-  mbbs_county %>% group_by(route_num, date) %>% mutate(observers = observers[!is.na(observers)][1])
  #will fill in stops 2:20 with checklist comments like v;3 and won't change data from pre-2019 because all the observations on the same route_num and date will already have the same comments/observer columns
  return(mbbs_county)
}


#' Create survey events dataframe
#' 
#' 
#' 

#' Full workflow for processing observers 
#' @param mbbs_county mbbs data.frame
#' @importFrom dplyr
#' @export
process_observers <- function(mbbs_county, county) {
  mbbs_county <- mbbs_county %>% 
    observers_extractor() %>% 
    propogate_observers_across_stops()
  
  update_observer_table(mbbs_county, county)
  
  #survey events
  
  return(mbbs_county)
}
