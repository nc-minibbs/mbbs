#------------------------------------------------------------------------------#
# Functions/workflows for cleaning and processing the observer field
#------------------------------------------------------------------------------#

#' Arranges and saves a new version of the observer table 
#' @param observer_table observer table data.frame
#' @importFrom dplyr arrange
save_observer_table <- function(observer_table, file = "inst/extdata/main_observer_conversion_table.csv") {
  observer_table %>% 
    arrange(mbbs_county, route_num) %>%
    write.csv(file, row.names = FALSE)
}


#' Full workflow for processing observers 
#' @param mbbs_county mbbs data.frame
#' @importFrom dplyr %>%
#' @export
process_observers <- function(mbbs_county, county) {
  mbbs_county <- mbbs_county %>% 
    observers_extractor() %>% 
    propogate_observers_across_stops()
  
  update_observer_table(mbbs_county, county)
  update_mini_observer_table()
  
  return(mbbs_county)
}


#' Interactive program to update the main observer table when new route 
#' + observer combos are present
#' @param mbbs_county mbbs data.frame, must end in and underscore then the name of the county ie: _durham, _orange, _chatham
#' @param selected_county county that the observer table should be filtered on
#' @importFrom dplyr filter add_row
update_observer_table <- function(mbbs_county, selected_county) {
  
  #load the main observer conversion table
  observer_table <- read.csv("inst/extdata/main_observer_conversion_table.csv", header = TRUE)
  
  #load survey events
  survey_list <- read.csv("inst/extdata/survey_list.csv", header = TRUE) %>% select(-S, -N)
  
  #filter the observer conversion table to just one county
  county_observer_table <- observer_table %>% filter(mbbs_county == selected_county)
  
  #generate list of unique route number/observer combinations from the mbbs_county dataframe
  rocombos <- as.data.frame(unique(mbbs_county[c("route_num","observers")]))
  
  #check if each row of the newobsrtcombos is already on the conversion table
  for(i in 1:nrow(rocombos)) {
    
    #filter observer table to same route and name as the conversion table. If there's a row, it's already on the conversion table
    if(county_observer_table %>% filter(route_num == rocombos$route_num[i]) %>% filter(observers == rocombos$observers[i]) %>% nrow() > 0) { 
      #route/observer combo already on table, do nothing
    
    #if the observer for this rocombo is NA, evaluate whether it's fine or throw an error if a year genuinely has no recorded observer either in another row or on the survey_list. 
    } else if (is.na(rocombos$observers[i]) == TRUE) {
      
      confirm_observer_NA(rocombos[i,], mbbs_county, county_observer_table, survey_list)

    } else { #this route/observer combo is not already on the conversion table, and is not NA
      
      #print border
      print("------------------------------------------------")
      
      #print the new route/observer combo
      print(paste("New route/observer combo:",list(rocombos[i,])))
      
      #print the survey history for the route
      print("Survey history")
      print(survey_list %>% filter(route_num == rocombos$route_num[i]) %>% filter(mbbs_county == selected_county))
      print("---- Current Conversion Table ----")
      print(county_observer_table[,1:4] %>% filter(route_num == rocombos$route_num[i]))
      
      #reprint the new route/observer combo
      print(paste("New route/observer combo:",list(rocombos[i,])))
      
      #take input on what the new conversion should be
      cat("\nWhat should the new primary observer be?:
      Type QUIT to save and exit function,
      TYPE !QUIT to exit function w/o saving,
      Type NA to skip to next without adding to conversion table") #change wording
      new_primaryobs <- readline(":")
      
      if(new_primaryobs == "QUIT") {save_observer_table(observer_table); #save and quit
        return("Function Ended")}
      if(new_primaryobs == "!QUIT") {return("Function Ended")} #quit without saving
      if(new_primaryobs == "NA") {}#do nothing
      else {
        #add new row to overall observer_table
        observer_table <- observer_table %>% 
          dplyr::add_row(mbbs_county = selected_county,
                  route_num = rocombos$route_num[i], 
                  observers = rocombos$observers[i],
                  primary_observer = new_primaryobs)
        #update the county_observer_table so the new info shows up if more than one new observer is going to be added to the route
        county_observer_table <- observer_table %>% filter(mbbs_county == selected_county)
      } #end else statement about adding a new primary observer or not
    } #end else statement about this observer/route combo not being on the conversion table
  }  #end for loop (done going through all the rocombos)
  
  #save updated version of observer conversion table
  save_observer_table(observer_table)
  print("No more new route/observer combos. Observer table update done!")
} #end function



#' Pull out observers from the checklist_comments column that process_comments missed
#' @param mbbs_county mbbs data.frame
#' @importFrom stringr str_replace_all str_detect
#' @importFrom dplyr mutate case_when
#' @export
observers_extractor <- function(mbbs_county) {
  
  #fix unicode 
  mbbs_county$checklist_comments <- mbbs_county$checklist_comments %>%
    stringr::str_replace_all("&#61;| =", "=")
  
  #when checklist comments contain "observer(s)", extract after observer(s) and before a ;
  mbbs_county <- mbbs_county %>% mutate(
    observers = case_when(
      is.na(observers) == FALSE ~ observers, #if observer column exists leave it
      #if observer column is NA, extract from comments after observer and before ;
      stringr::str_detect(checklist_comments,".*[oO]bserver(s)?=") == TRUE ~
        sub(".*[oO]bserver(s)?=", "", checklist_comments) %>% #extract after observer
        {sub(";.*", "", .)}, #extract before ;
      #if observer column is NA but comments doesn't include observers, leave as is
      stringr::str_detect(checklist_comments,".*[oO]bserver(s)?=") == FALSE ~ observers
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



#' Updates survey_list if needed by rbinding the new year, then updates survey_events
#' @importFrom dplyr filter group_by summarize ungroup arrange left_join mutate select ungroup n_distinct cur_group_id
#' @importFrom stringr str_to_lower
#' @param envir uses the local environment of import_data 
update_survey_events <- function(envir = parent.frame()) {
  
  #load in survey list
  survey_list <- read.csv("inst/extdata/survey_list.csv", header = TRUE)
  
  #generate the list of the latest year's surveys
  options(dplyr.summarise.inform = FALSE) #suppress dplyr "has grouped outby by"
  latest_surveys <- rbind(mbbs_chatham, mbbs_durham, mbbs_orange) %>%
    filter(count > 0 | count_raw > 0) %>%
    group_by(mbbs_county, route_num, year)%>%
    dplyr::summarize(S = dplyr::n_distinct(common_name), 
              N = sum(count),
              observers = observers[!is.na(observers)][1]) %>%
    filter(year == max(year)) %>%
    dplyr::ungroup()
  options(dplyr.summarise.inform = TRUE) #return this to normal
  
  #if the latest year is already on the survey_list, don't update. Otherwise, add in the new rows to survey_list and save the updated list
  if(max(latest_surveys$year) <= max(survey_list$year)) {
    cat(max(latest_surveys$year), "already in survey_list") #do nothing
  } else {
    
    cat(max(latest_surveys$year), "data has been added to survey_list")
    survey_list <- rbind(survey_list, latest_surveys)
    survey_list <- survey_list %>% 
      arrange(mbbs_county, route_num, year)
    write.csv(survey_list, "inst/extdata/survey_list.csv", row.names = FALSE)
    
  }

  #load in observer table
  observer_table <- read.csv("inst/extdata/main_observer_conversion_table.csv", header = TRUE)
  #Observer table may be updated several times during a year. So we regenerate and update survey_events even when we don't update(by rbinding new columns to) survey_list. 
  
  mbbs_survey_events <- left_join(survey_list, observer_table, by = c("mbbs_county", "route_num", "observers")) %>%
    group_by(primary_observer) %>%
    mutate(observer_ID = dplyr::cur_group_id()) %>%   #add observer ID
    dplyr::ungroup() %>% 
    rank_observers() 
  
  #save survey_events
  save(mbbs_survey_events, file = "data/mbbs_survey_events.rda")
  cat("\nsurvey_events updated")

}


#' Takes an observer/route combo where observer is NA and throws an error if the
#' survey for that route/year genuinely has no recorded observer either 
#' within the mbbs_county dataframe or on the survey_list   
#' @importFrom dplyr filter anti_join join_by 
#' @param rocombos a dataframe with a single route_num and observer
#' @param mbbs_county an mbbs dataset that's restricted to just one county (as we use route_num which is not distinct between counties)
#' @param county_observer_table a main_observer_table that has already been filtered to just the relevant county
#' @param survey_list list of all mbbs surveys
confirm_observer_NA <- function(rocombos, mbbs_county, county_observer_table, survey_list) {
  
  #confirm that the rocombos passed is an NA, if it's not just return and exit this function
  if(is.na(rocombos$observers) == FALSE) {
    #return("observers not NA") #for testing
    return(invisible(NULL))
  }
  
  #since the observer of this rocombo is NA (oh no! missing data?), evaluate if it should be ignored (not missing data) or should throw an error (we're missing data) 
  
  #filter to the na rows in mbbs_county that this rocombo represents 
  na_rows <- mbbs_county %>% #take county df
    filter(is.na(observers) == TRUE) %>% #filter to the NA rows
    filter(route_num == rocombos$route_num) %>% #filter to the NA rows for this route
    anti_join(survey_list, join_by(mbbs_county, route_num, year)) #only keep any rows with NA observers if that route/year combo is not already represented on the survey list. Years where observers == NA, but that are on the survey_list (and so have an observer) are cut.
  
  #evaluate if this route has all it's observers on the survey_list
  if(nrow(na_rows) == 0) {
    #the NA observer seen on this rocombos has been corrected elsewhere, it's on the survey_list. 
    #return("observers corrected elsewhere") #for testing 
    return(invisible(NULL))
  } else {
    #this NA observer is not already on the survey list. This is likely because
    #(1). It's a new year of data, observers did not propogate to all the rows, and survey_list gets updated after observer_conversion_table.
    #(2). It's a year of data where it's not on the survey_list and ALL the route's 'observers' column are NA

    #filter mbbs_county to evaluate if ANY row of data from this route/year combo contains an observers value (and will therefore have been caught in a non-NA rowcombos[i])
    non_na_rows <- mbbs_county %>% #take the county df
      filter(year %in% na_rows$year) %>% #filter to the year we've got NA values
      filter(route_num == rocombos$route_num) %>% #filter to the route in question
      filter(!is.na(observers)) #filter to any rows where in this year/route combo, observers is NOT NA
    
    #if there are NO rows in the mbbs where this route/year combo has a non-NA observer, flag the error
    if(nrow(non_na_rows) == 0) {
      print(paste("ERROR!", na_rows$year, "route", na_rows$route_num, "has only NA values for observers and no corrected record in mbbs_survey_events. Likely source of error: the ebird entry for stop 1 is missing observer information."))
    }
  #regardless of if there's an error or not, NA has now been fully evaluated.
  #return("Other row not NA") #for testing
  return(invisible(NULL))
  }  
}


#' Add new entries to the mini_observer_conversion_table
#' @importFrom dplyr filter anti_join join_by
#' @importFrom stringr str_split_fixed str_detect
#' @param rocombos a dataframe with a single route_num and observer
update_mini_observer_table <- function() {
  
  #load the main observer conversion table
  observer_table <- read.csv("inst/extdata/main_observer_conversion_table.csv", header = TRUE)
  #load the mini observer conversion table (for obs1,obs2,obs3)
  mini_observer_table <- read.csv("inst/extdata/mini_observer_conversion_table.csv", header = TRUE)
  
  #separate out observers into obs1,obs2,obs3
  observer_table[c('obs1', 'obs2', 'obs3')] <- 
    #split into at most 3 strings based off <,and> <,> <and> <&>
    stringr::str_split_fixed(observer_table$observers,
                    n = 3, 
                    pattern = ", and |, and|,and|, |,| and | and| & | &|& |&") 
  #make corrections:
  #if there's one name but split by comma ie: Driscoll, Tom
  for(w in 1:length(observer_table$observers)) {
    
    if(stringr::str_detect(string = observer_table$observers[w],
                  pattern = '^[\\w]+,\\s[\\w]+$')) { #one word comma one word
      #then give obs1 just the whole thing. It's one name.
      observer_table$obs1[w] <- observer_table$observers[w]
      observer_table$obs2[w] <- "blank"
      
    }}
  
  #make new table, get unique obs1, obs2, obs3
  obs_list <- c(observer_table$obs1, observer_table$obs2, observer_table$obs3)
  obs_list <- unique(obs_list)
  
  #take input for the output_name if it's not yet on the mini_conversion_table
  input_name <- "example";output_name <- "example";
  temp_row <- data.frame(input_name, output_name)
  for(a in 1:length(obs_list)) {
    if(mini_observer_table %>% filter(input_name == obs_list[a]) %>% nrow() > 0) { 
      #name is already on the list, do nothing
    } else {
      #name is not already on list, take input for the output name
      print("New observer name needs standardizing for the mini_observer_conversion_table:")
      print(mini_observer_table$input_name[a])
      print("What should this be converted to? Enter a standardized name or NA:")
      temp_row$input_name <- obs_list[a]
      temp_row$output_name <- readline(":")
      
      #add to mini_observer_table
      mini_observer_table <- rbind(mini_observer_table, temp_row)
    }
    
  }

  #save mini table
  write.csv(mini_observer_table, "inst/extdata/mini_observer_conversion_table.csv", row.names = FALSE)
  
  #convert obs1 obs2 and obs3 to their standardized format
  observer_table <- convert_based_on_mini_table(observer_table, mini_observer_table)
  
  #create standardized_observers
  # Specify the columns to be considered for alphabetical sorting
  obs_columns <- c("obs1", "obs2", "obs3")
  #combine obs1 obs2 and obs3 
  observer_table <- observer_table %>% rowwise() %>% mutate(standardized_observers = paste(sort(c_across(all_of(obs_columns))), collapse = ", "))
  
  save_observer_table(observer_table)
  
}


#' Standardize the names in the obs1, obs2, and obs3 columns of the main observer table
#'@importFrom dplyr left_join mutate %>% select
#' @param observer_table main_observer_table
#' @param mini_observer_table the mini_observer_table, has only columns 'input_name' and 'output name'
convert_based_on_mini_table <- function(observer_table, mini_observer_table){

  #add obs1 obs2 and obs3 to the observer_table
  observer_table <- observer_table %>%
    left_join(mini_observer_table, by = c("obs1" = "input_name")) %>%
    mutate(obs1 = output_name) %>%
    dplyr::select(-output_name) %>%
    left_join(mini_observer_table, by = c("obs2" = "input_name")) %>%
    mutate(obs2 = output_name) %>%
    dplyr::select(-output_name) %>%
    left_join(mini_observer_table, by = c("obs3" = "input_name")) %>%
    mutate(obs3 = output_name) %>%
    dplyr::select(-output_name)
  
  return(observer_table)
  
}

#' Creates a fixed effect (numeric value) of observer quality, which reflects
#' the standardized number of species the top observer in that year saw compared
#' to the mean number of species seen on that route across all years
#' observer_quality = max(obs1_Sdeviation, obs2_Sdeviation, obs3_Sdeviation, na.rm = TRUE)
#' Corrects for cases where a one-time observer accompanied a more experienced observer
#' and saw a high number of species in a particularly good year (putting their Sdeviation
#' above that of the more experienced observer)
#' @importFrom dplyr group_by summarize filter ungroup left_join relocate mutate rename select rowwise case_when 
#' @importFrom tidyr pivot_longer
#' @param mbbs_survey_events a dataframe with the list of survey events, importantly needs to include information about number of species and the observers for each survey
rank_observers <- function(mbbs_survey_events) {
  
  #goal is to create a fixed effect of observer quality, based off the standardized number of species each observer observers above the mean number of species on that route
  
  #table of average n species seen on each route
  S_average_route <- mbbs_survey_events %>% group_by(mbbs_county, route_num) %>% summarize(route_meanS = mean(S))
  
  #summary of number of mean(S) across routes for each observer, + n surveys they've done
  observer_average <- mbbs_survey_events %>%
    tidyr::pivot_longer(obs1:obs3, values_to = "obs") %>% #obs1/obs2/obs3 don't matter now
    filter(is.na(obs) == FALSE) %>% #remove NAs 
    group_by(obs) %>% #group by just how many times the observer has surveyed at all
    summarize(obs_meanS = mean(S), 
              n_surveys_obs = n())
  
  #main working table, calculate deviation from route mean n species for each observer
  observer_average_route <- mbbs_survey_events %>% 
    tidyr::pivot_longer(obs1:obs3, values_to = "obs") %>% #obs1/obs2/obs3 don't matter now
    filter(is.na(obs) == FALSE) %>% #remove NAs
    group_by(mbbs_county, route_num, obs) %>% #group to each county/route/observer
    summarize(obsroute_meanS = mean(S), 
              n_surveys_obsroute = n()) %>%
    ungroup() %>%
    #left join dfs we created above
    left_join(S_average_route, by = c("mbbs_county", "route_num")) %>%
    left_join(observer_average, by = c("obs")) %>%
    relocate(n_surveys_obsroute, n_surveys_obs, .after = "obs_meanS") %>% #readability
    #observer quality score based on observed richness compared to mean richness that's been estimated on that route. Standardize by dividing by the mean for the route
    mutate(obs_deviation = (obsroute_meanS - route_meanS)/route_meanS)
  
  ##comparing obs_rmean to mean of all other observers on that route
  #371-376, then do a nested for loop with that dataframe, for every route for every observer calculate their mean compared to the mean of other people. If there's only one observer, mean = 0
  #divide by mean of all observer's means - removes bias from n_surveys of any given observer, but remove the insufficient observerations, if n_surveys = 1, remove from mean calculation
  #if n_surveys =1, they should get a quality value of 0
  #denominator of mean richness for each combination of observers ie: /(Jen+Robin)(Jen+Robin+Noah)(Noah)(Jen+Other) - rather than /(Jen)(Robin)(Noah)
  #(observer's mean on this route - mean richness of years they are not one of the obs1-3)/(mean richness of years they are not one of the obs1-3) ie: (x-y)/y. Proportion relative to what other people observe on that route. Improvement over dividing by mean of whole route bc x influences that
  
  #can back calculate with the means - 
  #add in the number of years each route was run.
  #((route_meanS * n_surveys_route) - (obsroute_meanS * n_surveys_obsroute))/(n_surveys_route - n_surveys_obsroute) #divided by the number of years ie Marsha did not run the route. That gives y
  #if there's no other person on that route, going to throw an NA or INF, assign the NAs to 0s, observer quality does not vary for that route.
  #esentially remove her portion of the mean from consideration
  
  #each observer is going to have a mean compared to their multiple routes - but each observer needs to get a SINGLE quality value. If he ran routes 1, 4, 5 - need the mean between those routes. just another group_by mutate. And THEN take max observer quality. 
  
  #assign observer_quality based on the performance of the top observer
  mbbs_survey_events <- mbbs_survey_events %>%
    #add obs1_deviation
    left_join(observer_average_route[,c("mbbs_county", "route_num", "obs","obs_deviation", "n_surveys_obs")], by = c("mbbs_county", "route_num", "obs1" = "obs")) %>%
    rename(obs1_Sdeviation = obs_deviation,
           obs1_nsurveys = n_surveys_obs) %>%
    #add obs2_deviation
    left_join(observer_average_route[,c("mbbs_county", "route_num", "obs","obs_deviation", "n_surveys_obs")], by = c("mbbs_county", "route_num", "obs2" = "obs")) %>%
    rename(obs2_Sdeviation = obs_deviation,
           obs2_nsurveys = n_surveys_obs) %>%
    #add obs3 deviation
    left_join(observer_average_route[,c("mbbs_county", "route_num", "obs","obs_deviation", "n_surveys_obs")], by = c("mbbs_county", "route_num", "obs3" = "obs")) %>%
    rename(obs3_Sdeviation = obs_deviation,
           obs3_nsurveys = n_surveys_obs) %>%
    rowwise() %>%
    #get the maximum Sdeivation between obs1, obs2, obs, and which column it comes from
    mutate(observer_quality = max(obs1_Sdeviation, obs2_Sdeviation, obs3_Sdeviation, na.rm = TRUE), #observer quality is max btwn the three obs deviations
           max_qual_observer = which.max(c(obs1_Sdeviation, obs2_Sdeviation, obs3_Sdeviation))) %>% #record which observer was the best 
  #Correct for cases where 1-time surveyor winds up with higher Sdeviation than experienced observer
    mutate(observer_quality = case_when(
      #if there's only one observer
      (sum(is.na(c(obs1,obs2,obs3)) == FALSE) == 1) ~ observer_quality, #don't change obs_quality
      #obs 1 is the best observer but only has one survey across all routes
      (max_qual_observer == 1 & obs1_nsurveys == 1) ~ suppressWarnings(max(obs2_Sdeviation, obs3_Sdeviation, na.rm = TRUE)), #take max of obs2 and obs3
      #obs 2 is the best observer but only has one survey across all routes
      (max_qual_observer == 2 & obs2_nsurveys == 1) ~ suppressWarnings(max(obs1_Sdeviation, obs3_Sdeviation, na.rm = TRUE)), #take max of obs1 and obs3
      #obs 3 is the best observer but only has one survey across all routes
      (max_qual_observer == 3 & obs3_nsurveys == 1) ~ suppressWarnings(max(obs1_Sdeviation, obs2_Sdeviation, na.rm = TRUE)), #take max of obs1 and obs2
      #if none of the other statements are true, leave obs_quality the same
      TRUE ~ observer_quality
    )) %>%
    ungroup()
  
  rm(observer_average, observer_average_route, S_average_route)

  return(mbbs_survey_events)
}
