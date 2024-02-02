###
#modifying observer_conversion_table, and drafting out changes to process_observers
library(dplyr)
library(stringr)

observer_table <- read.csv("C:/git/mbbs/inst/extdata/main_observer_conversion_table.csv", header = TRUE)
survey_list <- read.csv("C:/git/mbbs/inst/extdata/survey_list.csv", header = TRUE)

#separate out observers into obs1,obs2,obs3
observer_table[c('obs1', 'obs2', 'obs3')] <- 
  #split into at most 3 strings based off <,and> <,> <and> <&>
  str_split_fixed(observer_table$observers,
                      n = 3, 
                      pattern = ", and |, and|,and|, |,| and | and| & | &|& |&") 
  #make corrections:
  #if there's one name but split by comma ie: Driscoll, Tom
for(w in 1:length(observer_table$observers)) {
  
  if(str_detect(string = observer_table$observers[w],
                pattern = '^[\\w]+,\\s[\\w]+$')) { #one word comma one word
    #then give obs1 just the whole thing. It's one name.
    observer_table$obs1[w] <- observer_table$observers[w]
    observer_table$obs2[w] <- "blank"
    
  }}

#make new table, get unique obs1, obs2, obs3
obs_list <- c(observer_table$obs1, observer_table$obs2, observer_table$obs3)
obs_list <- unique(obs_list)

#that's the input_name for mini_observer_conversion_table
mini_observer_table <- read.csv("C:/git/mbbs/inst/extdata/mini_observer_conversion_table.csv", header = TRUE)

#take input for the out_put name if it's not yet on the mini_conversion_table
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

#then take input for the output_name
#for(a in 1:length(obs_list)){
#  #set input name
#  mini_observer_table[a,1] <- obs_list[a]
#  print(mini_observer_table$input_name[a])
#  #set output name
# mini_observer_table[a,2] <- readline(":")
#}

#write.csv(mini_observer_table, "C:/git/mbbs/inst/extdata/mini_observer_conversion_table.csv", row.names = FALSE)

#convert obs1 obs2 and obs3 based on mini_observer_conversion_table to the correct spellings
convert_based_on_mini_table <- function(observer_table, mini_observer_table){

  observer_table <- observer_table %>%
    left_join(mini_observer_table, by = c("obs1" = "input_name")) %>%
    mutate(obs1 = output_name) %>%
    select(-output_name) %>%
    left_join(mini_observer_table, by = c("obs2" = "input_name")) %>%
    mutate(obs2 = output_name) %>%
    select(-output_name) %>%
    left_join(mini_observer_table, by = c("obs3" = "input_name")) %>%
    mutate(obs3 = output_name) %>%
    select(-output_name)
 
 return(observer_table)
}

observer_table <- convert_based_on_mini_table(observer_table, mini_observer_table)

# Specify the columns to be considered for alphabetical sorting
obs_columns <- c("obs1", "obs2", "obs3")

#combine obs1 obs2 and obs3 into the final observers
observer_table <- observer_table %>% rowwise() %>% mutate(standardized_observers = paste(sort(c_across(all_of(obs_columns))), collapse = ", "))



