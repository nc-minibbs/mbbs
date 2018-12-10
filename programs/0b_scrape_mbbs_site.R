#------------------------------------------------------------------------------#
#
#
#
#------------------------------------------------------------------------------#
library(rvest)
library(dplyr)
library(stringr)

get_route <- function(x){
  html_nodes(x, "h4") %>% 
    html_text() %>%
    .[1] %>%
    trimws()
}

get_info <- function(x){
  html_nodes(x, "p") %>% 
    html_text() %>%
    str_replace_all(c("^\n\n" = "", "\n\n$" = "", "\n\n" = ":::")) %>%
    {.[. != ""]} %>%
    str_split(":::") %>%
    .[[1]] %>%
    str_split("--") %>%
    purrr::map(trimws) %>%
    purrr::map_dfc(function(x){
      data_frame(!! tolower(x[1]) := x[2])
    })
}

get_counts <- function(x, which_tables){
  html_nodes(x, "table") %>%
    html_table(fill = TRUE, trim = TRUE) %>%
    {.[which_tables]} %>%
    purrr::map(~ .x %>%
                 select(spec_code = X1, common_name = X2, count = X3) %>%
                 filter(spec_code != "") %>%
                 mutate(
                   count = as.character(count),
                   count = case_when(
                     count == "3/td>" ~ "3", # cleaning up 2001 route 2
                     count == "?"     ~ NA_character_, # TODO: what does ? mean?
                     TRUE             ~ count
                   )) %>%
                 mutate(
                   count = as.numeric(count))
               ) %>%
    bind_rows()

}

get_data <- function(x, count_tables){
  inf <- cbind(route = get_route(x), get_info(x))
  cnt <- get_counts(x, count_tables)
  merge(inf, cnt) %>%
    mutate_if(is.factor, as.character)
}

#####
get_year_index_html <- function(year){
  read_html(sprintf("http://rhwiley.bio.unc.edu/mbbs/%sOrange/index.html", year))
}

get_year_links <- function(year_page){
  year_page %>%
    html_nodes("li") %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    # Exclude the totals page
    {.[!str_detect(., "totals")]}
}

get_route_data <- function(year, page, count_tables){
  read_html(sprintf("http://rhwiley.bio.unc.edu/mbbs/%sOrange/%s", year, page)) %>%
    get_data(count_tables = count_tables) 
}

get_all_year_data <- function(year){
  
  if(year == "1999"){
    count_tables <- 1:2
  } else {
    count_tables <- 2:3
  }
  
  get_year_index_html(year) %>% 
    get_year_links() %>%
    purrr::map_dfr(~ get_route_data(year, .x, count_tables))
}



### Gather all the data
years <- as.character(1999:2009)
dt <- purrr::map_dfr(years, ~get_all_year_data(.x))

dt
