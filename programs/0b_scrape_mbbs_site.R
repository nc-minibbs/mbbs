#------------------------------------------------------------------------------#
#  TITLE: Import MBBS website data 
#   DATE: 20181216
#   PROG: B. Saul
#   DESC: Scrape MBBS website data and collect data into analytic file
#------------------------------------------------------------------------------#
library(rvest)
library(lubridate)
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

get_data_post2003 <- function(html, year){
  x <- html %>%
    html_nodes("table") %>%
    html_table(fill = TRUE) %>%
    .[[1]]  %>%
    ## Remove blank rows
    filter(X1 != "") %>%
    ## Remove (summary) columns after last route
    select( -(min(which(.[1, ] == "")):ncol(.))) %>%
    ## Remove the total row and below
    filter(row_number() < which(str_detect(.$X1, regex("^total", ignore_case = TRUE)) ))
  
  route_info <- x %>%
    filter(row_number() <  which(str_detect(.$X1, regex("^species", ignore_case = TRUE)) )) %>%
    filter(row_number() != which(str_detect(.$X1, regex("^habitat", ignore_case = TRUE))))
  
  hold <- t(route_info) %>% 
    .[2:nrow(.), ] %>% 
    as.data.frame(stringsAsFactors = FALSE) %>%
    setNames(trimws(str_replace(tolower(t(route_info)[1, ]), "\\%", ""))) %>%
    ## Clean up
    mutate(
      date     = as.Date(sprintf("%s/%s", date, year), format = "%m/%d/%Y"),
      vehicles = as.numeric(str_extract(vehicles, "[0-9]+")),
      year     = year(date)
    ) %>%
    select(route_num = route, 
           hab_hm = `h or m`, hab_p = p, hab_o = o, hab_b = b, 
           hab_other = `*`,
           everything()) %>%
    mutate_at(
      .vars = vars(starts_with("hab")),
      .funs = funs(ifelse(max(as.numeric(.) > 1), as.numeric(.)/100, as.numeric(.) ))
    )
  
  route_counts <-  x %>%
    filter(row_number() > which(str_detect(.$X1, regex("^species", ignore_case = TRUE)) )) %>%
    setNames(c("common_name", 1:(ncol(.) - 1) )) %>%
    tidyr::gather(key = "route_num", value = "count", -common_name) %>%
    mutate(
      count = as.numeric(count)
    )
  
  hold %>%
    left_join(route_counts, by = "route_num")
}



#####
get_year_index_html <- function(year, base_url = "http://rhwiley.bio.unc.edu/mbbs/"){
  
  if(year %in% as.character(1999:2003)){
    url <- sprintf("%s%sOrange/index.html", base_url, year)
  } else if( year == "2004"){
    url <- sprintf("%s%sOrange/orange_%s.html", base_url, year, year)
  } else {
    url <- sprintf("%s%sOrange/orange%s.html", base_url, year, year)
  }
  
  read_html(url)
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
  
  yrhtml <- get_year_index_html(year)
  
  if(year %in% as.character(1999:2003)){
    yrhtml %>% 
      get_year_links() %>%
      purrr::map_dfr(~ get_route_data(year, .x, count_tables))
  } else {
    yrhtml %>%
      get_data_post2003(year)
  }

}



### Gather all the data




## Collect all the data ####
years <- as.character(1999:2009)

old_mbbs_dt <- purrr::map_dfr(years, function(y){
  x <- get_all_year_data(y)
  if(y %in% as.character(1999:2003)){
    x <- x %>% mutate(
      date  = as.Date(date, format = "%B %d, %Y"),
      year  = year(date),
      route = str_replace(route, " -- ", ", "),
      route_num = str_extract(route, "[1-9][0-9]*"))
  }
  return(x)
})







