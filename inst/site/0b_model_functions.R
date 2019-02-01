#------------------------------------------------------------------------------#
#  Modeling Functions for MBBS results
#------------------------------------------------------------------------------#

try_model <- function(expr){
  options(warn = 2)
  m <- try(expr, silent = TRUE)
  
  if(is(m, "try-error")){ return("failed")} 
  options(warn = 1)
  m
}

gee_model <- function(data){
  m <- try_model(geepack::geeglm(
    formula = count ~ time, 
    id      = route_num,
    corstr  = "independence",
    family  = poisson(), 
    data    = data))
  
  ## 
  
  broom::tidy(m)
}

mix_model <- function(data){
  try_model(lme4::glmer(count ~ time + (1|route), family = poisson(), data = data))
}