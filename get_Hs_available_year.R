get_Hs_available_year <- function(site,whichpc){
  
  if(whichpc=="/Users/zwang61"){
    user_wd="/Users/zwang61/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }else{
    user_wd="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }
  
  source(paste0(user_wd,"/NEON-AOP/code/My_functions_R/getAPI.R"))
  prod.req <- getAPI(apiURL = paste("http://data.neonscience.org/api/v0/products/", "DP1.30006.001", sep=""), 
                     token = NEON_TOKEN)
  avail <- jsonlite::fromJSON(httr::content(prod.req, as='text', encoding='UTF-8'), 
                              simplifyDataFrame=TRUE, flatten=TRUE)
  # error message if product not found
  if(!is.null(avail$error$status)) {
    stop(paste("No data found for product","DP3.30012.001", sep=" "))
  }
  
  # get the urls for months with data available, and subset to site
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)
  month.urls <- grep(site, month.urls, value = TRUE)
  
  avilb_year <- str_split(month.urls, "/") %>% 
    map_vec(9) %>% 
    substr(1,4) %>% 
    as.numeric()
  return(avilb_year)
}


NEON_TOKEN <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJ6aHVvbmFuLndhbmdAZW1vcnkuZWR1Iiwic2NvcGUiOiJyYXRlOnB1YmxpYyIsImlzcyI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvIiwiZXhwIjoxNzg3Njk4ODU4LCJpYXQiOjE2MzAwMTg4NTgsImVtYWlsIjoiemh1b25hbi53YW5nQGVtb3J5LmVkdSJ9.Ug3DuSLWLT0NsZy5pHKs8Wh51C9-tjxoCccbhrUW0m49n1GikPp0EQ3jZc3c6FFzRpDk04UXA-k4fECyxzqnmQ"
library(tidyverse)


get_Hs_available_year("LAJA",whichpc=getwd())

get_Hs_available_year("ORNL",whichpc=getwd())

get_Hs_available_year("ABBY",whichpc=getwd())
get_Hs_available_year("GUAN",whichpc=getwd())
get_Hs_available_year("TOOL",whichpc=getwd())
get_Hs_available_year("STEI",whichpc=getwd())
get_Hs_available_year("DEJU",whichpc=getwd())
get_Hs_available_year("OAES",whichpc=getwd())
get_Hs_available_year("TALL",whichpc=getwd())

get_Hs_available_year("GRSM",whichpc=getwd())

get_Hs_available_year("DSNY",whichpc=getwd())
get_Hs_available_year("JERC",whichpc=getwd())
get_Hs_available_year("OSBS",whichpc=getwd())

get_Hs_available_year("BLAN",whichpc=getwd())
get_Hs_available_year("SCBI",whichpc=getwd())
get_Hs_available_year("SERC",whichpc=getwd())

get_Hs_available_year("BONA",whichpc=getwd())
get_Hs_available_year("TEAK",whichpc=getwd())
get_Hs_available_year("SOAP",whichpc=getwd())



