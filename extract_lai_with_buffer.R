library("rgdal")
library(sf)
library(terra)
library(raster)
library(tidyverse)
NEON_TOKEN <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJ6aHVvbmFuLndhbmdAZW1vcnkuZWR1Iiwic2NvcGUiOiJyYXRlOnB1YmxpYyIsImlzcyI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvIiwiZXhwIjoxNzg3Njk4ODU4LCJpYXQiOjE2MzAwMTg4NTgsImVtYWlsIjoiemh1b25hbi53YW5nQGVtb3J5LmVkdSJ9.Ug3DuSLWLT0NsZy5pHKs8Wh51C9-tjxoCccbhrUW0m49n1GikPp0EQ3jZc3c6FFzRpDk04UXA-k4fECyxzqnmQ"

#domain10_sites
#"CPER" "RMNP" "STER"
extract_HS_with_buffer <- function(spc_mgp_sls_30cm_locationSOC,
                                   site="CPER",
                                   year=2021,
                                   buffer=3){
  #setwd to home, then check which pc.
  setwd("~")
  whichpc=getwd()
  cat(whichpc)
  if(whichpc=="/Users/zwang61"){
    user_wd="/Users/zwang61/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }else{
    user_wd="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  }
  #wd set to my external hard drive-seagate----
  # we want to save our files. Be sure to move the download into your working directory!
  # wd="/Volumes/seagate/"
  # setwd(wd)
  wd=paste0(user_wd,"NEON-AOP/data/hs_lai")
  setwd(wd)
  
  cat("LAI data:---DP2.30012.001 NEON-AOP flight line. \n Site:",site,"\n")  
  
  HARV_spc <-  spc_mgp_sls_30cm_locationSOC %>% filter(siteID == site) %>%
    arrange(rowID)
 
  #We want the soil plot hyperspectral data, all plots have soil(spc) measurements
  # cat(HARV_spc$utmZone)
  east <- HARV_spc$piteasting
  names(east) <- HARV_spc$rowID
  north <- HARV_spc$pitnorthing
  names(north) <- HARV_spc$rowID
  # cat(paste0("east: ",east,"\n"))
  # cat(paste0("north: ",east,"\n"))
  utm <- unique(HARV_spc$utmZone)
  cat(utm,"\n")
  if(length(utm)!=1){
    cat("This site ",site, " has more than one UTM zone! stop \n")
    cat(site,file="outfile_wrongUTM_files.txt",sep="\n",append=TRUE)
  }
  site_crs <- paste0("+proj=utm +zone=",parse_number(utm))
  
  east_addbuffer <- NULL
  north_addbuffer <- NULL
  if(buffer>0){
    for(k in 1:length(east)) {
      bufferEasting <- c(east[k]+1:buffer,east[k],east[k]-1:buffer)
      # names(bufferEasting) <- rep(names(east[k]),buffer+buffer+1)
      bufferNorthing <- c(north[k]+1:buffer,north[k],north[k]-1:buffer)
      # names(bufferNorthing) <- rep(names(north[k]),buffer+buffer+1)                
      buffer_pixels <- expand_grid(bufferEasting,bufferNorthing)        
      
      east_windows <- buffer_pixels$bufferEasting
      names(east_windows) <- rep(names(east[k]),length(east_windows))
      north_windows <- buffer_pixels$bufferNorthing
      names(north_windows) <- rep(names(north[k]),length(north_windows))
      
      east_addbuffer <- c(east_addbuffer,east_windows )
      north_addbuffer <- c(north_addbuffer,north_windows )
    }
  }
  
  #Now, create a SpatialPointsDataFrame of these points.
  spdf_buffer <- SpatialPointsDataFrame(coords = data.frame(x = c(east_addbuffer),
                                                            y = c(north_addbuffer)), 
                                        data = data.frame(plotID = names(east_addbuffer)),
                                        proj4string = sp::CRS(site_crs))
  
  
  # query the products endpoint for the product requested
  source(paste0(user_wd,"/NEON-AOP/code/My_functions_R/getAPI.R"))
  prod.req <- getAPI(apiURL = paste("http://data.neonscience.org/api/v0/products/", "DP2.30012.001", sep=""), 
                     token = NEON_TOKEN)
  avail <- jsonlite::fromJSON(httr::content(prod.req, as='text', encoding='UTF-8'), 
                              simplifyDataFrame=TRUE, flatten=TRUE)
  
  # get the urls for months with data available, and subset to site
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)
  month.urls <- grep(site, month.urls, value = TRUE)
  
  avilb_year <- str_split(month.urls, "/") %>% 
    map(9) %>% 
    substr(1,4) %>% 
    as.numeric()
  
  latest_year <- max(avilb_year)
  cat("\n",paste0("site:",site,"\navailable AOP flight line data in year: "),avilb_year,
      "\nlatest year: ",latest_year,"\n")
  if(year==0){
    cat("didn't specify year, use  available latest_year","\n")
  }else{
    if(!year %in% avilb_year){
      cat("user choose year is not in available year at this site.----->\n",
          "use  available latest_year",latest_year,"\n\n")
    }else{
      latest_year <- year
      cat("download user choose year, latest_year=", year,"\n")
    }
  }
  

  dir <- paste0(wd,"/DP2.30012.001/neon-aop-products/",latest_year,"/FullSite/",unique(HARV_spc$domainID))
  dirs <- list.dirs(dir)
  sites_dirs <- grep(paste0(latest_year,"_",site), dirs,value =TRUE)
  files_corrected=list.files(sites_dirs, pattern = "LAI.tif$",full.names = TRUE)

  
  #  extract lai tif data at these locations.
  out <- list()
  for (i in seq_along(files_corrected)) {
    r=rast(files_corrected[i])
    points <- terra::vect(spdf_buffer)
    res <- terra::extract(r, points, xy=TRUE,ID=TRUE,bind=TRUE)
    #on CADES HPC, terra::extract return data.frame with ID. wired!
    # data = data.frame(plotID = names(east_addbuffer))
    # res <- cbind(res,data)
    hs_df <-  as_tibble(res) %>% 
      drop_na()
    names(hs_df)[2]='LAI'
    out[[i]] <- hs_df
  }
  
  hs_df_wide <- out %>% 
    bind_rows %>% 
    as_tibble %>%
    dplyr::select(plotID, east=x,north=y, ends_with('LAI')) %>% 
    distinct
  
 # test= hs_df_wide %>%   group_by(plotID,east, north) %>% 
 #    summarise( n=n())
  
  write_csv(hs_df_wide, 
            paste0(wd,"/DP2.30012.001/",
                   unique(HARV_spc$domainID),"_",site,"_",latest_year,"_","LAI_buf_",buffer,"m_wide.csv")
  )
  
  
#   hs_df_long = hs_df_wide%>%
#     gather(band, reflectance, 
#            -plotID,-east,-north) %>% 
#   mutate(wavelength = parse_number(band))%>% 
#     # mutate(reflectance = case_when((wavelength>1340&wavelength<1445) ~NA,
#     #                                (wavelength>1790&wavelength<1955)~NA,
#     #                                TRUE ~ reflectance)) %>% 
#     mutate(reflectance = ifelse(wavelength>1340&wavelength<1445, NA,reflectance)) %>% 
#     mutate(reflectance = ifelse(wavelength>1790&wavelength<1955, NA,reflectance)) %>% 
#     group_by(plotID, wavelength) %>% 
#     summarise(reflectance = mean(reflectance),
#               n=n())
#   
# write_csv(hs_df_long, 
#             paste0(wd,"DP2.30012.001/",
#                    unique(HARV_spc$domainID),"_",site,"_",latest_year,"_","HS_buf_",buffer,"m_long.csv")
#           )
#   

}

#==============================================================
#setwd to home, then check which pc.
setwd("~")
whichpc=getwd()
cat(whichpc)
if(whichpc=="/Users/zwang61"){
  user_wd="/Users/zwang61/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
}else{
  user_wd="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
} 

spc_mgp_sls_30cm_locationSOC <- read_csv(paste0(user_wd,"NEON-AOP/data/hs_lai/NEON_SOC_Locations/spc_mgp_sls_30cm_locationSOC.csv"))
spc_mgp_sls_30cm_locationSOC$rowID=1:nrow(spc_mgp_sls_30cm_locationSOC)
allsites=unique(spc_mgp_sls_30cm_locationSOC$siteID)
length(allsites) #47 we have all NEON has 47 terrestrial sites
allsites

domainID_10 <- spc_mgp_sls_30cm_locationSOC %>% 
  filter(domainID=="D10")
domain10_sites=unique(domainID_10$siteID)
domain10_sites
#"CPER" "RMNP" "STER"
#--------------------------------------------------------
extract_HS_with_buffer(spc_mgp_sls_30cm_locationSOC,
                       site="CPER",year=2021,buffer=3)
extract_HS_with_buffer(spc_mgp_sls_30cm_locationSOC,
                       site="RMNP",year=2021,buffer=3)
extract_HS_with_buffer(spc_mgp_sls_30cm_locationSOC,
                       site="STER",year=2021,buffer=3)
