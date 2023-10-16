library("rgdal")
library(sf)
library(terra)
library(raster)
library(tidyverse)
NEON_TOKEN <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJ6aHVvbmFuLndhbmdAZW1vcnkuZWR1Iiwic2NvcGUiOiJyYXRlOnB1YmxpYyIsImlzcyI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvIiwiZXhwIjoxNzg3Njk4ODU4LCJpYXQiOjE2MzAwMTg4NTgsImVtYWlsIjoiemh1b25hbi53YW5nQGVtb3J5LmVkdSJ9.Ug3DuSLWLT0NsZy5pHKs8Wh51C9-tjxoCccbhrUW0m49n1GikPp0EQ3jZc3c6FFzRpDk04UXA-k4fECyxzqnmQ"


extract_HS_with_buffer <- function( site="TOOL",year=2021,buffer=3){
  user_wd="/lustre/or-scratch/cades-ccsi/scratch/z2w/NEON_hs/"
  wd="/lustre/or-scratch/cades-ccsi/scratch/z2w/NEON_hs/data/"
  setwd(wd)

  load(file='shared_flights.rda')
  shared_flights <- shared_flights[-2,]
  
  if(site %in% shared_flights$site){
    flightsite=shared_flights$flightSite[which(site==shared_flights$site)]
  }else{
    flightsite=site
  }
  
  # query the products endpoint for the product requested
  source(paste0(user_wd,"/code/My_functions_R/getAPI.R"))
  prod.req <- getAPI(apiURL = paste("http://data.neonscience.org/api/v0/products/", "DP1.30006.001", sep=""), 
                     token = NEON_TOKEN)
  avail <- jsonlite::fromJSON(httr::content(prod.req, as='text', encoding='UTF-8'), 
                              simplifyDataFrame=TRUE, flatten=TRUE)
  
  # get the urls for months with data available, and subset to site
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)
  month.urls <- grep(flightsite, month.urls, value = TRUE)
  
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
  
  if(site=="CHEQ"){
    latest_year <- year
  }
  
  dir <- paste0(wd,"/DP1.30006.001/neon-aop-products/",latest_year,"/FullSite/")
  dirs <- list.dirs(dir)
  sites_dirs <- grep(paste0(latest_year,"_",flightsite), dirs,value =TRUE)
  files_corrected=list.files(sites_dirs, pattern = "reflectance_BRDF_topo_corrected$",full.names = TRUE)

  r=rast(files_corrected[1])
  x=crs(r, describe=TRUE, proj=TRUE)$proj
  utmzone=as.numeric(substr(x,17,18))
  
  iscn_locationSOC <- read_csv(paste0(user_wd,"ISCN_30cm_USA_UTM.csv"))
    
    HARV_spc <- iscn_locationSOC %>% filter(utm==utmzone)
    
    east <- HARV_spc$east
    names(east) <- HARV_spc$profile_name
    north <- HARV_spc$north
    names(north) <- HARV_spc$profile_name
    
    site_crs <- paste0("+proj=utm +zone=",utmzone)
    
    #Now, create a SpatialPointsDataFrame of these points.
    spdf_buffer <- SpatialPointsDataFrame(coords = data.frame(x = c(east),
                                                              y = c(north)), 
                                          data = data.frame(plotID = names(east),
                                                            site_ID=site),
                                          proj4string = sp::CRS(site_crs))

  #  extract lai tif data at these locations.
  out <- list()
  for (i in seq_along(files_corrected)) {
    r=rast(files_corrected[i])
    points <- terra::vect(spdf_buffer)
    res <- terra::extract(r, points, xy=TRUE,ID=TRUE,bind=TRUE)
    #on CADES HPC, terra::extract return data.frame with ID. wired!
    data = data.frame(plotID = names(east))
    data$siteID=site
    res <- cbind(res,data)
    hs_df <-  as_tibble(res) %>% 
      drop_na()
    out[[i]] <- hs_df
  }
  
    hs_df_wide <- out %>% 
      bind_rows %>% 
      as_tibble %>%
      dplyr::select(plotID,siteID, east=x,north=y, ends_with('nanometers')) %>% 
      distinct
  
  
  write_csv(hs_df_wide, 
            paste0(wd,"DP1.30006.001/ISCN/",
                   site,"_",latest_year,"_","ISCN","_wide.csv")
  )
  
  
  hs_df_long = hs_df_wide%>%
    gather(band, reflectance, 
           -plotID,-siteID,-east,-north) %>% 
  mutate(wavelength = parse_number(band))%>% 
    # mutate(reflectance = case_when((wavelength>1340&wavelength<1445) ~NA,
    #                                (wavelength>1790&wavelength<1955)~NA,
    #                                TRUE ~ reflectance)) %>% 
    mutate(reflectance = ifelse(wavelength>1340&wavelength<1445, NA,reflectance)) %>% 
    mutate(reflectance = ifelse(wavelength>1790&wavelength<1955, NA,reflectance)) %>% 
    group_by(plotID, wavelength) %>% 
    summarise(reflectance = mean(reflectance),
              n=n())
  
write_csv(hs_df_long, 
            paste0(wd,"DP1.30006.001/ISCN/",
                   site,"_",latest_year,"_","ISCN","_long.csv")
          )
  

}

  

#==============================================================
user_wd="/lustre/or-scratch/cades-ccsi/scratch/z2w/NEON_hs/"
spc_mgp_sls_30cm_locationSOC <- read_csv(paste0(user_wd,"spc_mgp_sls_30cm_locationSOC_withPFT_Soiltype.csv"))
spc_mgp_sls_30cm_locationSOC$rowID=1:nrow(spc_mgp_sls_30cm_locationSOC)
allsites=unique(spc_mgp_sls_30cm_locationSOC$siteID)
length(allsites) #47 we have all NEON has 47 terrestrial sites + CHEQ total 48
allsites
validsites <- allsites[!allsites%in% c("LAJA")]
neonsites <- read_csv(paste0(user_wd,"neon_hs_sites.csv"))
neonsites$site %>% length()

for(a_site in validsites){
  siteyear=neonsites[neonsites$site==a_site,2][[1]]
  #run function:
  extract_HS_with_buffer(spc_mgp_sls_30cm_locationSOC,site=a_site,
                         year=siteyear,buffer=3)
}


