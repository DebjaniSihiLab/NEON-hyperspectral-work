library(terra)
library(tidyverse)

f="/Users/zhuonanwang/Downloads/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img"
NLCD <- rast(f)
crs(NLCD, describe=TRUE, proj=TRUE)
#plot(NLCD)

#read in spc_mgp_sls 3 data with 30cm OC and precise location in UTM------
#data from getOC_location.R soil OC and location:
{
  user_wd="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
  wd=paste0(user_wd,"NEON-AOP/data/hs_lai/")
  setwd(wd)
  
  spc_mgp_sls_30cm_locationSOC <- read_csv(paste0(user_wd,"NEON-AOP/data/hs_lai/NEON_SOC_Locations/spc_mgp_sls_30cm_locationSOC_withPFT_Soiltype.csv"))
  spc_mgp_sls_30cm_locationSOC$rowID=1:nrow(spc_mgp_sls_30cm_locationSOC)
  
  dim(spc_mgp_sls_30cm_locationSOC) #3250   13
  
  allsites=unique(spc_mgp_sls_30cm_locationSOC$siteID)
  length(allsites) #47 we have all NEON has 47 terrestrial sites
  allsites
}

# site="BLAN" #2 utm zone
# site="STEI" #2 utm zone
# site="TREE" 
site="PUUM" 

loopsites <- allsites[!(allsites %in% c("BLAN","STEI"))]
out <- list()
i=0
for (site in loopsites){
  HARV_spc <-  spc_mgp_sls_30cm_locationSOC %>% filter(siteID == site) %>%
  arrange(rowID)
i=i+1
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
  #Now, create a SpatialPointsDataFrame of these points.
  spdf <- SpatialPointsDataFrame(coords = data.frame(x = c(east),
                                                            y = c(north)), 
                                        data = data.frame(plotID = names(east)),
                                        proj4string = sp::CRS(site_crs))
  
  points <- vect(spdf) |> terra::project(NLCD)  
  
  landcover_df <- terra::extract(NLCD, points,bind=TRUE) %>% 
    as.data.frame() %>% 
    mutate(rowID=as.numeric(plotID)) %>% 
    drop_na()
  
  out[[i]] <- landcover_df
  }

landcover_df <- out %>% 
  bind_rows %>% 
  as_tibble %>%
  distinct

final_df = left_join(spc_mgp_sls_30cm_locationSOC,landcover_df,by=c("rowID"))




