library(terra)
library(tidyverse)

f="/Users/zhuonanwang/Downloads/nlcd_2019_land_cover_l48_20210604/nlcd_2019_land_cover_l48_20210604.img"
NLCD <- rast(f)
crs(NLCD, describe=TRUE, proj=TRUE)
#plot(NLCD)

soc_30cm_df <- read_csv("/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/iscn/ISCN_30cm_USA_UTM.csv") 
#some sites have different date!
  #Now, create a SpatialPointsDataFrame of these points.
  spdf <- SpatialPointsDataFrame(coords = data.frame(x = soc_30cm_df$long,
                                                     y = soc_30cm_df$lat), 
                                        data = data.frame(plotID = soc_30cm_df$profile_name),
                                        proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  points <- vect(spdf) |> terra::project(NLCD)  
  
  landcover_df <- terra::extract(NLCD, points,bind=TRUE) %>% 
    as.data.frame() %>% 
    drop_na() %>% 
  as_tibble %>%
  distinct
  
  names(landcover_df)[2]="nlcd"
  unique(landcover_df$nlcd)
 #NEON plots
  soc_df <- read_csv("NEON_SOC_Locations/spc_mgp_sls_30cm_locationSOC_withPFT_Soiltype.csv")
  unique(soc_df$nlcdClass)
  
  
  landcover_df=landcover_df %>%
    mutate(nlcdClass = case_when(nlcd== "Woody Wetlands" ~ "woodyWetlands",
                                 nlcd== "Hay/Pasture" ~ "pastureHay",
                                 nlcd== "Cultivated Crops" ~ "cultivatedCrops",
                                 nlcd== "Mixed Forest" ~ "mixedForest",
                                 nlcd== "Evergreen Forest" ~ "evergreenForest",
                                 nlcd== "Shrub/Scrub" ~ "shrubScrub",
                                 nlcd== "Herbaceous" ~ "sedgeHerbaceous",
                                 nlcd== "Deciduous Forest" ~ "deciduousForest",
                                 nlcd== "Emergent Herbaceous Wetlands" ~ "emergentHerbaceousWetlands",
                               TRUE ~ NA
    )) 
  write_csv(landcover_df, "/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/iscn/ISCN_nlcdClass.csv")
  




