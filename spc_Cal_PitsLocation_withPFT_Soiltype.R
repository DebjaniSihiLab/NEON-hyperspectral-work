library(tidyverse)
library(geoNEON)
NEON_TOKEN <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJ6aHVvbmFuLndhbmdAZW1vcnkuZWR1Iiwic2NvcGUiOiJyYXRlOnB1YmxpYyIsImlzcyI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvIiwiZXhwIjoxNzg3Njk4ODU4LCJpYXQiOjE2MzAwMTg4NTgsImVtYWlsIjoiemh1b25hbi53YW5nQGVtb3J5LmVkdSJ9.Ug3DuSLWLT0NsZy5pHKs8Wh51C9-tjxoCccbhrUW0m49n1GikPp0EQ3jZc3c6FFzRpDk04UXA-k4fECyxzqnmQ"

#10047 spc estimatedOC
#spc:Soil physical and chemical properties, distributed initial characterization DP1.10047.001
dir <- file.path("/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NSF-SOC/NSF-MSA-SOC/neon_data/")
dir <- file.path("/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/neon_data/") 
portalFolder <- "/portal"
outputs <- "/compiled"

spc_Cal_PitsLocation <- function(){
#to get easting northing,utmHemisphere utmZoneNumber columns:
spc_spatial <- read_csv(paste0(dir, outputs, "/spatial/spatial_spc.csv") ) %>% 
  filter(!is.na(decimalLatitude))

# In the soil physical properties data product, 
# if referenceCorner, sampleDistance and sampleBearing are provided, 
# users will be able to calculate more precise geolocations of the soil pit 

#to get  referenceCorner, sampleDistance and sampleBearing columns
spc_perplot <- read.csv(
  "/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/neon_data/portal/filesToStack10047/stackedFiles/spc_perplot.csv",
  header = T, na.strings = "", stringsAsFactors = F)
#data has a problem at "MLBS_064", correct its decimalLongitude
spc_perplot[spc_perplot$plotID=="MLBS_064","decimalLongitude"]=spc_spatial[spc_spatial$plotID=="MLBS_064","decimalLongitude"] %>% pull()
spc_perplot[spc_perplot$plotID=="MLBS_064","decimalLatitude"]=spc_spatial[spc_spatial$plotID=="MLBS_064","decimalLatitude"] %>% pull()


spc_table <- left_join(spc_spatial,spc_perplot,by=c(
  "namedLocation", "domainID", "siteID",
  "decimalLatitude", "decimalLongitude", "geodeticDatum",
  "nlcdClass", "plotID", "plotType")) %>% 
  select(namedLocation,domainID,siteID,pitID,plotID,
         decimalLatitude,decimalLongitude,easting,northing,utmHemisphere,utmZoneNumber,
         utmZone,referenceCorner,sampleDistance,sampleBearing,
         #add some covariables:
         elevation=elevation.y,maximumElevation,minimumElevation,
         nlcdClass,slopeAspect,slopeGradient,
         soilTypeOrder,soilSeries,soilSubgroup,
         soilGreatGroup,soilSuborder,soilOrder)
names(spc_table)

precise_spc <- spc_table %>% 
  mutate(referencecornernorthing=case_when(referenceCorner == "NW40"~ (northing+20),
                                           referenceCorner == "SW40"~ (northing-20),
                                           referenceCorner == "SW20"~ (northing-10),
                                           referenceCorner == "NE20"~ (northing+10),
                                           referenceCorner == "NE40"~ (northing+20),
                                           referenceCorner == "SE20"~ (northing-10),
                                           referenceCorner == "SE40"~ (northing-20),
                                           referenceCorner == "NW20"~ (northing+10),
                                           .default = NA
                                           )
         )%>% 
  mutate(referencecornereasting =case_when(referenceCorner == "NW40"~ (easting-20),
                                           referenceCorner == "SW40"~ (easting-20),
                                           referenceCorner == "SW20"~ (easting-10),
                                           referenceCorner == "NE20"~ (easting+10),
                                           referenceCorner == "NE40"~ (easting+20),
                                           referenceCorner == "SE20"~ (easting+10),
                                           referenceCorner == "SE40"~ (easting+20),
                                           referenceCorner == "NW20"~ (easting-10),
                                           .default = NA)
  ) %>% 
  mutate(theta = case_when(sampleBearing < 90 ~ ( 90-sampleBearing),
                           sampleBearing >= 90 ~ ( 450-sampleBearing),
                           .default = NA)
  ) %>% 
  mutate(pitnorthing = referencecornernorthing+sampleDistance*sin(theta* pi/180)) %>% 
  mutate(piteasting = referencecornereasting+sampleDistance*cos(theta* pi/180)) 
  # mutate(ndistance=sampleDistance*sin(theta* pi/180)) %>% 
  # mutate(edistance=sampleDistance*cos(theta* pi/180))
  
# convert degrees to radians first:
#   // [radians] = [degrees] * [pi]/180
#  theta = 100.7 * pi/180
#  // sin(1.757 radians) == ~0.98
#  result = sin(theta)
return(precise_spc)
}




