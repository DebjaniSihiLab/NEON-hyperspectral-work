#SOC
### Set directories - new users add your own path as needed
if (file.exists('/Users/zhuonanwang/')){
  dir <- file.path("/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NSF-SOC/NSF-MSA-SOC/neon_data") 
  portalFolder <- "/portal"
  outputs <- "/compiled"                                                                       
}
#INITIAL CHAR SOIL: spc ---------------------------------
{
  spc_perbiogeo <- read.csv(paste0
                            (dir, portalFolder, "/filesToStack10047/stackedFiles/spc_biogeochem.csv"), 
                            header = T, na.strings = "", stringsAsFactors = F)
  spc_bulkdens <- read.csv(paste0
                           (dir, portalFolder, "/filesToStack10047/stackedFiles/spc_bulkdensity.csv"), 
                           header = T, na.strings = "", stringsAsFactors = F)
  spc_particlesize <- read.csv(paste0
                               (dir, portalFolder, "/filesToStack10047/stackedFiles/spc_particlesize.csv"), 
                               header = T, na.strings = "", stringsAsFactors = F)
  spc_perplot <- read.csv(paste0
                          (dir, portalFolder, "/filesToStack10047/stackedFiles/spc_perplot.csv"), 
                          header = T, na.strings = "", stringsAsFactors = F)
  
}
#clear layer data:
{
  spc_perplot <- mutate(spc_perplot, nrcsDescriptionID = toupper(nrcsDescriptionID))
  # 3 horizons have > 1 BD measurements - select only one (clod preferred)
  spc_bulkdens_1 <- spc_bulkdens %>%
    dplyr::filter(!(horizonID == "16N02473" & bulkDensSampleType == "clod"),
                  !(horizonID == "16N02477" & bulkDensSampleType == "compliant cavity"),
                  !(horizonID == "18N02724" & bulkDensSampleType == "compliant cavity"))
  
  spc_bulkdens_dups <- spc_bulkdens_1 %>%
    dplyr::group_by(nrcsDescriptionID, horizonID, horizonName) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::filter(n > 1)# shoul be empty, good
  
  spc_bulkdens_1 <- spc_bulkdens_1 %>% 
    mutate(bulkDensDry = bulkDensFieldMoist/(1+fieldWaterContent/100))
  
  # Start with chem, then add on bulk density, particle size, and plot metadata
  spc_combined <- spc_perbiogeo %>%
    left_join(spc_bulkdens_1, by = c("namedLocation", "domainID", "siteID", "plotID", "nrcsDescriptionID",
                                     "horizonID", "horizonName")) %>%
    #dplyr::mutate(bulkDensCombined = ifelse(is.na(bulkDensFieldMoist), bulkDensThirdBar, bulkDensFieldMoist)) %>%
    dplyr::mutate(bulkDensCombined = ifelse(is.na(bulkDensDry), bulkDensOvenDry, bulkDensDry)) %>%
    dplyr::left_join(spc_particlesize, by = c("namedLocation", "domainID", "siteID", "plotID", "nrcsDescriptionID",
                                              "horizonID", "horizonName",  "biogeoIDnrcs", "biogeoSampleType",
                                              "biogeoTopDepth", "biogeoBottomDepth", "biogeoCenterDepth")) %>%
    dplyr::mutate(coarseFrac2to20 = (coarseFrag2To5 + coarseFrag5To20)*0.1) %>%
    dplyr::left_join(spc_perplot, by = c("namedLocation", "domainID", "siteID","plotID")) %>%
    dplyr::arrange(plotID, biogeoTopDepth) %>%
    dplyr::select(c(domainID,siteID,plotID,namedLocation,nrcsDescriptionID=nrcsDescriptionID.x,
                    decimalLatitude,decimalLongitude,
                    collectDate.x,horizonName,biogeoTopDepth,biogeoBottomDepth,biogeoCenterDepth,
                    carbonTot,estimatedOC,
                    bulkDensCenterDepth,bulkDensTopDepth,bulkDensBottomDepth,
                    bulkDensCombined,
                    coarseFrac2to20)) 
}

spc.layer <- spc_combined %>% 
  filter(siteID %in% c("HARV")) %>% 
  # filter(plotID %in% c("HARV_001","SRER_004","WOOD_009")) %>% 
  mutate(estimatedOC_percent = estimatedOC/10) %>% 
  mutate(bd_pedo =  1/(0.6268 + 0.0361 *estimatedOC_percent* 1.724)) %>% 
  mutate(bulkDenALL = ifelse(is.na(bulkDensCombined), bd_pedo, bulkDensCombined))  %>%
  mutate(bulkDenALL_method = ifelse(is.na(bulkDensCombined), "bd_pedo", "bulkDensCombined"))  %>%
  mutate(layer_top=biogeoTopDepth) %>% 
  filter(layer_top < 30) %>% #if layer_top is NA, NA will not include by filter
  filter((is.finite(biogeoBottomDepth))) %>% 
  mutate(layer_bottom = if_else(biogeoBottomDepth > 30, 30,as.numeric(biogeoBottomDepth)))%>% 
  filter((layer_top < layer_bottom)) %>% 
  mutate(`Calculate_soc_g_m-2` = 
           estimatedOC_percent*bulkDenALL*100*(1-coarseFrac2to20*0.01)*(layer_bottom-layer_top)) %>%  
  mutate(layer_depth = (layer_bottom-layer_top))%>% #gC/m2
  #g/m2 to 0.001 kg/m2
  group_by(domainID,siteID,plotID,namedLocation,nrcsDescriptionID,decimalLatitude,decimalLongitude) %>% 
  #profile level sum layer SOC
  dplyr::summarise(  
    profile_top_cm=min(layer_top),
    profile_bottom_cm = max(layer_bottom),
    profile_depth = sum(layer_depth),
    `Calculate_soc_g_m-2` = sum(`Calculate_soc_g_m-2`),
    `Calculate_soc_kg_m-2`=sum(`Calculate_soc_g_m-2`)*0.001,
    n=n()
  ) %>% 
  filter(profile_depth==30) 

write_csv(spc.layer, 
          paste0(wd,latest_year,"_",unique(HARV_spc$domainID),"_",site,"_","INICHARspc_SOC.csv"))

# return(cat(paste0("long_df saved:\n",
#                   paste0(wd,latest_year,"_",unique(HARV_spc$domainID),"_",site,"_","INICHARspc_SOC.csv"))

