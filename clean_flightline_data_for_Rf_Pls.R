#setwd to home, then check which pc.
setwd("~")
whichpc=getwd()
cat(whichpc)
if(whichpc=="/Users/zwang61"){
  user_wd="/Users/zwang61/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
}else{
  user_wd="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
}

cat(user_wd)
wd = paste0(user_wd,"/NEON-AOP/data/hs_lai/")
setwd(wd)
getwd()

library(tidyverse)
neon_lai_table <- read_csv("temporary_neon_lai_table.csv") %>% 
  group_by(plotID) %>% 
  summarise(lai=mean(LAI,na.rm=TRUE))

neon_CHM_table <- read_csv("temporary_neon_CHM_table.csv") %>% 
  group_by(plotID) %>% 
  summarise(CanopyHeight=mean(CHM,na.rm=TRUE))

neon_hs_table <- read_csv("temporary_neon_hs_table.csv")
soc_df <- read_csv("NEON_SOC_Locations/spc_mgp_sls_30cm_locationSOC_withPFT_Soiltype.csv")
soc_df$plotID=1:nrow(soc_df)

allsites=unique(soc_df$siteID)
length(allsites) #47 we have all NEON has 47 terrestrial sites
allsites

#clean Hs outlier
final_df <- left_join(neon_hs_table,soc_df,by="plotID") %>% 
  left_join(neon_lai_table,by="plotID") %>% 
  left_join(neon_CHM_table,by="plotID") %>% 
  rename(OC.percent=`wtmean.estimatedOC.percent`) 

#check all hs looks like now:
final_df %>% #filter(reflectance<0.55) %>% 
  ggplot(aes(x=wavelength, group = plotID)) +
  # geom_line(aes(y = reflectance, color = lai), linewidth = 0.3)+
  geom_line(aes(y = reflectance, color = plotID), linewidth = 0.3)+
  ylim(0,1)+
  # scale_x_continuous(breaks = seq(350, 600, by = 10), limits=c(350, 600))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

final_df  %>% 
  filter(!is.na(reflectance)) %>%
  #filter(!is.na(reflectance_withNA)) %>%
  pull(wavelength) %>% 
  unique() %>% length() #368 of 420 wavelenght total

#summary the wavelength, many wavelenght are not overlap
#we need the wavelengths that all sites have.

#if there is NA in this wavelength, all reflectance of this wavelength set NA
final_df <- final_df %>% 
  group_by(wavelength) %>% 
  mutate(reflectance_withNA = ifelse(!any(is.na(reflectance)), reflectance, NA)) %>% 
  ungroup() %>% 
  mutate(reflectance = ifelse((is.na(reflectance_withNA)), NA,reflectance))

final_df  %>% 
  filter(!is.na(reflectance_withNA)) %>%
  #filter(!is.na(reflectance_withNA)) %>%
  pull(wavelength) %>% 
  unique() %>% length() #359 wavelenght has no NA value.


final_df  %>%
  dplyr::select(wavelength,reflectance,plotID) %>%
  group_by(plotID) %>% 
  arrange(plotID,wavelength) %>%   
  ggplot(aes(x=wavelength, group = plotID)) +
  geom_point(aes(y = reflectance, color = as.factor(plotID)))+
  theme(legend.position = "none")

final_df %>% ggplot(aes(x=wavelength, group = plotID)) +
  geom_point(aes(y = reflectance, color = OC.percent),size=0.1)+
  scale_x_continuous(breaks = seq(400, 2500, by = 50), limits=c(400, 2500))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  scale_color_gradientn(colours = rainbow(10))
# theme(legend.position = "none")

ggsave(file=paste0(user_wd,"/NEON-AOP/data/hs_lai/RFigures/cleaned_aligned_wavelength.png"),
       width=8,height=4,dpi=300)  

aligned_wavelength_df = final_df %>%
   filter(!is.na(reflectance)) %>%
   # group_by(wavelength) %>% 
  dplyr::select(wavelength,reflectance,plotID) %>% 
  group_by(plotID) %>% 
  arrange(plotID,wavelength) %>% 
  pivot_wider(names_from = wavelength, values_from = reflectance) %>% 
  ungroup() %>% 
  select_if(~ !any(is.na(.))) 
  


aligned_wavelength_df %>% pull(plotID) %>% unique %>% length() 

#Test, plot out
doTest=FALSE
if(doTest==TRUE){

final_df %>%
  # filter(!is.na(reflectance)) %>% 
  ggplot(aes(x=wavelength, group = plotID)) +
  geom_line(aes(y = reflectance), linewidth = 0.3)+
  theme(legend.position = "none")
  
  final_df %>%
    # filter(!is.na(reflectance)) %>% 
    ggplot(aes(x=wavelength, group = plotID)) +
    geom_line(aes(y = reflectance, color = CanopyHeight), linewidth = 0.3)+
    scale_color_gradientn(colours = rainbow(3),name = "CanopyHeight")
  
  final_df %>%
    # filter(!is.na(reflectance)) %>% 
    ggplot(aes(x=wavelength, group = plotID)) +
    geom_line(aes(y = reflectance, color = nlcdClass), linewidth = 0.3)+
    scale_colour_brewer(palette = "Set3")
  
  final_df %>%
    # filter(!is.na(reflectance)) %>% 
    ggplot(aes(x=wavelength, group = plotID)) +
    geom_line(aes(y = reflectance, color = soilTypeOrder), linewidth = 0.3)+
    scale_colour_brewer(palette = "Set3")
  
  
  final_df %>%
    # filter(!is.na(reflectance)) %>% 
    ggplot(aes(x=wavelength, group = plotID)) +
    geom_line(aes(y = reflectance, color = lai), linewidth = 0.3)+
    scale_color_gradientn(colours = rainbow(3),name = "lai")
  
  final_df %>%
    # filter(!is.na(reflectance)) %>% 
    ggplot(aes(x=wavelength, group = plotID)) +
    geom_line(aes(y = reflectance, color = `OC.percent`), linewidth = 0.3)+
    scale_color_gradientn(colours = rainbow(3),name = "OC.percent")
  
  
test=aligned_wavelength_df %>%
    pivot_longer(!plotID, names_to = "wavelength", values_to = "reflectance") 
test  %>%   
  ggplot(aes(x=wavelength, group = plotID)) +
  geom_line(aes(y = reflectance, color = as.factor(plotID)), linewidth = 0.3)+
  theme(legend.position = "none")

}

getwd()
wd
write_csv(aligned_wavelength_df,
          "wavelength_covariable_input.csv")








