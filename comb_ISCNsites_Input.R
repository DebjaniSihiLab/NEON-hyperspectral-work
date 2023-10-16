library(tidyverse)
library(data.table)
library(foreach)
library(doParallel)

#Set Working Directory----
#setwd to home, then check which pc.
setwd("~")
whichpc=getwd()
cat(whichpc)
if(whichpc=="/Users/zwang61"){
  user_wd="/Users/zwang61/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
}else{
  user_wd="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
}
wd = paste0(user_wd,"/NEON-AOP/data/hs_lai/")
setwd(wd)
getwd()

ISCN_lai_table <- read_csv("temporary_ISCN_lai_table.csv") %>% 
  group_by(plotID) %>% 
  summarise(lai=mean(LAI,na.rm=TRUE))

ISCN_CHM_table <- read_csv("temporary_ISCN_CHM_table.csv") %>% 
  group_by(plotID) %>% 
  summarise(CanopyHeight=mean(CHM,na.rm=TRUE))

ISCN_hs_table <- read_csv("temporary_ISCN_hs_table.csv")%>% 
  group_by(plotID,wavelength) %>% 
  summarise(reflectance=mean(reflectance,na.rm=TRUE))

ISCN_hs_table$plotID %>% unique() %>% length()

soc_30cm_df <- read_csv("/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/iscn/ISCN_30cm_USA_UTM.csv") 
#some sites have different date!
soc_30cm= soc_30cm_df %>% group_by(site_name,profile_name) %>% 
  summarise(OC=mean(OC_30cm),
            lat=first(lat),
            long=first(long))

landcover_df=read_csv("/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/iscn/ISCN_nlcdClass.csv") %>% 
  group_by(plotID) %>% 
  summarise(nlcdClass=first(nlcdClass))


#clean Hs outlier
final_df <- left_join(ISCN_hs_table,soc_30cm,by=c("plotID"="profile_name") ) %>% 
  left_join(ISCN_lai_table,by="plotID") %>% 
  left_join(ISCN_CHM_table,by="plotID") %>% 
  left_join(landcover_df,by="plotID") 
  
#if there is NA in this wavelength, all reflectance of this wavelength set NA
final_df <- final_df %>% 
  group_by(wavelength) %>% 
  mutate(reflectance_withNA = ifelse(!any(is.na(reflectance)), reflectance, NA)) %>% 
  ungroup() %>% 
  mutate(reflectance = ifelse((is.na(reflectance_withNA)), NA,reflectance))

final_df %>% ggplot(aes(x=wavelength, group = plotID)) +
  geom_point(aes(y = reflectance, color = OC),size=0.1)+
  scale_x_continuous(breaks = seq(400, 2500, by = 50), limits=c(400, 2500))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  scale_color_gradientn(colours = rainbow(10))
# theme(legend.position = "none")

aligned_wavelength_df = final_df %>%
  filter(!is.na(reflectance)) %>%
  # group_by(wavelength) %>% 
  dplyr::select(wavelength,reflectance,plotID) %>% 
  group_by(plotID) %>% 
  arrange(plotID,wavelength) %>% 
  pivot_wider(names_from = wavelength, values_from = reflectance) %>% 
  ungroup() %>% 
  select_if(~ !any(is.na(.))) 

write_csv(aligned_wavelength_df,
          "ISCN_wavelength_covariable_input.csv")



