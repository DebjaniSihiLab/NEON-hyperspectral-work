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

library(caret)
library(pls)
library(tidyverse)

neon_lai_table <- read_csv("temporary_neon_lai_table.csv") %>% 
  group_by(plotID) %>% 
  summarise(lai=mean(LAI))

neon_hs_table <- read_csv("temporary_neon_hs_table.csv")
soc_df <- read_csv("NEON_SOC_Locations/spc_mgp_sls_30cm_locationSOC.csv")
soc_df$plotID=1:nrow(soc_df)

CPER_soc_df=soc_df %>% filter(siteID=="CPER") #77
STER_soc_df=soc_df %>% filter(siteID=="STER") #102
RMNP_soc_df=soc_df %>% filter(siteID=="RMNP") #31

png(file="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/hs_lai/RFigures/domain10_hist_OC.png",
    width=6, height=3,res=300, units="in")
hist(soc_df$wtmean.estimatedOC.percent,n=60,main="histogram of OC",xlab="OC%")
dev.off()

hist(CPER_soc_df$wtmean.estimatedOC.percent,n=50,main="CPER histogram of OC")
max(CPER_soc_df$wtmean.estimatedOC.percent)  
hist(STER_soc_df$wtmean.estimatedOC.percent,n=50,main="STER histogram of OC")
max(STER_soc_df$wtmean.estimatedOC.percent)  
hist(RMNP_soc_df$wtmean.estimatedOC.percent,n=50,main="RMNP histogram of OC")
max(RMNP_soc_df$wtmean.estimatedOC.percent)  

site=c("CPER","STER","RMNP")
soc_df=soc_df %>% filter(siteID %in% site)  %>% 
  dplyr::select(siteID,plotID,`wtmean.estimatedOC.percent`)

#add OC column to neon hs table
wide_data <- left_join(neon_hs_table,soc_df,by="plotID") %>% 
  rename(OC.percent=`wtmean.estimatedOC.percent`) %>% 
  left_join(neon_lai_table,by="plotID") %>%  
  relocate(OC.percent,.after = plotID) %>% 
  filter(!is.na(OC.percent)) %>% 
  group_by(wavelength) %>% 
  mutate(reflectance_withNA = ifelse(!any(is.na(reflectance)), reflectance, NA)) %>% 
  ungroup() %>% 
  mutate(reflectance = ifelse((is.na(reflectance_withNA)), NA,reflectance))

wide_data %>%
  ggplot(aes(x=`OC.percent`, group = plotID)) +
  geom_point(aes(y = lai, color = `OC.percent`))+
  scale_color_gradientn(colours = rainbow(3),name = "OC.percent")+ 
  facet_grid( factor(siteID, levels=c('CPER', 'STER', 'RMNP'))~.)
ggsave(file="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/hs_lai/RFigures/domain10_sites_lai_OC.png",
       width=7,height=4,dpi=400)

wide_data %>%
  #filter(!is.na(reflectance)) %>% 
  ggplot(aes(x=wavelength, group = plotID)) +
  geom_line(aes(y = reflectance), linewidth = 0.3)+
  theme(legend.position = "none") + 
  facet_grid( factor(siteID, levels=c('CPER', 'STER', 'RMNP'))~.)

  
wide_data %>%
  ggplot(aes(x=wavelength, group = plotID)) +
  geom_line(aes(y = reflectance, color = lai), linewidth = 0.3)+
  scale_color_gradientn(colours = rainbow(3),name = "lai")+ 
  facet_grid( factor(siteID, levels=c('CPER', 'STER', 'RMNP'))~.)
ggsave(file="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/hs_lai/RFigures/domain10_sites_HS_lai.png",
       width=6,height=4,dpi=400)

wide_data %>%
  ggplot(aes(x=wavelength, group = plotID)) +
  geom_line(aes(y = reflectance, color = `OC.percent`), linewidth = 0.3)+
  scale_color_gradientn(colours = rainbow(3),name = "OC.percent")+ 
  facet_grid( factor(siteID, levels=c('CPER', 'STER', 'RMNP'))~.)
ggsave(file="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/hs_lai/RFigures/domain10_sites_HS_OC.png",
       width=6,height=4,dpi=400)
  
  
  
  
  
  
