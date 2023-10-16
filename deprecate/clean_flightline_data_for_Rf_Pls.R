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

neon_hs_table <- read_csv("temporary_neon_hs_table.csv")
soc_df <- read_csv("NEON_SOC_Locations/spc_mgp_sls_30cm_locationSOC.csv")
soc_df$plotID=1:nrow(soc_df)

allsites=unique(soc_df$siteID)
length(allsites) #47 we have all NEON has 47 terrestrial sites
allsites

#clean Hs outlier
final_df <- left_join(neon_hs_table,soc_df,by="plotID") %>% 
  rename(OC.percent=`wtmean.estimatedOC.percent`) 

#check all hs looks like now:
final_df %>% #filter(reflectance<0.55) %>% 
  ggplot(aes(x=wavelength, group = plotID)) +
  geom_line(aes(y = reflectance, color = plotID), linewidth = 0.3)+
  ylim(0,1)+
  # scale_x_continuous(breaks = seq(350, 600, by = 10), limits=c(350, 600))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

#summary the wavelength, many wavelenght are not overlap
#we need the wavelengths that all sites have.

#if there is NA in this wavelength, all reflectance of this wavelength set NA
final_df_withNA <- final_df %>% 
  group_by(wavelength) %>% 
  mutate(reflectance_withNA = ifelse(!any(is.na(reflectance)), reflectance, NA)) %>% 
  ungroup() %>% 
  mutate(reflectance = ifelse((is.na(reflectance_withNA)), NA,reflectance))

final_df_withNA  %>% 
  filter(!is.na(reflectance_withNA)) %>%
  #filter(!is.na(reflectance_withNA)) %>%
  pull(wavelength) %>% 
  unique() %>% length() #359 wavelenght has no NA value.

#plotID unique is 1818, each plot has 420 bands
temp <- final_df %>%
  group_by(plotID) %>% 
  summarise(nbands=n()) 
#temp %>% View()

#some wavelength reflectance >1 or <0 set as NA,so not all wavelength can be used.
#check each wavelength how many sites have this wavelength
temp2 <- final_df %>%
   filter(!is.na(reflectance)) %>%
  group_by(wavelength) %>% 
  summarise(numbofsites=n()) %>% #
  arrange(desc(numbofsites))
View(temp2)
dim(temp2) #368 wavelength after remove watervapor wavelength

temp3 <- temp2 %>% group_by(numbofsites) %>% 
  summarise(numof_wavelength=n())  %>% 
  arrange(desc(numbofsites))

temp3 %>% pull(numof_wavelength) %>% sum #368 different wavelength (420-watervapor wavelength)
temp3 %>% filter(numbofsites>200) %>% 
  pull(numof_wavelength) %>% sum #359 overlapped wavelenght without NA


final_df %>%
  filter(!is.na(reflectance)) %>%
  group_by(wavelength) %>%
  mutate(numbofsites=n()) %>%
  # filter(numbofsites>1000) %>%
  filter(numbofsites==210) %>% 
  dplyr::select(wavelength,reflectance,plotID) %>% 
  group_by(plotID) %>% 
  arrange(plotID,wavelength) %>%   
  ggplot(aes(x=wavelength, group = plotID)) +
  geom_point(aes(y = reflectance, color = as.factor(plotID)))+
  theme(legend.position = "none")

final_df_withNA  %>%
  dplyr::select(wavelength,reflectance,plotID) %>%
  group_by(plotID) %>% 
  arrange(plotID,wavelength) %>%   
  ggplot(aes(x=wavelength, group = plotID)) +
  geom_point(aes(y = reflectance, color = as.factor(plotID)))+
  theme(legend.position = "none")

final_df_withNA %>% ggplot(aes(x=wavelength, group = plotID)) +
  geom_point(aes(y = reflectance, color = OC.percent),size=0.1)+
  scale_x_continuous(breaks = seq(400, 2500, by = 50), limits=c(400, 2500))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  scale_color_gradientn(colours = rainbow(10))
# theme(legend.position = "none")

temp = final_df %>%
  filter(!is.na(reflectance)) %>%
  group_by(wavelength) %>% 
  mutate(numbofsites=n()) %>% 
  filter(numbofsites==210) 

temp %>% ggplot(aes(x=wavelength, group = plotID)) +
  geom_point(aes(y = reflectance, color = OC.percent),size=0.1)+
  scale_x_continuous(breaks = seq(400, 2500, by = 50), limits=c(400, 2500))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  scale_color_gradientn(colours = rainbow(10))
  # theme(legend.position = "none")

ggsave(file=paste0(user_wd,"/NEON-AOP/data/hs_lai/RFigures/domain10_cleaned_aligned_wavelength.png"),
       width=8,height=4,dpi=300)  

aligned_wavelength_df = final_df %>%
  filter(!is.na(reflectance)) %>%
  group_by(wavelength) %>% 
  mutate(numbofsites=n()) %>% 
  # filter(numbofsites>1000) %>% 
  filter(numbofsites==210) %>% 
  dplyr::select(wavelength,reflectance,plotID) %>% 
  group_by(plotID) %>% 
  arrange(plotID,wavelength) %>% 
  pivot_wider(names_from = wavelength, values_from = reflectance) %>% 
  ungroup() %>% 
  select_if(~ !any(is.na(.))) 


aligned_wavelength_df %>% pull(plotID) %>% unique %>% length() #1372 of 1818 sites. the rest lack value at these wavelength
#1372 rows mean 1372 plots/observation
#187 cols mean 187 wavelength/features/covariables.

#Test, plot out
if(doTest==TRUE){
test=aligned_wavelength_df %>%
  pivot_longer(!plotID, names_to = "wavelength", values_to = "reflectance") 

test2=final_df_withNA %>% select(plotID,wavelength,reflectance)%>% 
  filter(!is.na(reflectance)) #%>% 
  # mutate(wavelength=as.character(wavelength))

test2  %>%   
  ggplot(aes(x=wavelength, group = plotID)) +
  geom_line(aes(y = reflectance, color = as.factor(plotID)), linewidth = 0.3)+
  theme(legend.position = "none")

test  %>%   
  ggplot(aes(x=wavelength, group = plotID)) +
  geom_line(aes(y = reflectance, color = as.factor(plotID)), linewidth = 0.3)+
  theme(legend.position = "none")
}

getwd()
wd
write_csv(aligned_wavelength_df,
          "wavelength_covariable_input.csv")








