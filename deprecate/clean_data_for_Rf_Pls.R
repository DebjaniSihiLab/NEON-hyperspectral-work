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
  # mutate(band_order = parse_number(index)) 

#check all hs looks like now:
final_df %>% #filter(reflectance<0.55) %>% 
  ggplot(aes(x=wavelength, group = plotID)) +
  geom_line(aes(y = reflectance, color = plotID), linewidth = 0.3)+
  ylim(0,1)+
  # scale_x_continuous(breaks = seq(350, 600, by = 10), limits=c(350, 600))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

# #find outlier wavelength
# final_df %>% filter(reflectance>0.5&wavelength>2300) %>% 
#   pull(wavelength) %>% 
#   unique()
# 
# final_df %>% filter(reflectance>0.6&wavelength>1900&wavelength<2100) %>% 
#   pull(wavelength) %>% 
#   unique()
# 
# final_df %>% filter(wavelength>565&wavelength<580&reflectance>0.26) %>% 
#   pull(wavelength) %>% 
#   unique()
# 
# final_df %>% filter(wavelength>375&wavelength<395&reflectance>0.135) %>% 
#   pull(wavelength) %>% 
#   unique()
# 
# final_df %>% filter(wavelength>395&wavelength<400&reflectance>0.125) %>% 
#   pull(wavelength) %>% 
#   unique()

# final_df=final_df %>% 
#   mutate(reflectance = case_when(wavelength%in%c(2510,2511,2512,2487,
#                                                  2492,2497,2502,2507,2496,2506,2491) ~NA,
#                                  wavelength==2021 ~NA,
#                                  wavelength==2026 ~NA,
#                                  wavelength==574 ~NA,
#                                  wavelength%in%c(382, 387, 384,399) ~NA,
#                                  TRUE ~ reflectance))


#check all hs looks like now:
final_df %>%
  ggplot(aes(x=wavelength, group = plotID)) +
  geom_line(aes(y = reflectance, color = plotID), linewidth = 0.3)+
  ylim(0,1)+
  # scale_x_continuous(breaks = seq(350, 600, by = 10), limits=c(350, 600))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

#summary the wavelength, many wavelenght are not overlap
#we need the wavelengths that all sites have.
#plotID unique is 1818, each plot has 426 bands
final_df %>%
  group_by(plotID) %>% 
  summarise(nbands=n()) -> temp  
temp %>% View()

final_df %>%
  filter(!is.na(reflectance)) %>%
  group_by(wavelength) %>% 
  summarise(numbofsites=n()) %>% 
  arrange(desc(numbofsites)) -> temp2
temp2
dim(temp2) #956 different wavelength for different sites 426 bands

#wavelength 2470
final_df[final_df$wavelength==2470,]

temp2 %>% group_by(numbofsites) %>% 
  summarise(numof_wavelength=n())  %>% 
  arrange(desc(numbofsites))->temp3

temp3 %>% pull(numof_wavelength) %>% sum #956 different wavelength 

temp3 %>% filter(numbofsites>1000) %>% 
  pull(numof_wavelength) %>% sum 
#343 bnads/wavelength have more than 1000 of 1818 sites have values at these 343 of wavelength

# test = final_df %>%
#   filter(!is.na(reflectance)) %>%
#   group_by(wavelength) %>% 
#   mutate(numbofsites=n()) %>% 
#   filter(numbofsites==1372)%>% 
#   pull(plotID) %>% 
#   unique() 
# for (i in 2:27) {
#  ind <-  temp3[[i,1]]
#  temp <- final_df %>%
#    filter(!is.na(reflectance)) %>%
#    group_by(wavelength) %>% 
#    mutate(numbofsites=n()) %>% 
#    filter(numbofsites==ind) %>% 
#    # filter(as.integer(numbofsites)==int) %>% 
#    pull(plotID) %>% 
#    unique() 
#  
#  sumfalse=sum(!(temp %in% test))
#  cat("row ",i," ",sumfalse,"\n")
# }

#used line101-121 checking the 1372 sites overlaped at 186 wavelengths.
#the other less overlaped number of sites plotID (such as 1343,1332) all in the 1372 sites plotID.
#SO we use the 1372 sites
final_df %>%
  filter(!is.na(reflectance)) %>%
  group_by(wavelength) %>% 
  mutate(numbofsites=n()) %>% 
  # filter(numbofsites>1000) %>% 
  filter(numbofsites==1372) %>% 
  dplyr::select(wavelength,reflectance,plotID) %>% 
  group_by(plotID) %>% 
  arrange(plotID,wavelength) %>%   
  
  ggplot(aes(x=wavelength, group = plotID)) +
  geom_point(aes(y = reflectance, color = as.factor(plotID)))+
  theme(legend.position = "none")

temp = final_df %>%
  filter(!is.na(reflectance)) %>%
  group_by(wavelength) %>% 
  mutate(numbofsites=n()) %>% 
  # filter(numbofsites>1000) %>% 
  filter(numbofsites==1372) 

temp %>% ggplot(aes(x=wavelength, group = plotID)) +
  geom_point(aes(y = reflectance, color = OC.percent),size=0.1)+
  scale_x_continuous(breaks = seq(400, 2500, by = 50), limits=c(400, 2500))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  scale_color_gradientn(colours = rainbow(10))
  # theme(legend.position = "none")

ggsave(file=paste0(user_wd,"/NEON-AOP/data/hs_lai/RFigures/aligned_wavelength.png"),
       width=8,height=4,dpi=300)  

aligned_wavelength_df = final_df %>%
  filter(!is.na(reflectance)) %>%
  group_by(wavelength) %>% 
  mutate(numbofsites=n()) %>% 
  # filter(numbofsites>1000) %>% 
  filter(numbofsites==1372) %>% 
  select(wavelength,reflectance,plotID) %>% 
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
  pivot_longer(!plotID, names_to = "wavelength", values_to = "reflectance") %>% 
slice(1:1500)

test  %>%   
  ggplot(aes(x=wavelength, group = plotID)) +
  geom_line(aes(y = reflectance, color = as.factor(plotID)), linewidth = 0.3)+
  theme(legend.position = "none")
}

getwd()
wd
write_csv(aligned_wavelength_df,
          "wavelength_covariable_input.csv")








