file_dir <- "/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/SoDaH/edi.521.1/521_soils_data_harmonization_6e8416fa0c9a2c2872f21ba208e6a919.csv"

library(tidyverse)

rdin_data <- read_csv(file_dir) #293592    157

data_ctrl <- rdin_data %>% filter(control_sample=="TRUE") #54328   157
rm(rdin_data)
#The SoDaH dataset focuses on soil organic carbon (SOC) concentration (% C), estimated SOC stocks (g C m−2), 
#lyr_soc
# Bulk Layer Organic Carbon (CN analyzer) concentration, inorganic C removed or not present
# float
# dimensionless
# real
# 0-69.19

data_soc <- data_ctrl %>% filter(!is.na(lyr_soc)) %>%   #11839   157
  group_by(lat,long,network,site_code,map,mat,layer_top,layer_bot,
           data_file,curator_PersonName,curator_email) %>% 
  summarise(m_soc= mean(lyr_soc,na.rm = TRUE),
            m_ctot=mean(lyr_c_tot,na.rm = TRUE)) %>% #4213   13 before remove NEON data
  filter(network!="NEON")

dim(data_soc)  #943  13 #after remove NEON data.

#only 0-30cm layer, align with our study

data_soc_30cm <- data_soc %>% 
  filter(layer_top < 30) %>% #   # 679  13
  filter((is.finite(layer_bot))) %>% 
  group_by(lat,long,network,site_code,map,mat,
           data_file,curator_PersonName,curator_email) %>% # pedon/profile level----
filter(( 
  ((layer_top %in% layer_bot) | (layer_top == 0))&
    ((layer_bot %in% layer_top) | (max(layer_bot)==layer_bot)) 
          )) %>%   #502  13
  group_by(lat,long,map,mat,network,site_code,
           data_file,curator_PersonName,curator_email) %>%
  mutate(n=n(),
         max_bot=max(layer_bot))

data_soc_30cm  %>% group_by(lat,long,map) %>% filter(row_number()==1) %>% dim() #290 4
df=data_soc_30cm %>% group_by(lat,long,map) %>% filter(row_number()==1)  %>% ungroup() %>% 
  mutate(names=paste(site_code, map,lat, sep="_")) 

ggplot(data=df,aes(x=1:290, y=max_bot)) +
  geom_bar(stat = "identity")+
  scale_y_continuous(breaks = seq(0, 80, by = 5))+
  scale_x_continuous(breaks = seq(1, 290, by = 5))+
  geom_hline(yintercept=c(5,10,30), linetype="dashed",color = "red")+
  theme(axis.text.x=element_text(angle=90))

maxbot_summary <- data_soc_30cm %>% group_by(max_bot,site_code,map,lat,long) %>% summarise(n=n()) %>% 
  group_by(max_bot) %>% summarise(n=n())

#bottom depth > 30 cm, which means the profile include 0-30cm
test=data_soc_30cm%>% filter(max_bot>=30)%>% group_by(lat,long,map) %>% filter(row_number()==1) %>% 
  mutate(names=paste(site_code, map,lat, sep="_")) #%>% dim() #21 15
write_csv(test,"/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/SoDaH/maxbot_30cm_21sites.csv")

test=data_soc_30cm%>% filter(max_bot>=30)%>% group_by(lat,long,map) %>% 
  mutate(names=paste(site_code, map,lat, sep="_")) %>% #%>% dim() #21 15
  group_by(lat,long,network,site_code,map,mat,layer_top,layer_bot,curator_PersonName,
           curator_email,max_bot) %>% 
  summarise(soc=mean(m_soc))
write_csv(test,"/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/SoDaH/maxbot_30cm_21sites_profiles.csv")


data_soc_30cm %>% filter(n>3) %>% group_by(lat,long,map) %>% filter(row_number()==1) %>% dim() #12 15
rep_profile30=data_soc_30cm %>% filter(n>3)

#mannually clean maxbot_30cm_21sites_profiles.csv
cleaned_data_soc_30cm_layers <- read_csv("/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/SoDaH/maxbot_30cm_21sites_profiles_manualcleaned.csv")
cleaned_data_soc_30cm_layers %>% distinct(lat,long) %>% dim()
#calculate 30cm soc pedon/profile level----
soc_30cm <- cleaned_data_soc_30cm_layers %>% #filter(`ISCN_SOC_g_cm-2`>0)%>% #dim()
  filter(layer_top < 30) %>% #if layer_top is NA, NA will not include by filter
  filter((is.finite(layer_bot))) %>% 
  mutate(layer_bot = if_else(layer_bot > 30, 30,layer_bot))%>% 
  filter((layer_top < layer_bot)) %>% 
  group_by(lat,long,network,site_code,map,mat,curator_PersonName,
           curator_email,max_bot) %>% # pedon/profile level----
filter(all(
  ((layer_top %in% layer_bot) | (layer_top == 0))&
    ((layer_bot %in% layer_top) | (max(layer_bot)==layer_bot))
) 
)%>%
  mutate(layer_depth = (layer_bot-layer_top))%>% 
  mutate(OC_weighted = soc*layer_depth/30)%>% 
  group_by(lat,long,network,site_code, map,mat,curator_PersonName,
           curator_email,max_bot)%>% #profile level sum layer SOC
  summarise(  
    profile_top_cm=min(layer_top),
    profile_bottom_cm = max(layer_bot),
    profile_depth = sum(layer_depth),
    OC_30cm = sum(OC_weighted),
    n=n()
  ) 

soc_30cm %>% distinct(lat,long) %>% dim() #20 sites

ggplot(data=soc_30cm,aes(x=1:20, y=OC_30cm)) +
  geom_bar(stat = "identity")+
  # scale_y_continuous(breaks = seq(0, 80, by = 5))+
  scale_x_continuous(breaks = seq(1, 20, by = 1))+
  # geom_hline(yintercept=c(5,10,30), linetype="dashed",color = "red")+
  theme(axis.text.x=element_text(angle=90))

