#ISCN sites observation
rm(list = ls())
library(SOCDRaH2)
library(tidyverse)
library(sf)

YourDataDir <- file.path("/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/iscn")#ISCN data saved here
iscn.ls <- ISCN3_3(data_dir = YourDataDir) 
names(iscn.ls$layer)
#create a variable which
layer.df <- iscn.ls$layer %>% #slice_head(n=2000) %>% 
  select(contains("name"),
         date = "observation_date (YYYY-MM-DD)",
         lat = "lat (dec. deg)",
         long = "long (dec. deg)",
         layer_top="layer_top (cm)",
         layer_bottom="layer_bot (cm)",
         c_tot="c_tot (percent)",
         oc="oc (percent)"
  )

names(layer.df)
dim(layer.df) 

layer_temp <- layer.df %>%
  mutate(oc_comb = case_when(oc>c_tot ~ c_tot,
                             oc<=c_tot ~ oc,
                             is.finite(oc) ~ oc,
                             is.finite(c_tot)~ c_tot,
                             #if no oc but have c_tot then we will use c-tot
                             TRUE ~ NA_real_
  )) %>%
  group_by(dataset_name_sub,site_name,profile_name) %>% 
  #record the profile deepest bottom
  filter(oc_comb>0 & oc_comb < 65 ) %>% 
  filter(is.finite(lat)&is.finite(long)) 

names(layer_temp)
layer_temp %>% dim()   #310640     24

#calculate 30cm soc pedon/profile level----
soc_30cm <- layer_temp %>% #filter(`ISCN_SOC_g_cm-2`>0)%>% #dim()
  filter(layer_top < 30) %>% #if layer_top is NA, NA will not include by filter
  filter((is.finite(layer_bottom))) %>% 
  mutate(layer_bottom = if_else(layer_bottom > 30, 30,layer_bottom))%>% 
  filter((layer_top < layer_bottom)) %>% 
  group_by(dataset_name_sub,site_name,profile_name,date) %>% # pedon/profile level----
  filter(all(
              ((layer_top %in% layer_bottom) | (layer_top == 0))&
              ((layer_bottom %in% layer_top) | (max(layer_bottom)==layer_bottom))
            ) 
        )%>%
  mutate(layer_depth = (layer_bottom-layer_top))%>% 
  mutate(OC_weighted = oc_comb*layer_depth/30)%>% 
  group_by(dataset_name_sub,site_name,profile_name,lat,long,layer_top,layer_bottom,date)%>% 
  #some layers are same but layer names different, so average same layers. 
  #Exp: profile_name == "41001", layer_names: 0041001.B4, 0041001.B5, 0041001.B6....
  summarise(
    layer_top=unique(layer_top),
    layer_bottom = unique(layer_bottom),
    layer_depth = unique(layer_depth),
    OC_weighted=mean(OC_weighted),
     n=n()
    )%>%
  group_by(dataset_name_sub,site_name,profile_name,date)%>% #profile level sum layer SOC
  summarise(  
    lat= first(lat),
    long= first(long),
    profile_top_cm=min(layer_top),
    profile_bottom_cm = max(layer_bottom),
    profile_depth = sum(layer_depth),
    OC_30cm = sum(OC_weighted),
    n=n()
  ) %>% 
  filter(profile_top_cm==0) %>% 
  filter(profile_bottom_cm>29)  %>% 
  # filter(profile_depth==30)  %>% 
  group_by(site_name,profile_name,date)%>%
#  different dataset_name_sub have same site_name,profile_name,
  summarise(
    dataset_name_sub=first(dataset_name_sub),
    lat= first(lat),
    long= first(long),
    profile_top_cm=min(profile_top_cm),
    profile_bottom_cm = max(profile_bottom_cm),
    OC_30cm=mean(OC_30cm,na.rm=TRUE),
    n=n()
  ) %>%
  filter(is.finite(OC_30cm)) %>% 
  group_by(lat,long)%>%  #same lat,long have repeat! remove!
  mutate(rep=n()) %>% 
  filter(rep==1) # 
  
dim(soc_30cm) #36446    11

ggplot(data = soc_30cm)+
  geom_point(aes(x=lat,y= OC_30cm))

ggplot(data = soc_30cm)+
  geom_histogram(aes(x=OC_30cm),bins = 100)
  

summary(soc_30cm)
  
#find the points in USA, only keep USA points.----------
# Convert 30cm pedon data frame to sf object-------
soc_30cm.point <- st_as_sf(x = soc_30cm, 
                           coords = c("long", "lat"),
                           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# simple plot
plot(soc_30cm.point["OC_30cm"])

us_shp <-  st_read("/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/us_shp/us.shp")
soc_30cm.point.pointUSA <- st_join(soc_30cm.point, us_shp, join = st_intersects) %>%
  mutate(lat = unlist(map(.$geometry,2)),
         long = unlist(map(.$geometry,1)))

soc_30cm_df <- soc_30cm.point.pointUSA %>% st_drop_geometry() %>% 
  filter(GEOID == "US") %>% 
  filter(lat<51&lat>23) %>% 
  filter(long<-65&long>-126)
  
# us_extent <- ext(-126, -65, 23, 51)
soc_30cm_df_sf <- st_as_sf(x = soc_30cm_df, 
                           coords = c("long", "lat"),
                           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
# simple plot
plot(soc_30cm_df_sf["OC_30cm"],pch = 19, cex=0.1,
     main="ISCN observation in US")
dim(soc_30cm_df)  #33881  rows <---------------------------
table(soc_30cm_df$dataset_name_sub)


ggplot(data = soc_30cm_df)+
  geom_point(aes(x=lat,y= OC_30cm))

names(soc_30cm_df)
ggplot(data = soc_30cm_df)+
  geom_histogram(aes(x= OC_30cm))+
  scale_x_log10()

ggplot(data = soc_30cm_df)+
  geom_histogram(aes(x= OC_30cm),binwidth = 1)
summary(soc_30cm_df)
#30cm END here! 

#save---ISCN_30cm_USA----
#ISCN final products
write_csv(soc_30cm_df, "/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/iscn/ISCN_30cm_USA.csv")
# 
soc_30cm_df <-  read_csv("/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/iscn/ISCN_30cm_USA.csv")
dim(soc_30cm_df) #33881    14
