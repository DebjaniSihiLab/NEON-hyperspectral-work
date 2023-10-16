library(tidyverse)
library(RColorBrewer)

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

neon_lai_table <- read_csv("temporary_neon_lai_table.csv") %>% 
  group_by(plotID) %>% 
  summarise(lai=mean(LAI))

neon_hs_table <- read_csv("temporary_neon_hs_table.csv")

soc_df <- read_csv("NEON_SOC_Locations/spc_mgp_sls_30cm_locationSOC.csv")
soc_df$plotID=1:nrow(soc_df)

final_df <- left_join(neon_hs_table,soc_df,by="plotID")  %>% 
  left_join(neon_lai_table,by="plotID") %>%  
  rename(OC.percent=`wtmean.estimatedOC.percent`)  
  
final_df <- final_df %>% 
  group_by(wavelength) %>% 
  mutate(reflectance_withNA = ifelse(!any(is.na(reflectance)), reflectance, NA)) %>% 
  ungroup() %>% 
  mutate(reflectance = ifelse((is.na(reflectance_withNA)), NA,reflectance))

hist(final_df$OC.percent)
min(final_df$OC.percent)
max(final_df$OC.percent)
ggplot()+
  geom_line(data=final_df,aes(x=wavelength, y=reflectance,group=plotID,
                                    color=`OC.percent`), linewidth = 0.3)+
  scale_color_gradientn(colours = rainbow(3))+
  xlab("Wavelength (nm)")+
  ylab("Reflectance")+
  labs(color = "OC (%)")+
  ggtitle("group by plotID")

ggsave(file=paste0(user_wd,"/NEON-AOP/data/hs_lai/RFigures/domain10_neon_hs_soc_plotID_groups.png"),
       width=8,height=3,dpi=400)  

ggplot()+
  geom_line(data=final_df,aes(x=wavelength, y=reflectance,group=plotID,
                              color=lai), linewidth = 0.3)+
  scale_color_gradientn(colours = rainbow(3))+
  xlab("Wavelength (nm)")+
  ylab("Reflectance")+
  labs(color = "lai")+
  ggtitle("group by plotID")

ggsave(file=paste0(user_wd,"/NEON-AOP/data/hs_lai/RFigures/domain10_neon_hs_lai_plotID_groups.png"),
       width=8,height=3,dpi=400)  

## define a new variable OC range on the data set
breaks <- c(0,5,10,15,20,40,80)
n=length(breaks)

display.brewer.pal(n-1, "Dark2")
coul <- brewer.pal(n-1, "Dark2")
barplot(rep(1, length(coul)), col = coul , main="Dark2") 
names(coul) <-  paste0(breaks[-n],"-",c(breaks[-c(1,n)],">"))

breaks.labels <- paste0(breaks[-n],"-",c(breaks[-c(1,n)],">"))

# define a new variable on the data set just as above
final_df$OCrange <- cut(final_df$OC.percent, 
                       breaks = breaks, 
                       include.lowest = TRUE, 
                       labels = breaks.labels)
brks_scale <- levels(final_df$OCrange)
labels_scale <- breaks.labels

final_df_mean <- final_df %>% group_by(OCrange,wavelength) %>% 
  mutate(mean.hs=mean(reflectance,na.rm = TRUE),
         sd.hs=sd(reflectance,na.rm = TRUE)
         # mean.lai=mean(LAI,na.rm = TRUE),
         # sd.lai=sd(LAI,na.rm = TRUE)
  ) 

#adding a shaded standard deviation to line plots on ggplot2 for multiple variables

#colored by OC
fig <- final_df_mean %>% 
  ggplot(aes(x=wavelength, group = OCrange)) + 
 
  geom_ribbon(aes(y = mean.hs, ymin = mean.hs - sd.hs, ymax = mean.hs + sd.hs, 
                  fill = OCrange), alpha = .3) +
  geom_line(aes(y = mean.hs, color = OCrange), size = 0.3)+#aes(group=plotID)
  scale_color_manual(values = (coul),
                    breaks = (brks_scale),
                    labels = (labels_scale),
                    guide = guide_legend(
                      direction = "vertical",
                      # keyheight = unit(8, units = "mm"),
                      # keywidth = unit(5, units = "mm"),
                      title = "OC%",
                      title.position = "bottom",
                      # label.hjust = 0,
                      # label.vjust = 0,
                      ncol = 1,
                      reverse = T,# also the guide needs to be reversed
                      label.position = "right"
                      )
    )+
  scale_fill_manual(values = (coul),
                     breaks = (brks_scale),
                     labels = (labels_scale),
                     guide = guide_legend(
                       direction = "vertical",
                       # keyheight = unit(8, units = "mm"),
                       # keywidth = unit(5, units = "mm"),
                       title = "OC%",
                       title.position = "bottom",
                       # label.hjust = 0,
                       # label.vjust = 0,
                       ncol = 1,
                       reverse = T,# also the guide needs to be reversed
                       label.position = "right"
                     )
  )+
  ylim(0, 0.8)+
  xlab('Wavelength (nm)') + 
  ylab('Reflectance') +
  ggtitle("group by OC groups")

fig
ggsave(file="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/hs_lai/RFigures/domain10_Hs_color_OC_interval_groups.png",
       fig,
       width=8,height=3,dpi=400)

# #colored by LAI , 
fig <- final_df_mean %>%
  ggplot(aes(x=wavelength, group = OCrange)) +
   geom_line(aes(y = mean.hs,color = lai), size = 0.8)+
   geom_point(aes(y = mean.hs,shape =OCrange), size = 0.4)+
  scale_color_gradientn(colours = rainbow(5))+
  # scale_color_viridis_d()+
  # scale_fill_viridis_b()+
  xlab('Wavelength (nm)') +
  ylab('Reflectance')
fig
ggsave(file="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/hs_lai/RFigures/domain_Hs_colorLAI.png",fig,
       width=8,height=3,dpi=400)


#OC vs LAI
final_df %>%
  ggplot(aes(OC.percent, lai)) +
  geom_point(size = .2, alpha = .5)






