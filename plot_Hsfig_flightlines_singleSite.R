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


list.files("DP1.30006.001/")
hs_df <- read_csv("DP1.30006.001/D10_CPER_2021_HS_buf_3m_long.csv")
# lai_df <- read_csv("DP3.30012.001/2019_D01_HARV_LAI_windows3m.csv")
soc_df <- read_csv("NEON_SOC_Locations/spc_mgp_sls_30cm_locationSOC.csv")
soc_df$plotID=1:nrow(soc_df)

final_df <- left_join(hs_df,soc_df,by="plotID") %>% 
  rename(OC.percent=`wtmean.estimatedOC.percent`)  
  
hist(final_df$OC.percent)
min(final_df$OC.percent)
max(final_df$OC.percent)

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
  xlab('Wavelength (nm)') + 
  ylab('Reflectance') 

fig
ggsave(file="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/hs_lai/RFigures/Hs_colorOC.png",
       fig,
       width=8,height=3,dpi=400)

#colored by LAI , 
fig <- final_df_mean %>%
  ggplot(aes(x=wavelength, group = OCrange)) + 
  geom_line(aes(y = mean.hs,color = mean.lai), size = 0.8)+
  scale_color_gradientn(colours = rainbow(5))+
  #scale_color_viridis_c()+
  # scale_fill_viridis_b()+
  xlab('Wavelength (nm)') + 
  ylab('Reflectance')

ggsave(file="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/hs_lai/RFigures/Hs_colorLAI.png",fig,
       width=8,height=3,dpi=400)


#OC vs LAI
final_df %>%
  ggplot(aes(OC.percent, LAI)) + 
  geom_point(size = .2, alpha = .5) 






