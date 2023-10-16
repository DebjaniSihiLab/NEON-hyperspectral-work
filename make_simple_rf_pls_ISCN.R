rm(list=ls())

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

library(randomForest)
library(caret)
library(pls)
library(tidyverse)
library(doParallel)


ISCN_lai_table <- read_csv("temporary_ISCN_lai_table.csv") %>% 
  group_by(plotID) %>% 
  summarise(lai=mean(LAI,na.rm=TRUE))

ISCN_CHM_table <- read_csv("temporary_ISCN_CHM_table.csv") %>% 
  group_by(plotID) %>% 
  summarise(CanopyHeight=mean(CHM,na.rm=TRUE))

ISCN_hs_table <- read_csv("ISCN_wavelength_covariable_input.csv")

soc_30cm_df <- read_csv("/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/iscn/ISCN_30cm_USA_UTM.csv") 
#some sites have different date!
soc_30cm= soc_30cm_df %>% group_by(site_name,profile_name) %>% 
  summarise(OC=mean(OC_30cm),
            lat=first(lat),
            long=first(long))

landcover_df=read_csv("/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/iscn/ISCN_nlcdClass.csv") %>% 
  group_by(plotID) %>% 
  summarise(nlcdClass=first(nlcdClass))

soc_df <- left_join(ISCN_hs_table,soc_30cm,by=c("plotID"="profile_name") ) %>% 
  left_join(ISCN_lai_table,by="plotID") %>% 
  left_join(ISCN_CHM_table,by="plotID") %>% 
  left_join(landcover_df,by="plotID")%>% 
  dplyr::select(-site_name,-lat,-long)

spdf <- SpatialPointsDataFrame(coords = data.frame(x = soc_df$long,
                                                   y = soc_df$lat), 
                               data = data.frame(plotID = soc_df$plotID),
                               proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
plot(spdf)

crs_target = "+proj=longlat +datum=WGS84 +no_defs" 
pts <- SpatialPointsDataFrame(coords = data.frame(x = soc_df$long,
                                                  y = soc_df$lat), 
                              data = data.frame(plotID = soc_df$plotID),
                              proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

plot(pts)

usa <- map_data('usa')
map=ggplot() + 
  geom_polygon(data=usa, aes(x=long, y=lat, group=group),fill='lightblue') + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  coord_fixed(1.3)+
  geom_point(data=coords,aes(x=coords$x, y=coords$y))

wide_data <- soc_df%>% 
  rename(OC.percent=OC) %>% 
  relocate(OC.percent,.after = plotID) %>% 
  relocate(lai,CanopyHeight,nlcdClass,.after = OC.percent)
  #mutate(OC.percent=log10(OC.percent))
  

col_name <- paste0("wavelength_",colnames(wide_data)[6:356])
length(col_name) #351 wavelengths/bands
colnames(wide_data)[6:356] <- col_name
wide_data_original_ISCN= wide_data
wide_data_original= wide_data #-------------save wide_data
#====================================================================
wide_data=wide_data_original
#==================================================================
colnames(wide_data)
wide_data=wide_data %>% 
  dplyr::select(-nlcdClass) %>% 
  drop_na()
dim(wide_data)#146
hist(wide_data$OC.percent)
testing_set=wide_data

file_name <- "explore_models/ISCN_RFtest1_model.RData"#% Var explained: 
rf_model_loaded <- readRDS(file_name)
rf_model_loaded
rf_model_loaded$finalModel

forest <- rf_model_loaded
#===========================================
#Let’s see how the model performs on testing data.
## Generate predictions
predicted_y <- predict(
  ## Random forest object
  object=forest, 
  ## Data to use for predictions; remove the Species
  newdata=testing_set[, -c(1,2)])

y <- testing_set[,2] %>% unlist()
1 - sum((y-predicted_y)^2)/sum((y-mean(y))^2)
print("Use of defaultSummary")
defaultSummary(
  data = data.frame(obs = y, 
                    pred = predicted_y))

print("Use of RMSE")
RMSE(predicted_y, y)

# Build scatterplot
ggplot() + 
  geom_point( data= testing_set ,aes(x = testing_set$plotID, y = `OC.percent`, color = 'red', alpha = 0.5) ) + 
  geom_point( aes(x =testing_set$plotID , y = predicted_y, color = 'blue',  alpha = 0.5)) + 
  labs(x = "plotID", y = "OC.percent", color = "", alpha = 'Transperency') +
  scale_color_manual(labels = c( "Predicted", "Real"), values = c("blue", "red")) 

print(paste0('R2: ' ,caret::postResample(predicted_y , y)['Rsquared'] ))
R2_default <- caret::R2(pred = predicted_y, obs = y)
R2_tradition   <- caret::R2(pred = predicted_y, obs = y, form = "traditional")
caret::postResample(predicted_y , y)

library(ggpubr)
ggplot(data = data.frame(obs = y, 
                         pred = predicted_y),
       aes(x=obs,y=pred))+
  geom_point()+
  geom_smooth(aes(x=obs,y=pred),method = "lm")+
  xlab("observation")+ylab("prediction")+
  stat_regline_equation(label.y = 28, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 25, aes(label = ..rr.label..))+
  xlim(0,50)+ylim(0,50) +
  geom_abline(intercept = 0,slope = 1)
ggsave(file=paste0(user_wd,"/NEON-AOP/data/hs_lai/RFigures/RF_pred_ISCN_OC.png"),
       width=5,height=5,dpi=400)  


###################################################################################################
## make pls model(partial least squares regression)----####
###########################################################################################################
# 2.. Load model under another name
model_loaded <- readRDS(file_name)
model_loaded
model_loaded$finalModel

model <- model_loaded
#=====================================================================================
# Make predictions
predictions <- model %>% predict(testing_set[, -c(1,2)])
# Model performance metrics
data.frame(
  RMSE = caret::RMSE(predictions, testing_set$OC.percent),
  Rsquare = caret::R2(predictions, testing_set$OC.percent)
)

print("Use of defaultSummary")
defaultSummary(
  data = data.frame(obs = testing_set$OC.percent, 
                    pred = predictions))

library(ggpubr)
ggplot( data = data.frame(obs = testing_set$OC.percent, 
                          pred = predictions),
       aes(x=obs,y=pred))+
  geom_point()+
  geom_smooth(aes(x=obs,y=pred),method = "lm")+
  xlab("observation")+ylab("prediction")+
  stat_regline_equation(label.y = 31, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 28, aes(label = ..rr.label..))+
  xlim(0,50)+ylim(0,50) +
  geom_abline(intercept = 0,slope = 1)
ggsave(file=paste0(user_wd,"/NEON-AOP/data/hs_lai/RFigures/GLS_obs_pred_ALL_20percentOC.png"),
       width=5,height=5,dpi=400)  

ggplot( data = data.frame(obs = testing_set$OC.percent, 
                          pred = predictions),
       aes(x=obs,y=pred))+
  geom_point()+
  geom_smooth(aes(x=obs,y=pred),method = "lm")+
  xlab("observation")+ylab("prediction")+
  stat_regline_equation(label.y = 2.5,label.x = 6, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 1.5, label.x = 6,aes(label = ..rr.label..))+
  xlim(0,22)+ylim(0,22) +                         
  geom_abline(intercept = 0,slope = 1)


ggsave(file=paste0(user_wd,"/NEON-AOP/data/hs_lai/RFigures/PLS_obs_pred_20percentOC.png"),
       width=5,height=5,dpi=400)  

# Build scatterplot
ggplot() + 
  geom_point(data= testing_set , aes(x = testing_set$plotID, y = `OC.percent`, color = 'red', alpha = 0.5) ) + 
  geom_point( aes(x =testing_set$plotID , y = predictions, color = 'blue',  alpha = 0.5)) + 
  # geom_line(data= testing_set , aes(x = testing_set$plotID, y = `OC.percent`, color = 'red', alpha = 0.5) ) + 
  # geom_line( aes(x =testing_set$plotID , y = predictions, color = 'blue',  alpha = 0.5)) + 
  labs(x = "plotID", y = "OC.percent", color = "", alpha = 'Transperency') +
  scale_color_manual(labels = c( "Predicted", "Real"), values = c("blue", "red")) 
