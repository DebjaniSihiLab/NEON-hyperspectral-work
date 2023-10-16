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

neon_lai_table <- read_csv("temporary_neon_lai_table.csv") %>% 
  group_by(plotID) %>% 
  summarise(lai=mean(LAI,na.rm=TRUE))

neon_CHM_table <- read_csv("temporary_neon_CHM_table.csv") %>% 
  group_by(plotID) %>% 
  summarise(CanopyHeight=mean(CHM,na.rm=TRUE))

neon_hs_table <- read_csv("wavelength_covariable_input.csv")

soc_df <- read_csv("NEON_SOC_Locations/spc_mgp_sls_30cm_locationSOC_withPFT_Soiltype.csv")
soc_df$plotID=1:nrow(soc_df)
soc_df <- soc_df %>% dplyr::select(plotID,`wtmean.estimatedOC.percent`,
                                   nlcdClass)#,elevation,slopeAspect,slopeGradient,soilTypeOrder)

#add OC column to neon hs table
wide_data <- left_join(soc_df,neon_hs_table,by="plotID")%>% 
  rename(OC.percent=`wtmean.estimatedOC.percent`) %>% 
  left_join(neon_lai_table,by="plotID") %>%  
  left_join(neon_CHM_table,by="plotID") %>%  
  relocate(OC.percent,.after = plotID) %>% 
  relocate(lai,CanopyHeight,.after = OC.percent)
  #mutate(OC.percent=log10(OC.percent))
  

col_name <- paste0("wavelength_",colnames(wide_data)[6:364])
length(col_name) #359 wavelengths/bands
colnames(wide_data)[6:364] <- col_name

wide_data_original_ISCN #get from make_simple_rf_pls_ISCN.R
colnames(wide_data_original_ISCN) 
dim(wide_data_original_ISCN)
wide_data=wide_data[,colnames(wide_data) %in% colnames(wide_data_original_ISCN) ]

wide_data_original= wide_data #-------------save wide_data
#====================================================================
# wide_data=wide_data_original
# lapply(wide_data,class) %>% as.data.frame()
# 
# #1 only use wavelength reflections
# wide_data=wide_data[,c(1,2,10:368)]
# 
# #2 include nlcdclass
# wide_data=wide_data %>% select(-soilTypeOrder) %>% drop_na()
# 
# #3 only use numerical predictors
# wide_data=wide_data %>% select(-soilTypeOrder,-nlcdClass)
# 
# lapply(wide_data,class) %>% as.data.frame()
#==================================================================
#----------------------------------------------------------------
wide_data=wide_data_original
wide_data=wide_data  %>% dplyr::select(-nlcdClass) %>% 
  drop_na()


dim(wide_data)
## Set seed for reproducibility
set.seed(123)

## Split the data so that we use 70% of it for training
train_index <- createDataPartition(y=wide_data$OC.percent, p=0.8, list=FALSE)

## Subset the data
training_set <- wide_data[train_index, ]#1,100
testing_set <- wide_data[-train_index, ]#272

## Set seed for reproducibility
set.seed(123)
## Define repeated cross validation with 5 folds and three repeats
repeat_cv <- trainControl(method='repeatedcv', number=3, repeats=1)
#=================================================================================================
#foreach to parallel read 6357 spectra files
parallel::detectCores() #10
n.cores <- parallel::detectCores() - 1
#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK"
)
#check cluster definition (optional)
print(my.cluster)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
foreach::getDoParRegistered()
#how many workers are available? (optional)
foreach::getDoParWorkers()
start <- Sys.time()
## Train a random forest model
forest <- train(
  # Formula. We are using all variables to predict Species
  OC.percent~., 
  # Source of data; remove the Species variable
  data=training_set[,-1], 
  # `rf` method for random forest
  method='rf')
  # Add repeated cross validation as trControl
  # trControl=repeat_cv)

print( Sys.time() - start ) #~10min
parallel::stopCluster(cl = my.cluster)
## Print out the details about the model
forest$finalModel #--random forest final model------

#save randomforest model--------------------
# 1. Save model
file_name <- "explore_models/ISCN_RFtest1_model.RData"#% Var explained: 
saveRDS(forest, file_name)

# 2.. Load model under another name
rf_model_loaded <- readRDS(file_name)
rf_model_loaded
rf_model_loaded$finalModel

forest <- rf_model_loaded
#===========================================
## Get variable importance, and turn into a data frame
var_imp <- varImp(forest, scale=FALSE)$importance
var_imp <- data.frame(variables=row.names(var_imp), importance=var_imp$Overall) %>% 
  arrange(desc(importance)) %>% 
  slice(1:30)

## Create a plot of variable importance
var_imp %>%
  ## Sort the data by importance
  arrange(importance) %>%
  ## Create a ggplot object for aesthetic
  ggplot(aes(x=reorder(variables, importance), y=importance)) + 
  ## Plot the bar graph
  geom_bar(stat='identity') + 
  ## Flip the graph to make a horizontal bar plot
  coord_flip() + 
  ## Add x-axis label
  xlab('Variables') +
  ## Add a title
  labs(title='Random forest variable importance') + 
  ## Some layout for the plot
  theme_minimal() + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 20), 
  )
ggsave(file=paste0(user_wd,"/NEON-AOP/data/hs_lai/RFigures/RF_vip.png"),
       width=7,height=8,dpi=400)  

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
ggsave(file=paste0(user_wd,"/NEON-AOP/data/hs_lai/RFigures/RF_obs_pred_ALL_20percentOC.png"),
       width=5,height=5,dpi=400)  

ggplot(data = data.frame(obs = y, 
                         pred = predicted_y),
       aes(x=obs,y=pred))+
  geom_point()+
  geom_smooth(aes(x=obs,y=pred),method = "lm")+
  xlab("observation")+ylab("prediction")+
  stat_regline_equation(label.y = 2.5,label.x = 6, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 1.5, label.x = 6,aes(label = ..rr.label..))+
  # xlim(0,15)+ylim(0,15) +   
  xlim(0,22)+ylim(0,22) +
  geom_abline(intercept = 0,slope = 1)
  

ggsave(file=paste0(user_wd,"/NEON-AOP/data/hs_lai/RFigures/RF_obs_pred_20percentOC.png"),
       width=5,height=5,dpi=400)  
###################################################################################################
## make pls model(partial least squares regression)----####
###########################################################################################################
#foreach to parallel read 6357 spectra files
n.cores <- parallel::detectCores() - 1
#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK"
)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
foreach::getDoParRegistered()
#how many workers are available? (optional)
foreach::getDoParWorkers()
#check if it is registered (optional)
foreach::getDoParWorkers()
start <- Sys.time()
# Build the model on training set
set.seed(123)
model <- train(
  OC.percent~.,   
  data=training_set[,-1], 
  method = "pls",
  scale = TRUE,
  # trControl = trainControl("cv", number = 5),
  tuneLength = 30
)
print( Sys.time() - start ) #~10min
parallel::stopCluster(cl = my.cluster)
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
unregister()
# Plot model RMSE vs different values of components
plot(model)
model
# Print the best tuning parameter ncomp that
# minimize the cross-validation error, RMSE

model$bestTune
# Summarize the final model
summary(model$finalModel)
model$finalModel
#=====================================================================================
#save randomforest model--------------------
# 1. Save model
file_name <- "explore_models/deciduousForest_model_pls.RData"# 0.22888452
file_name <- "explore_models/evergreenForest_model_pls.RData"# 0.2948213
file_name <- "explore_models/mixedForest_model_pls.RData"# 0.06612939
file_name <- "explore_models/woodyWetlands_model_pls.RData" #NULL
file_name <- "explore_models/shrubScrub_model_pls.RData" # 0.7619287
file_name <- "explore_models/cultivatedCrops_mode_plsl.RData"# 0.03761147
file_name <- "explore_models/grasslandHerbaceous_model_pls.RData"# 0.3551481
file_name <- "explore_models/dwarfScrub_model_pls.RData"# NULL


saveRDS(model, file_name)

# 2.. Load model under another name
model_loaded <- readRDS(file_name)
model_loaded
model_loaded$finalModel

model <- model_loaded
#=====================================================================================
glsImp <- varImp(model, scale = FALSE)
glsImp

## Get variable importance, and turn into a data frame
var_imp <- varImp(forest, scale=FALSE)$importance
var_imp <- data.frame(variables=row.names(var_imp), importance=var_imp$Overall) %>% 
  arrange(desc(importance)) %>% 
  slice(1:30)


# png(file=paste0(user_wd,"/NEON-AOP/data/hs_lai/RFigures/pls_vip.png"),
#     width=4,height=6,units="in",res=400)
# # plot(glsImp, top = 30)
# dev.off()

## Create a plot of variable importance
var_imp %>%
  ## Sort the data by importance
  arrange(importance) %>%
  ## Create a ggplot object for aesthetic
  ggplot(aes(x=reorder(variables, importance), y=importance)) + 
  ## Plot the bar graph
  geom_bar(stat='identity') + 
  ## Flip the graph to make a horizontal bar plot
  coord_flip() + 
  ## Add x-axis label
  xlab('Variables') +
  ## Add a title
  labs(title='PLSR variable importance') + 
  ## Some layout for the plot
  theme_minimal() + 
  theme(axis.text = element_text(size = 10), 
        axis.title = element_text(size = 15), 
        plot.title = element_text(size = 20), 
  )

ggsave(file=paste0(user_wd,"/NEON-AOP/data/hs_lai/RFigures/pls_vip.png"),
       width=7,height=8,dpi=400)  

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
