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
wd=paste0(user_wd,"NEON-AOP/data/hs_lai/DP2.30012.001/")
cat(wd)
setwd(wd)

# combine neon sites hs data into neon_hs_table------
first=FALSE
if(first==TRUE){
  files <- list.files(paste0(user_wd,"/NEON-AOP/data/hs_lai/DP3.30015.001/"),
                      pattern = "3m_wide.csv$") 
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
  
  neon_hs_table <- 
    foreach (i = 1:length(files),.combine=rbind) %dopar% {
      temp_df <- read_csv(paste0(user_wd,"/NEON-AOP/data/hs_lai/DP3.30015.001/",
                                 files)[i])
      if(dim(temp_df)[1]==(0|1)){
        temp_df=NULL
      }
      return(temp_df)
    }
  
  print( Sys.time() - start )
  parallel::stopCluster(cl = my.cluster)
  
  names(neon_hs_table)
  
  write_csv(neon_hs_table,
            paste0(user_wd,"/NEON-AOP/data/hs_lai/temporary_neon_CHM_table.csv")
  )
}
#final results:------
neon_hs_table <- read_csv(paste0(user_wd,"/NEON-AOP/data/hs_lai/temporary_neon_CHM_table.csv"))

{
  spc_mgp_sls_30cm_locationSOC <- read_csv(paste0(user_wd,"NEON-AOP/data/hs_lai/NEON_SOC_Locations/spc_mgp_sls_30cm_locationSOC.csv"))
  spc_mgp_sls_30cm_locationSOC$plotID=1:nrow(spc_mgp_sls_30cm_locationSOC)
  allsites=unique(spc_mgp_sls_30cm_locationSOC$siteID)
  length(allsites) #47 we have all NEON has 47 terrestrial sites
  allsites
}
neon_hs_soc.df <- left_join(neon_hs_table,spc_mgp_sls_30cm_locationSOC,by="plotID") 
  
neon_hs_soc.df <- neon_hs_soc.df %>% group_by(plotID) %>% 
  mutate(CanopyHeight=mean(CHM))

ggplot()+
  geom_point(data=neon_hs_soc.df,aes(x=plotID, y=CanopyHeight,group=plotID,
                                     color=`wtmean.estimatedOC.percent`))+
  scale_color_gradientn(colours = rainbow(3),name = "OC%")

ggplot()+
  geom_point(data=neon_hs_soc.df,aes(x=plotID, y=CHM,group=plotID,
                                    color=`wtmean.estimatedOC.percent`))+
  scale_color_gradientn(colours = rainbow(3),name = "OC%")


ggsave(file=paste0(user_wd,"/NEON-AOP/data/neon_CHM_soc.png"),
       width=8,height=3,dpi=400)  

