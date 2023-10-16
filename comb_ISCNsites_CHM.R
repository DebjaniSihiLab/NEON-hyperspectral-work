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
wd=paste0(user_wd,"NEON-AOP/data/iscn/ISCN/")
cat(wd)
setwd(wd)

# combine neon sites hs data into neon_hs_table------
first=FALSE
if(first==TRUE){
  files <- list.files(paste0(user_wd,"/NEON-AOP/data/iscn/ISCN/"),
                      pattern = "CHM_wide.csv$") 
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
      temp_df <- read_csv(paste0(user_wd,"/NEON-AOP/data/iscn/ISCN/",
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
            paste0(user_wd,"/NEON-AOP/data/hs_lai/temporary_ISCN_CHM_table.csv")
  )
}
#final results:------
neon_hs_table <- read_csv(paste0(user_wd,"/NEON-AOP/data/hs_lai/temporary_ISCN_CHM_table.csv"))

soc_30cm_df <- read_csv("/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/iscn/ISCN_30cm_USA_UTM.csv") 
#some sites have different date!
soc_30cm= soc_30cm_df %>% group_by(site_name,profile_name) %>% 
  summarise(OC=mean(OC_30cm),
            lat=first(lat),
            long=first(long))

neon_hs_soc.df <- left_join(neon_hs_table,soc_30cm,by=c("plotID"="profile_name") )

ggplot()+
  geom_point(data=neon_hs_soc.df,aes(x=plotID, y=CHM,group=plotID,
                                     color=OC))+
  scale_color_gradientn(colours = rainbow(3),name = "OC%")



ggsave(file=paste0(user_wd,"/NEON-AOP/data/ISCN_CHM_soc.png"),
       width=8,height=3,dpi=400)  

