#combine neon sites hs data into neon_hs_table------
library(tidyverse)
library(data.table)
library(foreach)
library(doParallel)


#setwd to home, then check which pc
setwd("~")
whichpc=getwd()
cat(whichpc)
if(whichpc=="/Users/zwang61"){
  user_wd="/Users/zwang61/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
}else{
  user_wd="/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/"
}

  files <- list.files(paste0(user_wd,"/NEON-AOP/data/hs_lai/DP3.30006.001/"),
                      pattern = "_HS_windows3m.csv") 
  #foreach to parallel read 6357 spectra files
  parallel::detectCores() #10
  n.cores <- parallel::detectCores() - 2
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
      temp_df <- read_csv(paste0(user_wd,"/NEON-AOP/data/hs_lai/DP3.30006.001/",
                                 files)[i])
      if(dim(temp_df)[1]==(0|1)){
        temp_df=NULL
      }
      return(temp_df)
    }
  
  print( Sys.time() - start )
  parallel::stopCluster(cl = my.cluster)
  
  names(neon_hs_table)
  write_csv(neon_hs_table,paste0(user_wd,"/NEON-AOP/data/hs_lai/temporary_neon_hs_table.csv")
            )


neon_hs_table <- read_csv(paste0(user_wd,"/NEON-AOP/data/hs_lai/temporary_neon_hs_table.csv"))

df <- neon_hs_table %>% group_by(wavelength) %>% 
  summarise(has_wavelength=length(unique(plotID)))
