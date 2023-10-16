# load libraries
library(soilDB)
require(aqp)
require(lattice)
#VNIR Spectra
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
wd=paste0(user_wd,"NEON-AOP/data/R_RaCA/")
cat(wd)
setwd(wd)

#1. Download RaCA data-into R_downloaded----
# fetch data by rcasiteid
#VNIR spectra are large, and therefore not downloaded by default. 
#The get.vnir=TRUE argument to fetchRaCA() will download VNIR spectra associated with the query results.
#rcasiteid

pedonlist <- read_csv("/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/R_RaCA/Data Tables/RaCA_SOC_pedons.csv")
siteloc_list <- read_csv("/Users/zhuonanwang/Library/CloudStorage/OneDrive-EmoryUniversity/01_Projects-sihi/NEON-AOP/data/R_RaCA/Data Tables/RaCa_general_location.csv") 

pedonlist$rcasiteid %>% length #6236
pedonlist$rcasiteid %>% unique %>% length #6236
siteloc_list$RaCA_Id%>% length #6405
siteloc_list$RaCA_Id%>% unique %>%length #6403

sum(!pedonlist$rcasiteid %in% siteloc_list$RaCA_Id) #23
sum(!siteloc_list$RaCA_Id %in% pedonlist$rcasiteid) #190

indx <- !siteloc_list$RaCA_Id %in% pedonlist$rcasiteid
temp <- siteloc_list$RaCA_Id[indx] #190
rcasiteid = c(pedonlist$rcasiteid,temp) #include all siteID #6426
#I don't know why pedon and siteloc don't have same racasite-id.


#1.2 download RaCA into R_downloaded/ use 6426 rcasitedid with fethcRaCA function:-------
download_RaCA=FALSE
if(download_RaCA==TRUE){
for (i in 1:length(rcasiteid)) {#6426
  r.state <- try(
    fetchRaCA(rcasiteid=rcasiteid[i], get.vnir=TRUE)
  )
  if(inherits(r.state, "try-error")){
    #error handling code, maybe just skip this iteration using
    line= paste0("Error: i=",i," loop next\n\n")
    write(line,file="Download_Error_logfile.txt",append=TRUE)
    next
  }
  #rest of iteration for case of no error
  site_df <- aqp::site(r.state$pedons)
  sample_df <- r.state$sample
  spectra_df <- data.frame(r.state$spectra)
  names(spectra_df) <- colnames(r.state$spectra)
  
  write.csv(site_df, paste0("R_downloaded/",i,"_site.csv"))
  write.csv(sample_df, paste0("R_downloaded/",i,"_sample.csv"))
  write.csv(spectra_df, paste0("R_downloaded/",i,"_spectra.csv"))
}
}

#1.3 save site_df,sample_df,spectra_df as csv file for future use:------
#1.3.1.combine downloaded RaCA site x,y data in R_downloaded-------
#combine all site files to one dataframe:
firsttime=FALSE
if(firsttime==TRUE){
sitefiles <- list.files("R_downloaded/",pattern = "_site.csv") 
site_table <- read_csv(paste0("R_downloaded/",sitefiles))
site_table <- site_table %>% dplyr::select(rcapid,rcasiteid,obs_date,x,y,state)
write.csv(site_table,"RaCA_sites_table.csv") #save, future only need to read in this file.
rm(site_table)
}

raca_sites_table <- read_csv("RaCA_sites_table.csv")

#summary some information
raca_sites_table %>% dim
unique(raca_sites_table$rcasiteid) %>% length()

site_pedon_repeats <- raca_sites_table %>% group_by(rcasiteid) %>% 
  summarise(n=n())
table(site_pedon_repeats$n)
#1.3.2. combine all spectra files to one dataframe:-----
#it takes 15min with parallel, don't run second time.
firsttime=FALSE
if(firsttime==TRUE){
spectrafiles <- list.files("R_downloaded/",pattern = "_spectra.csv") 
length(spectrafiles) #6357

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

spectra_df <- 
  foreach (i = 1:length(spectrafiles),.combine=rbind) %dopar% {
  cat(i,"\n")
  temp_df <- read_csv(paste0("R_downloaded/",spectrafiles)[i])
  if(dim(temp_df)[1]==0){
    temp_df=NULL
  }
  return(temp_df)
  }

print( Sys.time() - start )
parallel::stopCluster(cl = my.cluster)

spectra_df <- spectra_df %>% rename(sample_id="...1")
write_csv(spectra_df,"RaCA_spectra_table.csv")
rm(spectra_df)
}

raca_spectra_table <- read_csv("RaCA_spectra_table.csv")

#summary:
raca_spectra_table %>% dim
unique(raca_spectra_table$sample_id) %>% length()
#1.3.3. combine all 6357 sample files to one dataframe:------
firsttime=FALSE
if(firsttime==TRUE){
samplefiles <- list.files("R_downloaded/",pattern = "_sample.csv") 

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

sample_df <- 
  foreach (i = 1:length(samplefiles),.combine=rbind) %dopar% {
    cat(i,"\n")
    temp_df <- read_csv(paste0("R_downloaded/",samplefiles)[i])
    if(dim(temp_df)[1]==0){
      temp_df=NULL
    }
    return(temp_df)
  }

print( Sys.time() - start )
parallel::stopCluster(cl = my.cluster)

names(sample_df)
write_csv(sample_df,"RaCA_sample_table.csv")
rm(sample_df)
}

raca_sample_table <- read_csv("RaCA_sample_table.csv")
#summary:
raca_sample_table %>% dim
unique(raca_sample_table$rcapid) %>% length()
#=========================================================
#2. find Match NEON and RaCA lat, long--------
#=========================================================
#2.1 RaCA accurate location, lat long 5 decimal.#get this data from RaCA staff.-------

RaCA_accurate_loc <- read_csv("RaCA_data_summary.csv")
head(RaCA_accurate_loc)
dim(RaCA_accurate_loc) # 6421, compare to 6426 (RaCa_general_location.csv + pedon siteids)

#2.2. NEON SOC pits location (!not all pits have hyperspectral data) ------
spc_mgp_sls_30cm_locationSOC <- read_csv(paste0(user_wd,"NEON-AOP/data/hs_lai/NEON_SOC_Locations/spc_mgp_sls_30cm_locationSOC.csv"))
spc_mgp_sls_30cm_locationSOC$rowID=1:nrow(spc_mgp_sls_30cm_locationSOC)
names(spc_mgp_sls_30cm_locationSOC)
spc_mgp_sls_30cm_locationSOC$lat <- spc_mgp_sls_30cm_locationSOC$pitnorthing
spc_mgp_sls_30cm_locationSOC$long <- spc_mgp_sls_30cm_locationSOC$piteasting
#2.2.1. convert NEON UTM to lat and long----
# install.packages("PBSmapping")
library("PBSmapping")
for (i in 1:nrow(spc_mgp_sls_30cm_locationSOC)) {
  latlong=spc_mgp_sls_30cm_locationSOC[i,] %>%
    dplyr::select(X=piteasting,Y=pitnorthing)
  attr(latlong, "projection") <- "UTM"
  utmzone= as.numeric(gsub("N", "", spc_mgp_sls_30cm_locationSOC[i,"utmZone"]))
  attr(latlong, "zone") <- utmzone 
  latlongUTM <- convUL(latlong, km=FALSE) # convert UTM to lat, long
  spc_mgp_sls_30cm_locationSOC$lat[i]=latlongUTM$Y
  spc_mgp_sls_30cm_locationSOC$long[i]=latlongUTM$X
}

#2.3. Match NEON and RaCA lat, long--------
# mathc neon and raca in 100m around, 
# 0.00833333～1km； 
# 0.01 = 1.11 km (2 decimals, km accuracy)
# 0.001 =111 m
neon_latlong <- spc_mgp_sls_30cm_locationSOC 
raca_latlong <- RaCA_accurate_loc %>% rename(lat=Lat,long=Lon)
rm(spc_mgp_sls_30cm_locationSOC,RaCA_accurate_loc) #save memory

#0.05 5km 45 neon sites match
#0.01 1km 3 neon sites match
#0.02 2km 6 neon sites match
match_list=list()
 for (i in 1:nrow(raca_latlong)) {
  difflat=neon_latlong$lat-raca_latlong$lat[i]
  difflong=neon_latlong$long-raca_latlong$long[i]
  
  comp=ifelse(abs(difflat)>0.05, FALSE,
         ifelse(abs(difflong)<0.05, TRUE, FALSE))
  
    NEON_row_indx <- which(comp)
    match_list[[i]]=list(neon_row=NEON_row_indx,raca_row=i)
  
}
# str(match_list)
#quick look how many mathes
 map(match_list,1) %>% 
  unlist %>% length()

match_df=data.frame()
for (i in 1:length(match_list)) {
  neontemp=match_list[i][[1]]$neon_row
  racatemp=match_list[i][[1]]$raca_row
  if(length(neontemp)>0){
  temp=data.frame(neonN=neontemp,racaN=racatemp)
  }else{
    temp=NULL
  }
 match_df=bind_rows(match_df,temp)
}
# match_df, first column is NEON plot_id/row_id, second is raca_latlong row number,
# we can locatate the raca_latlong row to extract RaCA_site id.

#2.4. Extract Match NEON and RaCA HS and soc making new dataframe;--------
raca_hs_soc.df <- data.frame()
for (i in 1:nrow(match_df)) {
raca_siteID <- raca_latlong[match_df[i,2],]$RaCA_site
neon_plotid <- neon_latlong[match_df[i,1],]$rowID
#find matched raca and neon id, then find their spectral, OC, date

#raca_siteID to find the samples at that site,
#find the first layer samples to get sampleid
#sampleid to find thte spectra in raca_spectra_table

#one raca site have several pedons/sampleid
pedonid=raca_sites_table[raca_sites_table$rcasiteid==raca_siteID,]$rcapid
sampleID <- raca_sample_table[raca_sample_table$rcapid %in% pedonid,] %>% 
  # filter(sample_top==0) %>% 
  pull(sample_id)

#use sample_id get hs for raca ----
raca_hs=raca_spectra_table[raca_spectra_table$sample_id %in% sampleID,]

raca_hs_df <- raca_hs %>%
  gather(wavelength, reflectance, 
         -sample_id) %>%
  mutate(wavelength = parse_number(wavelength)) 

temp_sample <- raca_sample_table[raca_sample_table$rcapid %in% pedonid,] %>% 
  select(sample_id,rcapid,sample_top,sample_bottom,soc)
temp_raca_sites <- raca_sites_table[raca_sites_table$rcasiteid==raca_siteID,] %>% 
  select(-`...1`)
sample_info_df <- left_join(temp_sample,temp_raca_sites,by="rcapid")
raca_hs_soc <- left_join(raca_hs_df,sample_info_df,by="sample_id")
raca_hs_soc.df <- bind_rows(raca_hs_soc.df,raca_hs_soc)
}

ggplot(data=raca_hs_soc.df)+
  geom_line(aes(x=wavelength, y=reflectance,group=sample_id,color=soc))+
  scale_color_gradientn(colours = rainbow(3))+
  xlab("Wavelength (nm)")+
  ylab("Reflectance")+
  labs(color = "OC (%)")

ggsave(file=paste0(user_wd,"/NEON-AOP/data/raca_hs_soc.png"),
       width=8,height=3,dpi=400)

#2.4.1 combine neon sites hs data into neon_hs_table------
first=FALSE
if(first==TRUE){
files <- list.files(paste0(user_wd,"/NEON-AOP/data/hs_lai/DP3.30006.001/"),
                          pattern = "_HS_windows3m.csv") 
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
write_csv(neon_hs_table,
          paste0(user_wd,"/NEON-AOP/data/hs_lai/temporary_neon_hs_table.csv")
         )
}

neon_hs_table <- read_csv(paste0(user_wd,"/NEON-AOP/data/hs_lai/temporary_neon_hs_table.csv"))

neon_hs_soc.df <- data.frame()
for (i in 1:nrow(match_df)) {
neon_plotid <- neon_latlong[match_df[i,1],]$rowID
#find matched raca and neon id, then find their spectral, OC, date
neon_hs <- neon_hs_table[neon_hs_table$plotID %in%neon_plotid,]
neon_hs_soc <- left_join(neon_hs,neon_latlong,by=c("plotID"="rowID") )

neon_hs_soc.df <- bind_rows(neon_hs_soc.df,neon_hs_soc)
}
#14 plot_id in neon

ggplot()+
  geom_line(data=neon_hs_soc.df,aes(x=wavelength, y=reflectance,group=plotID,
                                    color=`wtmean.estimatedOC.percent`))+
  scale_color_gradientn(colours = rainbow(3))+
  xlab("Wavelength (nm)")+
  ylab("Reflectance")+
  labs(color = "OC (%)")

ggsave(file=paste0(user_wd,"/NEON-AOP/data/neon_hs_soc.png"),
       width=8,height=3,dpi=400)  

ggplot()+
  geom_line(data=raca_hs_soc.df,aes(x=wavelength, y=reflectance,
                                    group=sample_id,color=soc),
            linetype = "dashed",linewidth=0.2)+
  geom_line(data=neon_hs_soc.df,aes(x=wavelength, y=reflectance,group=plotID,
                                 color=`wtmean.estimatedOC.percent`))+
  scale_color_gradientn(colours = rainbow(3))+
  xlab("Wavelength (nm)")+
  ylab("Reflectance")+
  labs(color = "OC (%)")

ggsave(file=paste0(user_wd,"/NEON-AOP/data/RaCA_NEON_hs_soc.png"),
       width=8,height=3,dpi=400)  

