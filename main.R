#1.get_NEON_data.R
#2.getOC_location.R
#3.download_hs_flightlines.R ---> flight line data DP1. This process is on CADES.
#.OR: download_hs_Tile.R

#statistic cloud condition:---->need a plot show sites cloud condition

#4.Use Python--->code/My_functions_python/apply_brdf_correct.py
#.     /lustre/or-scratch/cades-ccsi/scratch/z2w/NEON_hs/code/My_functions_python/apply_brdf_correct.py
#.     One site could take 10hr on CADES.

#5. CADES_extract_ENVI_3m_buffer.R Then manually transfer output csv from CADES to local 
#6. comb_NEONsites_hs.R

#7. download_lai_flightlines.R
#8. extract_lai_with_buffer.R
#9. comb_NEONsites_LAI.R

#7. cades_download_CHM.R
#8. extract_CHM_with_buffer.R
#9. comb_NEONsites_CHM.R

#10. clean_data_for_Rf_Pls.R
#11. plot_NEON_Hs_fig.R/
#       plot_Hsfig_flightlines_singleSite.R
#12. make_simple_rf_pls.R




library(tidyverse)
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

#source(paste0(user_wd,"NEON-AOP/code/My_functions_R/hs_extract_PlotsSites.R"))
#source(paste0(user_wd,"NEON-AOP/code/My_functions_R/lai_extract_PlotsSites.R"))
wd=paste0(user_wd,"NEON-AOP/data/hs_lai/")
setwd(wd)
load(file='shared_flights.rda')
shared_flights
#read in spc_mgp_sls 3 data with 30cm OC and precise location in UTM------
#data from getOC_location.R
#input data, soil OC and location:
{
spc_mgp_sls_30cm_locationSOC <- read_csv(paste0(user_wd,"/NEON-AOP/data/hs_lai/NEON_SOC_Locations/spc_mgp_sls_30cm_locationSOC_withPFT_Soiltype.csv"))
  
spc_mgp_sls_30cm_locationSOC <- read_csv(paste0(user_wd,"NEON-AOP/data/hs_lai/NEON_SOC_Locations/spc_mgp_sls_30cm_locationSOC.csv"))
spc_mgp_sls_30cm_locationSOC$rowID=1:nrow(spc_mgp_sls_30cm_locationSOC)
allsites=unique(spc_mgp_sls_30cm_locationSOC$siteID)
length(allsites) #47 we have all NEON has 47 terrestrial sites
allsites
}
#eyecheck all sites information-------
{
i=11;allsites[i]
site=allsites[i]
sites_available_year <- get_Hs_available_year(site,whichpc) #This funtion in hs_extract_PlotsSites.R
sites_available_year
}
#Hold sites that are special-----
# check for sites that are flown under the flight box of a different site
if(site %in% c('TREE','CHEQ','KONA','DCFS')) {
  cat("ites that are flown under the flight box of a different site\n")
  cat("ignore this site for now.\n")
}
# 'STEI' plots are in two UTM zone 15N,16N, there are two names for HDF5: "[STEI|CHEQ]"
if(site %in% c('STEI')) {
  cat("'STEI' plots are in two UTM zone 15N,16N, there are two names for HDF5: STEI,CHEQ","\n")
  cat("ignore this site for now.\n")
}

indx <- !allsites %in% c('STEI','TREE','CHEQ','KONA','DCFS')
Clean_allsites <- allsites[indx]
#loop all Clean_allsites, download HS and LAI----
#mannual process  c('STEI','TREE','CHEQ','KONA','DCFS') later----
for (i in 11:length(Clean_allsites)) {
  site=Clean_allsites[i] #
  cat("############","Start ", site,"site","############")
  cat("\n",site," available year: ",get_Hs_available_year(site,whichpc) )
  cat("\n",site,"\nhyperspectral processing-----\n\n")

  res <- try(
  hs_extract_PlotsSites_buffer(
    wd=paste0(user_wd,"NEON-AOP/data/hs_lai/"),
    spc_mgp_sls_30cm_locationSOC= spc_mgp_sls_30cm_locationSOC,
    site=site,buffer=3,
    downloadAOPfile=TRUE,
    year=0,
    whichpc=whichpc) #next time don't need to download AOP tiles, make this FALSE.
  )
  if(inherits(res, "try-error"))
  {
    #error handling code, maybe just skip this iteration using
    line= paste0("Error: ",site," loop next\n\n")
    write(line,file="my_logfile.txt",append=TRUE)
    next
  }
  #rest of iteration for case of no error
  
  cat("\nLAI processing-----\n\n")
  lai_extract_PlotsSites_buffer(
    wd=paste0(user_wd,"NEON-AOP/data/hs_lai/"),
    spc_mgp_sls_30cm_locationSOC= spc_mgp_sls_30cm_locationSOC,
    site=site,buffer=3,
    downloadAOPfile=TRUE,
    year=0,
    whichpc=whichpc) #next time don't need to download AOP tiles, make this FALSE.
  cat("\n",site,": succeed!------\n\n")
  
}
#END here,above are useful.---------------------------------------------------------------------------------
#
#test site:SCBI
site=allsites[13] #4 SCBI  
#12 TREE download TREE is part of the flight box for STEI. Downloading data from STEI.
print(site)
cat(site,": ",get_Hs_available_year(site,whichpc),"\n" )
sites_available_year <- get_Hs_available_year(site,whichpc) #This funtion in hs_extract_PlotsSites.R
sites_available_year

hs_extract_PlotsSites_buffer(
  wd=paste0(user_wd,"NEON-AOP/data/hs_lai/"),
  spc_mgp_sls_30cm_locationSOC= spc_mgp_sls_30cm_locationSOC,
  site=site,buffer=3,
  downloadAOPfile=FALSE,
  year=2020,
  whichpc=whichpc) #next time don't need to download AOP tiles, make this FALSE.


#test-------
# source(paste0(user_wd,"NEON-AOP/code/My_functions/hs_extract_PlotsSites.R"))
# hs_extract_PlotsSites_buffer(
#   wd=paste0(user_wd,"NEON-AOP/data/hs_lai/"),
#   spc_mgp_sls_30cm_locationSOC= spc_mgp_sls_30cm_locationSOC,
#   site="HARV",buffer=3,
#   downloadAOPfile=TRUE,
#   whichpc=whichpc) #next time don't need to download AOP tiles, make this FALSE.
# 
# source(paste0(user_wd,"NEON-AOP/code/My_functions/lai_extract_PlotsSites.R"))
# lai_extract_PlotsSites_buffer(
#   wd=paste0(user_wd,"NEON-AOP/data/hs_lai/"),
#   spc_mgp_sls_30cm_locationSOC= spc_mgp_sls_30cm_locationSOC,
#   site="HARV",buffer=3,
#   downloadAOPfile=FALSE,
#   whichpc=whichpc
#   ) #next time don't need to download AOP tiles, make this FALSE.
# 

