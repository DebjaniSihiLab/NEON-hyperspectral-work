library(terra)
file_dir="/Users/zhuonanwang/local_folder/CADES_NEON_hs/"
setwd(file_dir)
list.files(file_dir)
list.files()

h5envifile=rast("NEON_D10_CPER_DP1_20210525_170421_reflectance")
correctedenvifile=rast("NEON_D10_CPER_DP1_20210525_170421_reflectance_BRDF_topo_corrected")

filename="NEON_D10_CPER_DP1_20210525_170421_reflectance.h5"
# read the values from the raster
file_h5 <- hdf5r::H5File$new(filename, mode = 'r+')
site <- file_h5$ls()$name

cloud_indx <- file_h5[[paste0(site, '/Reflectance/Metadata/Ancillary_Imagery/Haze_Cloud_Water_Map')]]
hdf5r::h5attr_names(cloud_indx)
hdf5r::h5attr(cloud_indx, "Description")
hdf5r::h5attributes(cloud_indx)

names(hdf5r::h5attributes(cloud_indx))

library(raster)
library(rhdf5)
library(rgdal)
f="NEON_D10_CPER_DP1_20210525_170421_reflectance.h5"
# look at the HDF5 file structure 
View(h5ls(f,all=T))
Cloud_Water_Info <- h5readAttributes(f,"/CPER/Reflectance/Metadata/Ancillary_Imagery/Haze_Cloud_Water_Map/")
names(Cloud_Water_Info)
Cloud_Water <- h5read(f,"/CPER/Reflectance/Metadata/Ancillary_Imagery/Haze_Cloud_Water_Map/")
t=t(Cloud_Water)

reflInfo <- h5readAttributes(f, "/CPER/Reflectance/Reflectance_Data")


H5close()
