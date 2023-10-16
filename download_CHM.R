#https://www.neonscience.org/resources/learning-hub/tutorials/tree-heights-veg-structure-chm
# Canopy height model data

byTileAOP(dpID="DP3.30015.001", site="WREF", year="2017", 
          easting=veg$adjEasting[which(veg$plotID=="WREF_075")], 
          northing=veg$adjNorthing[which(veg$plotID=="WREF_075")],
          check.size=FALSE, savepath=wd)

#https://www.neonscience.org/resources/learning-hub/tutorials/neondatastackr
byFileAOP("DP3.30015.001", site="HOPB", 
          year="2017", check.size=FALSE)

