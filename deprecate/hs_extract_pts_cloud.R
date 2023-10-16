# function: hs_extract_pts_cloud()
#extract cloud data :--------------------------------------------
hs_extract_pts_cloud <- function(filename, pts, bands=1:3) {
  stopifnot(all(bands > 0))
  bands <- sort(bands)
  h5_dims <- hs_dims(filename)
  h5_extent <- hs_extent(filename)
  r <- raster::raster(h5_extent, crs = hs_proj4string(filename), 
                      nrows = h5_dims[2], ncols = h5_dims[3])
  
  pts[['row_identifier']] <- seq_len(nrow(pts))
  pts_in_raster <- raster::intersect(pts, r)
  if (nrow(pts_in_raster) == 0) {
    warning('No points are within the hyperspectral raster extent.')
    return(pts[, names(pts) != 'row_identifier'])
  }
  
  # assuming pts is a spatialpointsdataframe, find row/col indices
  cells <- raster::cellFromXY(r, pts_in_raster)
  rowcols <- raster::rowColFromCell(r, cells)
  
  # read the values from the raster
  file_h5 <- hdf5r::H5File$new(filename, mode = 'r+')
  site <- file_h5$ls()$name
  
  cloud_indx <- file_h5[[paste0(site, '/Reflectance/Metadata/Ancillary_Imagery/Weather_Quality_Indicator')]]
  # hdf5r::h5attr_names(cloud_indx)
  # hdf5r::h5attr(cloud_indx, "Description")
  cloud_value <-  matrix(nrow = length(cells), ncol = 3)
  for (i in seq_along(cells)) {
    cloud_value[i, ] <- cloud_indx[1:3, rowcols[i, 'col'], rowcols[i, 'row']]
  }
  
  file_h5$close_all()
  
  colnames(cloud_value) <- c("R","G","B")
  extracted_vals <- cbind(pts_in_raster['row_identifier'], cloud_value)
  res <- sp::merge(pts, extracted_vals, by = 'row_identifier')

   temp <-  res %>% 
    as_tibble() %>% 
    mutate(cloud_index = case_when(R==255&G==0&B==0~"Fail",#red
                                   R==0&G==255&B==0~"OK",#green
                                   R==255&G==255&B==0~"OK",#yellow
                                   TRUE ~ 'Fail')
           ) %>% 
   dplyr::select(row_identifier,cloud_index)
 
   cloudindx <- cbind(res, temp)
   
   cols_to_keep <- !(names(cloudindx) %in% c('row_identifier', 'row_identifier.1', 
                                     'x', 
                                     'y'))
   cloudindx[, cols_to_keep]

 #indicating cloud cover (green <10%, yellow 10%-50%, red >50%).
}


