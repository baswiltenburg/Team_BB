# Function 1: serveral pre-processing steps. Returns brick-raster layers of landsat 7 and landsat 8.
preprocessing <- function(){
  destFile <- "output"
  if (!dir.exists(file.path(destFile))) {
    dir.create("output")
    }
  untar("data/Landsat7/LT51980241990098-SC20150107121947.tar.gz", exdir = "data/Landsat7")
  untar("data/Landsat8/LC81970242014109-SC20141230042441.tar.gz", exdir = "data/Landsat8")
  ListLS7 <- list.files('data/Landsat7', pattern = glob2rx('*.tif'), full.names = TRUE)
  ListLS8 <- list.files('data/Landsat8', pattern = glob2rx('*.tif'), full.names = TRUE)
  Stack_LS7 <- stack(ListLS7)
  Stack_LS8 <- stack(ListLS8)
  writeRaster(x=Stack_LS7, filename='output/Stack_LS7.tif', datatype='INT2S', overwrite=TRUE)
  writeRaster(x=Stack_LS8, filename='output/Stack_LS8.tif', datatype='INT2S', overwrite=TRUE)
  RasterLS7 <- brick('output/Stack_LS7.tif') 
  RasterLS8 <- brick('output/Stack_LS8.tif')
  result <- c(RasterLS8, RasterLS7)
  return(result)
}
# preprocessing_result <- preprocessing()


# Function 2: Extract clouds from brick-raster layers, which are the result of function 1. 
# The results of function 1 are saved in a variable called 'preprocessing_result'. See main_lesson5.R. 
extract_clouds <- function(preprocessing_result) {
  cloudsLS7 <- preprocessing_result[[2]][[1]]
  cloudsLS8 <- preprocessing_result[[1]][[1]]
  result <- c(cloudsLS7, cloudsLS8)
  return(result)
}
# extract_clouds_result <- extract_clouds(preprocessing_result)


# Function 3: Split layers. Cloud layers are dropped from the brick-raster layers, which are the result of function 1. 
split_layers <- function(preprocessing_result){
  restLS7 <- dropLayer(preprocessing_result[[2]], 1)
  restLS8 <- dropLayer(preprocessing_result[[1]], 1)
  result <- c(restLS7, restLS8)
  return(result)
}
# split_layers_result <- split_layers(preprocessing_result)


# Function 3: Function to assign 'NA' to cells with cloudcover.  
RmClouds <- function(x, y){
  x[y != 0] <- NA
  return(x)
}

# Function 4: Apply function 3 for the splitted brick-raster layers. 
# Cells with cloudcover will get 'NA' value. 
Clouds_NA <- function(split_layers_result, extract_clouds_result) {
  CloudfreeLS7 <- overlay(split_layers_result[[1]], extract_clouds_result[[1]], fun=RmClouds)
  CloudfreeLS8 <- overlay(split_layers_result[[2]], extract_clouds_result[[2]], fun=RmClouds)
  result <- c(CloudfreeLS7, CloudfreeLS8) 
  return(result)
}
# Clouds_NA_result <- Clouds_NA(split_layers_result, extract_clouds_result)


# The function below is a second option to split layers and remove clouds. It combines the above two functions.
# This function is not executed but is just another option to assign "NA" to clouds. 
split_layers_RmClouds <- function(preprocessing_result, extract_clouds_result){  
  restLS7_2 <- dropLayer(preprocessing_result[[2]], 1)
  restLS8_2 <- dropLayer(preprocessing_result[[1]], 1)
  restLS7_2[extract_clouds_result[[1]] != 0] <- NA
  restLS8_2[extract_clouds_result[[2]] != 0] <- NA
  result <- c(restLS7_2, restLS8_2)
  return(result)
}
# split_layers_RmClouds_result <- split_layers_RmClouds(preprocessing_result, extract_clouds_result)


# Function 5: A function to calculate the NDVI.
ndvi <- function(x, y) {
  ndvi <- (y - x) / (x + y)
  return(ndvi)
}


# Function 6: Apply function 5 to the results of function 4.
# The result of function 4 was a new raster layer wherby clouds get the value 'NA'.
cal_ndvi <- function(Clouds_NA_result){
  ndvi_LS7 <- overlay(x=Clouds_NA_result[[1]][[6]], y=Clouds_NA_result[[1]][[7]], fun=ndvi)
  ndvi_LS8 <- overlay(x=Clouds_NA_result[[2]][[5]], y=Clouds_NA_result[[2]][[6]], fun=ndvi)
  result <- c(ndvi_LS7, ndvi_LS8)
  return(result)
}
# cal_ndvi_result <- cal_ndvi(Clouds_NA_result)


# Function 7: Reproject and resample the results of function 6 to be able to substract.
ndvi_reproject_resample <- function(cal_ndvi_result){ 
  ndvi_LS7_reproject <- projectRaster(cal_ndvi_result[[1]], crs='+proj=longlat')
  ndvi_LS8_reproject <- projectRaster(cal_ndvi_result[[2]], crs='+proj=longlat')
  ndvi_LS7_reproject2 <- crop(ndvi_LS7_reproject, extent(ndvi_LS8_reproject))
  ndvi_LS8_reproject2 <- crop(ndvi_LS8_reproject, extent(ndvi_LS7_reproject))
  ndvi_LS7_resample <- projectRaster(ndvi_LS7_reproject2, ndvi_LS8_reproject2)
  result <- c(ndvi_LS7_resample, ndvi_LS8_reproject2)
  return(result)
}
# ndvi_reproject_resample_result <- ndvi_reproject_resample(cal_ndvi_result)


# Function 7: Substract NDVI-Landsat 8 of NDVI-Landsat 7 to calculate the difference.
ndvi_result <- function(ndvi_reproject_resample_result){
  result <- ndvi_reproject_resample_result[[1]] - ndvi_reproject_resample_result[[2]]
  plot(result)
  writeRaster(x=result, filename = 'output/NDVI.tiff', overwrite=TRUE)
  return(result)
}
# NDVI_result <- ndvi_result(reproject_resample_result)

# END OF SCRIPT 