# Title:            Geoscripting lesson 5 - calculate NDVI from Landsat
# Teamname:         Team BB
# Team member 1:    Bram Schippers
# Team member 2:    Bas Wiltenburg
# Date:             Januari the 15th, 2017

# Click Source to get final result.

# install.packages("raster")
# install.packages("rgdal")
library(raster)
library(rgdal)

# Run all the functions from "functions.R" to calculate the NDVI.
source('R/functions.R')

# Results of all separate functions are assigned to variables. 
# Assigning results to variables is necessary to use the results in follow up functions. 
preprocessing_result <- preprocessing()
extract_clouds_result <- extract_clouds(preprocessing_result)
split_layers_result <- split_layers(preprocessing_result)
Clouds_NA_result <- Clouds_NA(split_layers_result, extract_clouds_result)
cal_ndvi_result <- cal_ndvi(Clouds_NA_result)
ndvi_reproject_resample_result <- ndvi_reproject_resample(cal_ndvi_result)
NDVI_result <- ndvi_result(ndvi_reproject_resample_result) 

#Remove intermediate files
file.remove("output/Stack_LS7.tif")
file.remove("output/Stack_LS8.tif")

# END OF SCRIPT
