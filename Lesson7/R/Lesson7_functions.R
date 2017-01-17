# Function to exucute pre-processing steps
# Creates folders if not already exist 
# Only downloads the data if not already downloaded previously 

library(raster)
library(rgdal)

preprocessing <- function() {
  destFile <- "data"
  if (!dir.exists(file.path(destFile))) {
    dir.create("data")
  }
  if (!file.exists(file.path("data/modis_ndvi.zip"))){
    download.file("https://raw.githubusercontent.com/GeoScripting-WUR/VectorRaster/gh-pages/data/MODIS.zip", destfile = "data/modis_ndvi.zip", quiet = TRUE)
  }

  unzip("data/modis_ndvi.zip", exdir = "data")
}

preprocessing()

getdata <- function(){
  nlMunicipality <- getData('GADM',country='NLD', level=2)
  Municipality_data <- brick('data/MOD13A3.A2014001.h18v03.005.gri')
  result <- c(nlMunicipality, Municipality_data)
  return(result)
}


nlMunicipality <- getdata()[[1]]
Municipality_data <- getdata()[[2]]


prj_string_RD <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +
                     k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel 
                     +towgs84=565.2369,50.0087,465.658,-0.406857330322398,
                     0.350732676542563,-1.8703473836068,4.0812 +units=m +no_defs")

nlMunicipality_RD <- spTransform(nlMunicipality, prj_string_RD)
Municipality_data_RD <- projectRaster(Municipality_data, crs = prj_string_RD)


mean_january <- extract(Municipality_data_RD$January, nlMunicipality_RD, fun=mean, df = TRUE)
mean_january2 <- mean_january[!is.na(mean_january)]

max_january <- max(mean_january)





