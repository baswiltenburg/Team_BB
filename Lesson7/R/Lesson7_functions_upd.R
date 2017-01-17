## FUNCTION WHICH EXECUTE THE DOWNLOAD AND UNZIPPING WORK
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
## preprocessing()


## FUNCTION TO GET THE DATA FROM MULTIPLE LAYERS OF THE DOWNLOADED FILE
getdata <- function(level){
  nlMunicipality <- getData('GADM',country='NLD', level=level)
  Municipality_data <- brick('data/MOD13A3.A2014001.h18v03.005.gri')
  result <- c(nlMunicipality, Municipality_data)
  return(result)
}
## result <- getdata(1) 
## nlMunicipality <- result[[1]]
## Municipality_data <- result[[2]]


## FUNCTION TO SET THE COORDINATE SYSTEM OF "nlMunicipality" to RD_NEW
reproject_nlMunicipality <- function(nlMunicipality){
  prj_string_RD <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +
                     k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel 
                     +towgs84=565.2369,50.0087,465.658,-0.406857330322398,
                     0.350732676542563,-1.8703473836068,4.0812 +units=m +no_defs")
  nlMunicipality_RD <- spTransform(nlMunicipality, prj_string_RD)
  return(nlMunicipality_RD)
}
## nlMunicipality_RD <- reproject_nlMunicipality(nlMunicipality)


## FUNCTION TO SET THE COORDINATE SYSTEM OF "Municipality_data" to RD_NEW.
## CROPS AND MASK THE "Municipality_data" to the reprojected "nlMunicipality_RD"
reproject_Municipality_data <- function(Municipality_data, nlMunicipality_RD){
  prj_string_RD <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +
                       k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel 
                       +towgs84=565.2369,50.0087,465.658,-0.406857330322398,
                       0.350732676542563,-1.8703473836068,4.0812 +units=m +no_defs")
  Municipality_data_RD <- projectRaster(Municipality_data, crs = prj_string_RD)
  NDVI_NL_CROP <- crop(Municipality_data_RD, nlMunicipality_RD)
  NDVI_NL_MASK <- mask(NDVI_NL_CROP, nlMunicipality_RD)
  return(NDVI_NL_MASK)
}
## NDVI_NL_MASK <- reproject_Municipality_data(Municipality_data, nlMunicipality_RD)


## FUNCTION TO CALCULATE THE GREENEST MUNICIPALITY, DEPENDENT ON A GIVEN MONTH (INPUT VARIABLE)  
greenest_municipality_month <- function(NDVI_NL_MASK, month, nlMunicipality_RD){
  mean_month <- extract(subset(NDVI_NL_MASK, month), nlMunicipality_RD, fun=mean, df = TRUE)
  Greenest_Municipality <- mean_month[which.max(mean_month[, month]), "ID"]
  Mun_name_month <- subset(nlMunicipality_RD, nlMunicipality_RD$OBJECTID==Greenest_Municipality)
  result <- c(Greenest_Municipality, Mun_name_month)
  return(result)
}
## January <- greenest_municipality_month (NDVI_NL_MASK, "January", nlMunicipality_RD)
## greenest_municipality_January <- January[[1]]
## Mun_name_January <- January[[2]] 

## August <- greenest_municipality_month (NDVI_NL_MASK, "August", nlMunicipality_RD)
## greenest_municipality_august <- August[[1]]
## Municipality_name_august <- August[[2]] 


## FUNCTION TO CALCULATE THE GREENEST MUNICIPALITY OVER THE WOLE YEAR  
greenest_municipality_year <- function(NDVI_NL_MASK, nlMunicipality_RD){
  mean_year <- extract(NDVI_NL_MASK, nlMunicipality_RD, na.rm=TRUE, fun=mean, df = TRUE)
  mean_year_municipality <- data.frame(ID=mean_year[,1], Means=rowMeans(mean_year[,-1]))
  greenest_municipality_year <- subset(mean_year_municipality$ID, mean_year_municipality$Means==max(mean_year_municipality$Means, na.rm=TRUE))
  Mun_name_year <- subset(nlMunicipality_RD, nlMunicipality_RD$OBJECTID==greenest_municipality_year)
  return(Mun_name_year)
}
## Mun_name_year <- greenest_municipality_year(NDVI_NL_MASK, nlMunicipality_RD)
  

## FUNCTION TO CREATE OUTPUTMAPS FOR THE PDF-FILES  
create_output_maps <- function(){
  dir.create('output', showWarnings = F)
  dir.create('output/provinces', showWarnings = F)
  dir.create('output/municipalities' , showWarnings = F)
}  
  