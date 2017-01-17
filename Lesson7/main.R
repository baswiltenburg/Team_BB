# Title:            Geoscripting lesson 6 
# Teamname:         Team BB
# Team member 1:    Bram Schippers
# Team member 2:    Bas Wiltenburg
# Date:             Januari the 17th, 2017

# install.packages("rgeos")
# install.packages("rgdal")
# install.packages("raster")

# Click Source to get final result.

# REMARK!
# RUN THE SCIRPT TWICE !! THIS IS NECESSARY SINCE YOU ENTER A LEVEL YOURSELF. IN THE FIRST RUN YOU SHOULD ENTER
# '1' TO GENERATE THE PROVINCE MAPS. IN THE SECOND RUN YOU SHOULD ENTER "2" TO GENERATE THE MUNICIPALITY MAPS. 
# CALCULATION TAKES SOME TIME, SO WAIT A MINUTE AFTER ENTERING THE LEVEL!
# OUR SCRIPT CREATES PDF-FILES AND .TIF-FILES IN THE "output" FOLDER !


library(rgeos)
library(rgdal)
library(raster)
library(sp)


# Run all the functions from "functions.R" to calculate the NDVI.
source('R/Lesson7_functions_upd.R')


# Wait a while after entering the level !
enterlevel <- readline("Enter 1 for provinces, enter 2 for municipalities: ")


# Results of all separate functions are assigned to variables. 
# Assigning results to variables is necessary to use the results in follow up functions.
preprocessing()

result <- getdata(enterlevel) #User input!
nlMunicipality <- result[[1]]
Municipality_data <- result[[2]]

nlMunicipality_RD <- reproject_nlMunicipality(nlMunicipality)

NDVI_NL_MASK <- reproject_Municipality_data(Municipality_data, nlMunicipality_RD)

January <- greenest_municipality_month (NDVI_NL_MASK, "January", nlMunicipality_RD)
greenest_municipality_January <- January[[1]]
Mun_name_January <- January[[2]] 

August <- greenest_municipality_month (NDVI_NL_MASK, "August", nlMunicipality_RD)
greenest_municipality_august <- August[[1]]
Mun_name_August <- August[[2]] 

Mun_name_year <- greenest_municipality_year(NDVI_NL_MASK, nlMunicipality_RD)

create_output_maps()


# PLOT AND CREATE MAPS OF LEVEL 1 (PROVINCES) SEE OUTPUT FOLDER
if (enterlevel == "1") {
  plot(NDVI_NL_MASK$January, axes=TRUE)
  plot(Mun_name_January, add = TRUE, col=rgb(0.5,0.5,0.5,0.5))
  text((coordinates(Mun_name_January)[1,1]+50000), (coordinates(Mun_name_January)[1,2]+10000), labels = Mun_name_January$NAME_1, cex = 1, col="black", font=2)
  mtext(side = 3, line = 1, "Greenest province in January", cex = 1.5, font=2)
  
  pdf("output/provinces/Provinces_January.pdf")
  plot(NDVI_NL_MASK$January, axes=TRUE)
  plot(Mun_name_January, add = TRUE, col=rgb(0.5,0.5,0.5,0.5))
  text((coordinates(Mun_name_January)[1,1]+50000), (coordinates(Mun_name_January)[1,2]+10000), labels = Mun_name_January$NAME_1, cex = 1, col="black", font=2)
  mtext(side = 3, line = 1, "Greenest province in January", cex = 1.5, font=2)
  dev.off()
  
  tiff("output/provinces/Provinces_January.tif")
  plot(NDVI_NL_MASK$January, axes=TRUE)
  plot(Mun_name_January, add = TRUE, col=rgb(0.5,0.5,0.5,0.5))
  text((coordinates(Mun_name_January)[1,1]+50000), (coordinates(Mun_name_January)[1,2]+10000), labels = Mun_name_January$NAME_1, cex = 1, col="black", font=2)
  mtext(side = 3, line = 1, "Greenest province in January", cex = 1.5, font=2)
  dev.off()
  }


# PLOT AND CREATE MAPS OF LEVEL 2 (MUNICIPALITIES) SEE OUTPUT FOLDER
if (enterlevel == '2'){
  plot(NDVI_NL_MASK$August, axes=TRUE)
  plot(Mun_name_August, add = TRUE, col=rgb(0.5,0.5,0.5,0.5))
  text((coordinates(Mun_name_August)[1,1]+50000), (coordinates(Mun_name_August)[1,2]+10000), labels = Mun_name_August$NAME_2, cex = 1, col="black", font=2)
  mtext(side = 3, line = 1, "Greenest municipality in August", cex = 1.5, font=2)
  
  pdf("output/municipalities/municipalities_august.pdf")
  plot(NDVI_NL_MASK$August, axes=TRUE)
  plot(Mun_name_August, add = TRUE, col=rgb(0.5,0.5,0.5,0.5))
  text((coordinates(Mun_name_August)[1,1]+50000), (coordinates(Mun_name_August)[1,2]+10000), labels = Mun_name_August$NAME_2, cex = 1, col="black", font=2)
  mtext(side = 3, line = 1, "Greenest municipality in August", cex = 1.5, font=2)
  dev.off()
  
  tiff("output/municipalities/municipalities_august.tif")
  plot(NDVI_NL_MASK$August, axes=TRUE)
  plot(Mun_name_August, add = TRUE, col=rgb(0.5,0.5,0.5,0.5))
  text((coordinates(Mun_name_August)[1,1]+50000), (coordinates(Mun_name_August)[1,2]+10000), labels = Mun_name_August$NAME_2, cex = 1, col="black", font=2)
  mtext(side = 3, line = 1, "Greenest municipality in August", cex = 1.5, font=2)
  dev.off()
  
  plot(NDVI_NL_MASK$January, axes=TRUE)
  plot(Mun_name_January, add = TRUE, col=rgb(0.5,0.5,0.5,0.5))
  text((coordinates(Mun_name_January)[1,1]+50000), (coordinates(Mun_name_January)[1,2]+10000), labels = Mun_name_January$NAME_2, cex = 1, col="black", font=2)
  mtext(side = 3, line = 1, "Greenest municipality in January", cex = 1.5, font=2)
  
  pdf("output/municipalities/municipalities_january.pdf")
  plot(NDVI_NL_MASK$January, axes=TRUE)
  plot(Mun_name_January, add = TRUE, col=rgb(0.5,0.5,0.5,0.5))
  text((coordinates(Mun_name_January)[1,1]+50000), (coordinates(Mun_name_January)[1,2]+10000), labels = Mun_name_January$NAME_2, cex = 1, col="black", font=2)
  mtext(side = 3, line = 1, "Greenest municipality in January", cex = 1.5, font=2)
  dev.off()
  
  tiff("output/municipalities/municipalities_january.tif")
  plot(NDVI_NL_MASK$January, axes=TRUE)
  plot(Mun_name_January, add = TRUE, col=rgb(0.5,0.5,0.5,0.5))
  text((coordinates(Mun_name_January)[1,1]+50000), (coordinates(Mun_name_January)[1,2]+10000), labels = Mun_name_January$NAME_2, cex = 1, col="black", font=2)
  mtext(side = 3, line = 1, "Greenest municipality in January", cex = 1.5, font=2)
  dev.off()
  
  
  # PLOT AND CREATE MAP OF GREENEST MUNICIPALITY OF WHOLE YEAR, SEE OUTPUT FOLDER
  plot(NDVI_NL_MASK$May, axes=TRUE)
  plot(Mun_name_year, add = TRUE, col=rgb(0.5,0.5,0.5,0.5))
  text((coordinates(Mun_name_year)[1,1]+50000), (coordinates(Mun_name_year)[1,2]+10000), labels = Mun_name_year$NAME_2, cex = 1, col="black", font=2)
  mtext(side = 3, line = 1, "Greenest municipality during whole year", cex = 1.5, font=2)
  
  pdf("output/municipalities/municipalities_year.pdf")
  plot(NDVI_NL_MASK$May, axes=TRUE)
  plot(Mun_name_year, add = TRUE, col=rgb(0.5,0.5,0.5,0.5))
  text((coordinates(Mun_name_year)[1,1]+50000), (coordinates(Mun_name_year)[1,2]+10000), labels = Mun_name_year$NAME_2, cex = 1, col="black", font=2)
  mtext(side = 3, line = 1, "Greenest municipality during whole year", cex = 1.5, font=2)
  dev.off()
  
  tiff("output/municipalities/municipalities_year.tif")
  plot(NDVI_NL_MASK$May, axes=TRUE)
  plot(Mun_name_year, add = TRUE, col=rgb(0.5,0.5,0.5,0.5))
  text((coordinates(Mun_name_year)[1,1]+50000), (coordinates(Mun_name_year)[1,2]+10000), labels = Mun_name_year$NAME_2, cex = 1, col="black", font=2)
  mtext(side = 3, line = 1, "Greenest municipality during whole year", cex = 1.5, font=2)
  dev.off()
}

# END OF SCRIPT