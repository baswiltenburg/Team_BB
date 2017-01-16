# Title:            Geoscripting lesson 6 - Vector calculations
# Teamname:         Team BB
# Team member 1:    Bram Schippers
# Team member 2:    Bas Wiltenburg
# Date:             Januari the 16th, 2017

# install.packages("rgeos")
# install.packages("rgdal")

# Click Source to get final result.

library(rgeos)
library(rgdal)

# Run all the functions from "functions.R" to calculate the NDVI.
source('R/Lesson6_functions.R')

# Results of all separate functions are assigned to variables. 
# Assigning results to variables is necessary to use the results in follow up functions.
industrial_railway_RD <- reproject()

buffer_railway_RD <- buffer(industrial_railway_RD[[1]])

places_names <- intersect(buffer_railway_RD, industrial_railway_RD[[2]])

plot1(buffer_railway_RD, places_names)

# END OF SCRIPT

# City of intersection is Utrecht
# Population of Utrecht is 100000 people