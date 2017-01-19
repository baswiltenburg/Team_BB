# Title:            Geoscripting lesson 8 
# Teamname:         Team BB
# Team member 1:    Bram Schippers
# Team member 2:    Bas Wiltenburg
# Date:             Januari the 19th, 2017

# install.packages("rgeos")
# install.packages("rgdal")
# install.packages("raster")

# Click Source to get final result.

library(rgeos)
library(rgdal)
library(raster)
library(sp)

# Run all the functions from "functions.R" to calculate the NDVI.
source('R/Functions.R')

# Run the function "preprocessing" to be able to load all unpacked files. 
preprocessing()
load("data/AdvancedRasterAnalysis-gh-pages/data/GewataB2.rda")
load("data/AdvancedRasterAnalysis-gh-pages/data/GewataB3.rda")
load("data/AdvancedRasterAnalysis-gh-pages/data/GewataB4.rda")
load("data/AdvancedRasterAnalysis-gh-pages/data/GewataB1.rda")
load("data/AdvancedRasterAnalysis-gh-pages/data/GewataB5.rda")
load("data/AdvancedRasterAnalysis-gh-pages/data/GewataB7.rda")
load("data/AdvancedRasterAnalysis-gh-pages/data/vcfGewata.rda")
load("data/AdvancedRasterAnalysis-gh-pages/data/trainingPoly.rda")

# Results of all separate functions are assigned to variables. 
# Assigning results to variables is necessary to use the results in follow up functions.
result <- scatterplots()
dataframe <- result[[1]]
Gewata <- result[[2]]
result <- estimate_VCF(dataframe)
vcf_model <- result[[3]]
vcf_predict <- result[[1]]
values_vcf_predict <- result[[2]]
rmse_general1 <- rmse_general(vcf_predict,Gewata$VCF)
rmse_per_class1 <- rmse_per_class(vcf_predict, Gewata$VCF)



# END OF SCRIPT

