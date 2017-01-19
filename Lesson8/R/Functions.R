preprocessing <- function() {
  destFile <- "data"
  if (!dir.exists(file.path(destFile))) {
    dir.create("data")
  }
  if (!dir.exists(file.path("output"))) {
    dir.create("output")
  }
  if (!file.exists(file.path("data/data.zip"))){
    download.file("https://github.com/GeoScripting-WUR/AdvancedRasterAnalysis/archive/gh-pages.zip", destfile = "data/data.zip", quiet = TRUE)
  }
  unzip("data/data.zip", exdir = "data")
}
#preprocessing()


scatterplots <- function(){
  vcfGewata[vcfGewata > 100] <- NA
  GewataB1[GewataB1 > 800] <- NA
  GewataB2[GewataB2 > 1000] <- NA
  GewataB3[GewataB3 > 1200] <- NA
  GewataB4[GewataB4 > 3800] <- NA
  GewataB4[GewataB4 < 1200] <- NA
  GewataB5[GewataB5 > 4500] <- NA
  GewataB7[GewataB7 > 2300] <- NA
  GewataB7[GewataB7 < 200] <- NA
  Gewata <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7, vcfGewata)
  names(Gewata) <- c("band1", "band2", "band3", "band4", "band5", "band7", "VCF")
  df <- as.data.frame(getValues(Gewata))
  png("output/scatterplots.png")
  par(mfrow = c(3, 2))
  plot(Gewata$band1, Gewata$VCF, main="Scatterplot band 1", xlab="Band 1", ylab="VCF", pch=1, col = "darkblue")
  plot(Gewata$band2, Gewata$VCF, main="Scatterplot band 2", xlab="Band 2", ylab="VCF", pch=1, col = "darkblue")
  plot(Gewata$band3, Gewata$VCF, main="Scatterplot band 3", xlab="Band 3", ylab="VCF", pch=1, col = "darkblue")
  plot(Gewata$band4, Gewata$VCF, main="Scatterplot band 4", xlab="Band 4", ylab="VCF", pch=1, col = "darkblue")
  plot(Gewata$band5, Gewata$VCF, main="Scatterplot band 5", xlab="Band 5", ylab="VCF", pch=1, col = "darkblue")
  plot(Gewata$band7, Gewata$VCF, main="Scatterplot band 7", xlab="Band 7", ylab="VCF", pch=1, col = "darkblue")
  dev.off()
  result = list(df, Gewata)
  return(result)
}
#result <- scatterplots()
#dataframe <- result[[1]]
#Gewata <- result[[2]]


estimate_VCF <- function(dataframe){
  vcf_model <- lm(VCF ~ band1 + band2 + band3 + band4 + band5 + band7, data=dataframe)
  cols <- c("orange", "dark green", "light blue")
  vcf_predict <- predict(Gewata, model=vcf_model, na.rm=TRUE, col = cols)
  vcf_predict[vcf_predict < 0] <- NA
  vcf_predict[vcf_predict > 100] <- NA
  values_vcf_predict <- getValues(vcf_predict)
  dataframe$predicted_VCF <- values_vcf_predict
  result <- list(vcf_predict, values_vcf_predict, vcf_model)
  return(result)
}
#result <- estimate_VCF(dataframe)
#vcf_model <- result[[3]]
#vcf_predict <- result[[1]]
#values_vcf_predict <- result[[2]]


rmse_general <- function(predict, real){
  vcf_predict_DF <- as.data.frame(predict)
  names(vcf_predict_DF) <- c("predicted_vcf")
  Gewata_VSF_DF <- as.data.frame(real)
  names(Gewata_VSF_DF) <- c("real_vcf")
  rmse_general <- sqrt(mean((vcf_predict_DF$predicted_vcf - Gewata_VSF_DF$real_vcf)^2 , na.rm = TRUE))
  return(rmse_general)
}
#rmse_general <- rmse_general(vcf_predict,Gewata$VCF)


rmse_per_class <- function(predict, real){
  trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
  classes <- rasterize(trainingPoly, predict, field='Code')
  cols <- c("orange", "dark green", "light blue")
  # plot(classes, col=cols, legend=FALSE)
  names(predict) <- "vcf_predicted"
  difference_VCF <- predict - real
  power2_difference_VCF <- difference_VCF^2
  zonal_mean <- zonal(power2_difference_VCF, classes, fun="mean", na.rm=TRUE)
  zonal_mean_DF <- as.data.frame(zonal_mean)
  zonal_rmse<- sqrt(zonal_mean)
  zonal_rmse_DF <- as.data.frame(zonal_rmse)
  zonal_rmse_DF$zone <- zonal_mean_DF$zone
  names(zonal_rmse_DF) <- c("class", "RMSE")
  zonal_rmse_DF$class_name <- unique(trainingPoly@data$Class)
  return(zonal_rmse_DF)
}
#rmse_per_class <- rmse_per_class(vcf_predict, Gewata$VCF)



