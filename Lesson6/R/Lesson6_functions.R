
# Function to exucute pre-processing steps
# Creates folders if not already exist 
# Only downloads the data if not already downloaded previously 
preprocessing <- function() {
  destFile <- "data"
  if (!dir.exists(file.path(destFile))) {
    dir.create("data")
  }
  if (!dir.exists(file.path("data/places"))) {
    dir.create("data/places")
  }
  if (!dir.exists(file.path("data/railways"))) {
    dir.create("data/railways")
  }
  if (!file.exists(file.path("data/places/places.zip"))){
    download.file("http://www.mapcruzin.com/download-shapefile/netherlands-places-shape.zip", destfile = "data/places/places.zip", quiet = TRUE)
  }
  if (!file.exists(file.path("data/railways/railways.zip"))){
    download.file("http://www.mapcruzin.com/download-shapefile/netherlands-railways-shape.zip", destfile = "data/railways/railways.zip", quiet = TRUE)
  }

  unzip("data/places/places.zip", exdir = "data/places")
  unzip("data/railways/railways.zip", exdir = "data/railways")
}


# Function to select industrial railways
# Function to reproject railway and places to dutch RD New
reproject <- function(){
  read_railway <- readOGR("data/railways", "railways")
  industrial_railway <- read_railway[read_railway$type=="industrial",]
  read_places <- readOGR("data/places", "places")
  prj_string_RD <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +
                     k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel 
                     +towgs84=565.2369,50.0087,465.658,-0.406857330322398,
                     0.350732676542563,-1.8703473836068,4.0812 +units=m +no_defs")
  industrial_railway_RD <- spTransform(industrial_railway, prj_string_RD)
  places_RD <- spTransform(read_places, prj_string_RD)
  result <- c(industrial_railway_RD, places_RD)
  return(result)
}

# Function that buffers the industrial railways
buffer <- function(buffer_object){
  buffer_railway_RD <- gBuffer(buffer_object, width = 1000, quadsegs=8)
  return(buffer_railway_RD)
}

# Function that intersect the buffer with all places in vector-dataset
intersect <- function(buffer_railway_RD, places_RD){
  railway_places <- gIntersects(buffer_railway_RD, places_RD, byid=TRUE)
  places_names <- subset(places_RD, railway_places[, TRUE])
  return(places_names)
}

# FUnction to plot the buffer with intersected places
plot1 <- function(buffer_railway_RD, places_names){
  plot2 <- plot(buffer_railway_RD, axes =TRUE, col = "grey")
  plot3 <- plot(places_names, pch=19, cex=2, col="red", add = TRUE)
  plot4 <- mtext(side = 3, line = 1, "Places withing 1000 meters of industrial railway", cex = 1.5, font=2, add=TRUE)
  plot5 <- text((places_names@coords[1])+200,(places_names@coords[2])+150, labels = places_names$name, cex = 1, col="black", font=2)
  plot6 <- box()
}


