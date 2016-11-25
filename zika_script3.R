dazika <- read.csv2("Disease.csv")

library("caret")
library("gbm")
library("rpart")
library("rpart.plot")
library("RColorBrewer")
library("NbClust")
library("geosphere")



# K-mean clustering using geoposition

# First, I will build a dataset from the original (dazika) with the long, lat, and disease infos.

df <- data.frame(long = dazika$longitude, lat = dazika$latitude, disease = dazika$NM_DISEASE)

# Then, I will build a distance function using the distance algorithm distHaversine() from the geosphere package.

geo_dist <- function(df) {
  require(geosphere)
  d <- function(i, z){
    dist <- rep(0, nrow(z))
    dist[i:nrow(z)] <- distHaversine(z[i:nrow(z), 1:2], z[i, 1:2])
    return(dist)
  }
  dm <- do.call(cbind, lapply(1:nrow(df), d, df))
  return(as.dist(dm))
}

# Next step is to build a distance matrix

distance_matrix <- geo_dist(df)

# It took more than one hour to fit and it didn't work well. Try with a shorter dataset.
