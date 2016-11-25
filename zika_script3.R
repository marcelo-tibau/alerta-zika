dazika <- read.csv2("Disease.csv")

library("caret")
library("geosphere")

# Partitioning
# It took more than one hour to fit with the entire dataset and it didn't work well. Tried with a shorter dataset.

set.seed(14000)
inTrain <- createDataPartition(y=dazika$NM_DISEASE, p=0.9, list=FALSE)
training <- dazika[inTrain, ]
testing <- dazika[-inTrain, ]



# K-mean clustering using geoposition

# First, I will build a dataset from the original with the long, lat, and disease infos.

df <- data.frame(long = testing$longitude, lat = testing$latitude, disease = testing$NM_DISEASE)

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

# Codes to get the k-means 
km <- kmeans(distance_matrix,centers=3) 
