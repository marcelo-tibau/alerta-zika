library("caret")
library("geosphere")

dazika <- read.csv("zika_data.csv")

df <- data.frame(long = dazika$longitude, lat = dazika$latitude, disease = dazika$NM_DISEASE)

dfzika <- subset(df, disease == "Zika", select = c(long, lat))

geo_dist <- function(dfzika) {
  require(geosphere)
  d <- function(i, z){
    dist <- rep(0, nrow(z))
    dist[i:nrow(z)] <- distHaversine(z[i:nrow(z), 1:2], z[i, 1:2])
    return(dist)
  }
  dm <- do.call(cbind, lapply(1:nrow(dfzika), d, dfzika))
  return(as.dist(dm))
}

distance_matrix <- geo_dist(dfzika)

km <- kmeans(distance_matrix,centers=10) 
