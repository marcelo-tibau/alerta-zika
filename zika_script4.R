library("caret")
library("geosphere")

dazika <- read.csv("zika_data.csv")

df <- data.frame(long = dazika$longitude, lat = dazika$latitude, disease = dazika$NM_DISEASE)

dfzika <- subset(df, disease == "Zika", select = c(long, lat))

dweather <- read.csv("2016-11-07_estacao_wu.csv")

weather_data <- read.csv("2016-11-07_clima_wu.csv")
weather_RJ <- subset(weather_data, Estacao_wu_estacao_id == c("SBGL", "SBRJ"), select = c(data_dia, temp_min, temp_max, temp_med))

write.csv(weather_RJ, file = "rj_weather.csv")

dwrj <- read.csv("rj_weather_2016.csv")

dwrj_2016 <- subset(dwrj, select = c(data_dia, temp_min, temp_max, temp_med))

write.csv(dwrj_2016, file = "rj_weather_2016.csv")

dat1 <- read.csv("rj_weather_2016.csv")

dat2 <- read.csv("rj_weather_2015.csv")

dezembro_2015 <- subset(dat2, Month == "Dezembro", select = c(data_dia, temp_min, temp_max, temp_med))

write.csv(dezembro_2015, file = "dezembro_2015.csv")

idh_rio_reg <- read.csv("RM-63300-Rio-de-Janeiro-Base-REGIONAL-2000_2010.csv")

idh_rio_reg_var <- subset(idh_rio_reg, select = c(NOME_REG, ANO, ESPVIDA, IDHM, IDHM_E, IDHM_L, IDHM_R))

write.csv(idh_rio_reg_var, file = "idh_rio_reg_var.csv")

idh_data <- subset(idh_rio_reg_var, ANO == 2010, select = c(NOME_REG, ANO, ESPVIDA, IDHM, IDHM_E, IDHM_L, IDHM_R))

write.csv(idh_data, file = "idh_data.csv")

idh_rio_coord <- read.csv("idh_excel.csv")

idh_rio_coord_data <- subset(idh_rio_coord, select = c(NOME_REG, IDHM, LONGITUDE, LATITUDE))

write.csv(idh_rio_coord_data, file = "idh_rio_coord_data.csv")


fit <- lm(NM_DISEASE ~ ID_AGRAVO + DT_TRANSSM, data = dazika)
abline(fit, pch = 16, cex = 1.3, col = "blue", main = "LINEAR REGRESSION ")

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
