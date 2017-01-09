data_cases <- read.csv("cases_per_neighborhood.csv")

newdata <- data.frame(bairro = data_cases$bairro,
                      doenca = data_cases$doenca,
                      dia = as.numeric(data_cases$dia),
                      populacao = as.numeric(data_cases$populacao))

zika <- subset(newdata, doenca == "Zika", select = c(bairro, populacao, doenca))

dengue <- subset(newdata, doenca == "Dengue", select = c(bairro, populacao, doenca))

chikungunya <- subset(newdata, doenca == "Chikungunya", select = c(bairro, populacao, doenca)) 

zika$doenca <- as.integer(zika$doenca)

dengue$doenca <- as.integer(dengue$doenca)

chikungunya$doenca <- as.integer(chikungunya$doenca)

countzika <- aggregate(doenca ~ bairro, data = zika, sum)

countdengue <- aggregate(doenca ~ bairro, data = dengue, sum)

countchikua <- aggregate(doenca ~ bairro, data = chikungunya, sum)


zika1 <- zika %>%
  group_by(bairro) %>%
  summarise(averagepop = mean(populacao))

zika <- data.frame(bairro = zika1$bairro, casos_zika = countzika$doenca, populacao = zika1$averagepop)

dengue1 <- dengue %>%
  group_by(bairro) %>%
  summarise(averagepop = mean(populacao))

dengue <- data.frame(bairro = dengue1$bairro, casos_dengue = countdengue$doenca, populacao = dengue1$averagepop)

chikungunya1 <- chikungunya %>%
  group_by(bairro) %>%
  summarise(averagepop = mean(populacao))

chikungunya <- data.frame(bairro = chikungunya1$bairro, casos_chikungunya = countchikua$doenca, populacao = chikungunya1$averagepop)

modelzika <- lm(populacao~casos_zika, data = zika)

modeldengue <- lm(populacao~casos_dengue, data = dengue)

modelchiku <- lm(populacao~casos_chikungunya, data = chikungunya)



list(countchikua$bairro)  



rj <- data.frame(bairro = zika1$bairro, casos_zika = countzika$doenca, casos_dengue = countdengue$doenca, populacao = zika1$averagepop)

d <- rbind_all(countchikua, countdengue, countzika, fill=TRUE)

