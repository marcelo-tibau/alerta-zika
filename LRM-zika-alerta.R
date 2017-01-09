data_cases <- read.csv("cases_per_neighborhood.csv")

data_weather <- read.csv("dados-climaticos-limpo.csv")

write.csv(newdata, file = "data-cases.csv")



# Models based on all 3 diseases. I coerced the original data into numeric vectors.

newdata <- data.frame(bairro = as.numeric(data_cases$bairro),
                      doenca = as.numeric(data_cases$doenca),
                      dia = as.numeric(data_cases$dia),
                      populacao = as.numeric(data_cases$populacao))

## this
newdata <- data.frame(bairro = as.numeric(data_cases$bairro),
                      doenca = as.numeric(data_cases$doenca),
                      dia = as.numeric(data_cases$dia),
                      populacao = as.numeric(data_cases$populacao))

newdata2 <- data.frame(bairro = newdata$bairro,
                       doenca = count(newdata$doenca),
                       populacao = mean(newdata$populacao))

d <- table(newdata$doenca)

freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
freqData$child <- as.numeric(as.character(freqData$child))
freqData$parent <- as.numeric(as.character(freqData$parent))

sum-data <- newdata %>%
  group_by(bairro) %>%
  summarise(populacao = mean(populacao),
            doenca = table(doenca))
  
sum-data <- newdata %>%
  group_by(bairro) %>%
  doenca = table(doenca)
library(dplyr)
bairro = group_by(newdata$bairro)
doenca = table(newdata$doenca)
## this
avgStats <- newdata %>%
  group_by(bairro) %>%
  summarise(averagepop = mean(populacao),
            averagecases = mean(doenca))


aggregate(cost ~ name + drink, data = bevs, sum)

q <- aggregate(doenca ~ bairro, data = newdata, sum)

avgStats <- newdata %>%
  #table(doenca) %>%
  group_by(bairro) %>%
  summarise(averagepop = mean(populacao))

modelavgStats <- lm(averagepop~averagecases, data = avgStats)

modelavgStats2 <- lm(averagecases~averagepop, data = avgStats)

model.fit <- lm(
  formula = doenca ~ bairro + dia + populacao, data = newdata)

model2 <- lm(bairro~populacao, data = newdata)

model2_1 <- lm(populacao~bairro, data = newdata)

model2_2 <- lm(bairro~populacao+doenca, data = newdata)

model2_3 <- lm(bairro~populacao+doenca+dia, data = newdata)

model2_4 <- lm(populacao~bairro+doenca+dia, data = newdata)

modelfit <- lm(populacao~doenca+dia, data = newdata)

summary(model2_4)$r.squared

summary(coef(model2_4))

str(summary(model2_4))

p <- predict(model2_3)

p2 <- predict(model2_4)

plot(p2)

summary(model2_3)$r.squared

model3 <- lm(bairro~doenca, data = newdata)

model3_1 <- lm(doenca~bairro, data = newdata)

model_4_1 <- lm(dia~doenca, data = newdata)

plot(predict(model2_3,newdata, interval="confidence"))

predictions <- predict(model2,newdata, interval="confidence")

# Models considering only Zika
# To show correlation of models with R-squared, pick the model with the highest limit,
# but the best and easiest way to compare models is to select one with the smaller adjusted R-squared.
# http://www.investopedia.com/ask/answers/012615/whats-difference-between-rsquared-and-adjusted-rsquared.asp


datazika <- subset(data_cases, doenca == "Zika", select = c(bairro, dia, populacao))

newdatazika <- data.frame(bairro = as.numeric(data_cases$bairro),
                          dia = as.numeric(data_cases$dia),
                          populacao = as.numeric(data_cases$populacao))

model.fit.1 <- lm(populacao ~ bairro + dia, data = newdatazika)

model.fit.2 <- lm(populacao ~ bairro, data = newdatazika)

summary(model.fit.1)$r.squared

summary(model.fit.2)