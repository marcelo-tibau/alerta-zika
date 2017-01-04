data_cases <- read.csv("cases_per_neighborhood.csv")

newdata <- data.frame(bairro = as.numeric(data_cases$bairro),
                      doenca = as.numeric(data_cases$doenca),
                      dia = as.numeric(data_cases$dia),
                      populacao = as.numeric(data_cases$populacao))

model.fit <- lm(
  formula = doenca ~ bairro + dia + populacao, data = newdata)

model2 <- lm(bairro~populacao, data = newdata)

model2_1 <- lm(populacao~bairro, data = newdata)

model2_2 <- lm(bairro~populacao+doenca, data = newdata)

model2_3 <- lm(bairro~populacao+doenca+dia, data = newdata)

model2_4 <- lm(populacao~bairro+doenca+dia, data = newdata)

p <- predict(model2_3)

p2 <- predict(model2_4)

plot(p2)

summary(model2_3)$r.squared

model3 <- lm(bairro~doenca, data = newdata)

model3_1 <- lm(doenca~bairro, data = newdata)

model_4_1 <- lm(dia~doenca, data = newdata)

plot(predict(model2_3,newdata, interval="confidence"))

predictions <- predict(model2,newdata, interval="confidence")

