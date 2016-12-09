dazika <- read.csv("zika_data.csv")

data_cases <- read.csv("cases_per_neighborhood.csv")

dataframe_cases <- data.frame(Bairro = dazika$NM_BAIRRO, Doenca = dazika$NM_DISEASE, Dia = dazika$DT_NOTIFIC, Populacao = dazika$DT_OBITO)

write.csv(dataframe_cases, file = "dataframe_cases.csv")

dataframe_cases2 <- read.csv("dataframe_cases2.csv")


# codes to drop the selected variables
dazika$DT_TRANSRM <- dazika$DT_TRANSRS <- dazika$DT_TRANSSE <- dazika$NU_LOTE_H <- dazika$MIGRADO_W <- NULL

data_cases$bairro <- factor(data_cases$bairro)
data_cases$doenca <- factor(data_cases$doenca)
data_cases$dia <- factor(data_cases$dia)
data_cases$populacao <- factor(data_cases$populacao)

# Modelling regressions:
model1 <- lm(doenca ~ bairro, data = data_cases)
summary(coef(model1))
str(summary(model1))

model2 <- lm(NM_DISEASE ~ DT_TRANSSM, data = dazika)
summary(coef(model2))

model3 <- lm(doenca ~ ., data = data_cases)
summary(coef(model3))
str(summary(model3))

best_model <- step(model3, direction = "both")
m3 <- sparse.model.matrix(lm(NM_DISEASE ~ ., data = dazika))
s <- sparseVector(lm(NM_DISEASE ~ ., data = dazika), length = 10)

summary(best_model)