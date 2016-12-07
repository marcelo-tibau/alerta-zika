dazika <- read.csv("zika_data.csv")

dazika$ID_AGRAVO <- factor(dazika$ID_AGRAVO)
dazika$NM_DISEASE <- factor(dazika$NM_DISEASE)
dazika$DT_TRANSSM <- factor(dazika$DT_TRANSSM)

model1 <- lm(NM_DISEASE ~ ID_AGRAVO, data = dazika)
summary(coef(model1))

model2 <- lm(NM_DISEASE ~ DT_TRANSSM, data = dazika)
summary(coef(model2))