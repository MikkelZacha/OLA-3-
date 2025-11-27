####### Opg. 2.1 Lav PCA ###############

library(dplyr)

str(ftiq)

# Y-variabel
y <- ftiq$realvaekst

# X-variabler = de 12 spørgsmål i forbrugerforventningen
X <- ftiq %>%
  select(
    Anser.det.som.fornuftigt.at.spare.op.i.den.nuvaerende.oekonomiske.situation,
    Anskaffelse.af.stoerre.forbrugsgoder..fordelagtigt.for.oejeblikket,
    Anskaffelse.af.stoerre.forbrugsgoder..inden.for.de.naeste.12.mdr.,
    Arbejdsloesheden.om.et.aar..sammenlignet.med.i.dag,
    Danmarks.oekonomiske.situation.i.dag..sammenlignet.med.for.et.aar.siden,
    Danmarks.oekonomiske.situation.om.et.aar..sammenlignet.med.i.dag,
    Familiens.oekonomiske..situation.om.et.aar..sammenlignet.med.i.dag,
    Familiens.oekonomiske.situation.i.dag..sammenlignet.med.for.et.aar.siden,
    Familiens.oekonomiske.situation.lige.nu..kan.spare.penge.slaar.til..bruger.mere.end.man.tjener,
    Priser.i.dag..sammenlignet.med.for.et.aar.siden,
    Priser.om.et.aar..sammenlignet.med.i.dag,
    Regner.med.at.kunne.spare.op.i.de.kommende.12.maaneder
  )

# prcomp laver PCA; scale. = TRUE standardiserer
pca_mod <- prcomp(X, scale. = TRUE)

# se hvor meget hver komponent forklarer
summary(pca_mod)

pca_scores <- as.data.frame(pca_mod$x)
head(pca_scores)

# her tager vi de 4 første
regdata <- cbind(
  realvaekst = y,
  pca_scores[, 1:4]  # PC1:PC4
)

pca_lm <- lm(realvaekst ~ PC1 + PC2 + PC3 + PC4, data = regdata)
summary(pca_lm)

par(mfrow = c(2, 2))
plot(pca_lm)

plot(regdata$PC1, regdata$realvaekst,
     xlab = "PC1 (forbrugerforventninger)",
     ylab = "Realvækst i forbrug (%)")
abline(lm(realvaekst ~ PC1, data = regdata), col = "red")

pca_mod$rotation[,1]

summary(pca_mod)
