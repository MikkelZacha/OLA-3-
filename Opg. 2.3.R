########### Opg. 2.3 - 

library(dplyr)
library(tidyr)

# === 1) Klargør data for 3. kvartal 2025 (måned → kvartal) ===

k3fti <- Q3_Ftillid   # kopi af df med 12 spørgsmål og 2025M08, 2025M09, 2025M10

# gør de tre månedskolonner numeriske (de kom ind som tekst med komma)
k3fti[, c("2025M08", "2025M09", "2025M10")] <-
  lapply(k3fti[, c("2025M08", "2025M09", "2025M10")],
         function(x) as.numeric(gsub(",", ".", x)))

# kvartalsgennemsnit for hvert af de 12 spørgsmål
k3fti$K3_2025 <- rowMeans(k3fti[, c("2025M08", "2025M09", "2025M10")], na.rm = TRUE)

# spørgsmålsnavnene sidder i første kolonne (fx ...1) – giv den et ordentligt navn
k3fti <- k3fti %>%
  rename(variabel = ...1)


# === 2) Lang → bred: én række med de 12 spørgsmål ===

k3fti_wide <- k3fti %>%
  select(variabel, K3_2025) %>%         # vi skal kun bruge navnet + kvartalstallet
  pivot_wider(
    names_from  = variabel,            # kolonnenavn = spørgsmålets navn
    values_from = K3_2025              # værdien = K3_2025
  )

# nu har vi 1 række og 12 kolonner (plus vi tilføjer selv nedenfor)


# === 3) Fix det lange/grimme kolonnenavn så det matcher ftiq ===

k3fti_wide <- k3fti_wide %>%
  rename(
    "Familiens.oekonomiske.situation.lige.nu..kan.spare.penge.slaar.til..bruger.mere.end.man.tjener" =
      `Familiens.oekonomiske.situation.lige.nu:.kan.spare/penge.slaar.til/.bruger.mere.end.man.tjener`
  )


# === 4) Tilføj tid + indikator + tomme kolonner ===

# tid for observationen
k3fti_wide$tid <- "2025K3"

# de 12 spørgsmål i den rækkefølge vi bruger dem i modellen
indk3 <- c(
  "Anser.det.som.fornuftigt.at.spare.op.i.den.nuvaerende.oekonomiske.situation",
  "Anskaffelse.af.stoerre.forbrugsgoder..fordelagtigt.for.oejeblikket",
  "Anskaffelse.af.stoerre.forbrugsgoder..inden.for.de.naeste.12.mdr.",
  "Arbejdsloesheden.om.et.aar..sammenlignet.med.i.dag",
  "Danmarks.oekonomiske.situation.i.dag..sammenlignet.med.for.et.aar.siden",
  "Danmarks.oekonomiske.situation.om.et.aar..sammenlignet.med.i.dag",
  "Familiens.oekonomiske..situation.om.et.aar..sammenlignet.med.i.dag",
  "Familiens.oekonomiske.situation.i.dag..sammenlignet.med.for.et.aar.siden",
  "Familiens.oekonomiske.situation.lige.nu..kan.spare.penge.slaar.til..bruger.mere.end.man.tjener",
  "Priser.i.dag..sammenlignet.med.for.et.aar.siden",
  "Priser.om.et.aar..sammenlignet.med.i.dag",
  "Regner.med.at.kunne.spare.op.i.de.kommende.12.maaneder"
)

# indikator = gennemsnit af de 12 spørgsmål for 2025K3
# (brug any_of så den ikke fejler hvis ét navn mangler)
k3fti_wide$indikator <- rowMeans(k3fti_wide[, intersect(indk3, names(k3fti_wide))], na.rm = TRUE)

# de to sidste kolonner findes ikke endnu – vi sætter dem til NA
k3fti_wide$forbrug    <- NA
k3fti_wide$realvaekst <- NA

# sæt kolonne-rækkefølgen:
# tid → 12 spørgsmål → indikator → forbrug → realvaekst
k3fti_wide <- k3fti_wide %>%
  select(
    tid,
    all_of(indk3),
    indikator,
    forbrug,
    realvaekst
  )


# === 5) Merge ind i den store kvartals-df (ftiq) ===

# nu har k3fti_wide nøjagtig samme kolonner som ftiq, så vi kan binde
ftiq <- dplyr::bind_rows(ftiq, k3fti_wide)

############ FORUDSIGELSE ##################

######### TJEK 
ftiq %>%
  filter(tid == "2025K3") %>%
  select(all_of(indk3))


# === 1) Beregn PCA-scorer for hele datasættet (inkl. 2025K3) ===

library(dplyr)

# 1) spørgsmålet vi ikke bruger
drop_spg <- "Anser.det.som.fornuftigt.at.spare.op.i.den.nuvaerende.oekonomiske.situation"

# 2) de 11 vi faktisk bruger
indk11 <- setdiff(indk3, drop_spg)

# 3) lav datasættet uden den kolonne
ftiq_vaegt <- ftiq %>%
  select(-all_of(drop_spg))

# 4) historik = dem hvor vi har realvækst (altså alle undtagen 2025K3)
hist_data <- ftiq_vaegt %>%
  filter(!is.na(realvaekst))

# 5) PCA på historikken (11 spørgsmål)
pca_11 <- prcomp(
  scale(hist_data[, indk11]),
  center = TRUE,
  scale. = TRUE
)

# 6) lav PCA-scorer for hele datasættet (inkl. 2025K3!)
#    -> her skal vi bruge pca_11$center og pca_11$scale, ikke bare scale()
pca_all <- ftiq_vaegt[, indk11]

pca_all_vaegt <- sweep(pca_all, 2, pca_11$center, FUN = "-")
pca_all_vaegt <- sweep(pca_all_vaegt, 2, pca_11$scale,  FUN = "/")

pca_all <- as.data.frame(predict(pca_11, newdata = pca_all_vaegt))

# 7) lav regression på historiske data
regdata <- cbind(
  realvaekst = hist_data$realvaekst,
  pca_all[!is.na(ftiq_vaegt$realvaekst), 1:4]  # samme rækker som hist_data
)

pca_lm_11 <- lm(realvaekst ~ PC1 + PC2 + PC3 + PC4, data = regdata)

# 8) forudsig for HELE datasættet (inkl. 2025K3)
ftiq_vaegt$realvaekst_pred <- predict(pca_lm_11, newdata = pca_all)

# 9) læg den forudsagte værdi ind i realvaekst for 2025K3
ftiq_vaegt$realvaekst[ftiq_vaegt$tid == "2025K3"] <-
  ftiq_vaegt$realvaekst_pred[ftiq_vaegt$tid == "2025K3"]

library(ggplot2)
library(dplyr)

# Udvælg kun de seneste 10 år ud fra årstallet i 'tid'
vaekst_df_10 <- ftiq_vaegt %>%
  mutate(
    år = as.numeric(substr(tid, 1, 4)),   # udtræk år som tal
    type = ifelse(tid == "2025K3", "Forudsagt", "Observeret")
  ) %>%
  filter(år >= max(år, na.rm = TRUE) - 9)   # behold kun de sidste 10 år

# Plot som før
ggplot(vaekst_df_10, aes(x = tid, y = realvaekst, group = 1)) +
  geom_line(color = "gray70", linewidth = 0.9) +
  geom_point(data = subset(vaekst_df_10, type == "Observeret"),
             color = "gray25", size = 2) +
  geom_point(data = subset(vaekst_df_10, type == "Forudsagt"),
             color = "#D1495B", size = 3.5) +
  geom_label(
    data = subset(vaekst_df_10, type == "Forudsagt"),
    aes(label = paste0(round(realvaekst, 2), " %")),
    vjust = -0.7,
    fill = "white",
    color = "#D1495B",
    label.size = 0,
    fontface = "bold"
  ) +
  scale_x_discrete(
    breaks = vaekst_df_10$tid[grepl("K1", vaekst_df_10$tid)],
    labels = substr(vaekst_df_10$tid[grepl("K1", vaekst_df_10$tid)], 1, 4)
  ) +
  labs(
    title = "Realvækst i husholdningernes forbrug",
    subtitle = "Seneste 10 år + forudsigelse for 2025K3 (PCA-regression)",
    x = NULL,
    y = "Årlig realvækst (%)"
  ) +
  expand_limits(y = max(vaekst_df_10$realvaekst, na.rm = TRUE) + 0.5) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank()
  )
