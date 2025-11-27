############################################################
# Opgave 1.5 – Mikroøkonomisk indikator
############################################################

library(dplyr)
library(purrr)
library(tidyr)

## 1) Definér de mikroøkonomiske spørgsmål (pæne navne)
mikro_pretty <- c(
  "Familiens oekonomiske situation i dag, sammenlignet med for et aar siden",
  "Familiens oekonomiske  situation om et aar, sammenlignet med i dag",
  "Familiens oekonomiske situation lige nu: kan spare/penge slaar til/ bruger mere end man tjener",
  "Anskaffelse af stoerre forbrugsgoder, inden for de naeste 12 mdr.",
  "Regner med at kunne spare op i de kommende 12 maaneder"
)

## 2) Match til safe-navne
mikro_safe <- nm$safe[match(mikro_pretty, nm$pretty)]
mikro_safe <- mikro_safe[!is.na(mikro_safe)]  # fjern evt. NA'er

## 3) Byg alle kombinationer af mikro-variabler
K_MAX_mikro <- length(mikro_safe)
alle_komb_mikro <- unlist(lapply(1:K_MAX_mikro, function(k) combn(mikro_safe, k, simplify = FALSE)),
                          recursive = FALSE)

## 4) Beregn R² og adj.R² for alle mikro-kombinationer
r2_mikro <- purrr::map_dfr(alle_komb_mikro, function(vars) {
  tmp <- ftiq %>% select(realvaekst, all_of(vars)) %>% drop_na()
  if (nrow(tmp) < (length(vars) + 3)) {
    return(tibble(k = length(vars), R2 = NA_real_, adjR2 = NA_real_,
                  kombi = paste(nm$pretty[match(vars, nm$safe)], collapse = " | ")))
  }
  s <- summary(lm(reformulate(vars, "realvaekst"), data = tmp))
  tibble(
    k = length(vars),
    R2 = unname(s$r.squared),
    adjR2 = unname(s$adj.r.squared),
    kombi = paste(nm$pretty[match(vars, nm$safe)], collapse = " | ")
  )
}) %>%
  drop_na(R2)

## 5) Find bedste mikro-indikator
best_mikro <- r2_mikro %>% slice_max(adjR2, n = 1, with_ties = FALSE)

cat("\n=== Bedste mikroøkonomiske indikator ===\n")
print(best_mikro)

## 6) Sammenlign med tidligere indikator (fra Opg. 1.3)
cat("\nSammenligning:\n")
cat(sprintf("Bedste mikro-indikator adjR² = %.3f\n", best_mikro$adjR2))
cat(sprintf("Tidligere (Opg.1.3) indikator adjR² = %.3f\n", best_adjR2$adjR2))
cat(sprintf("Forskel = %.3f\n", best_mikro$adjR2 - best_adjR2$adjR2))


################### Her plotter vi, så vi har det visuelt ####################
#############################################
#############################################
library(ggplot2)
library(dplyr)
library(tibble)

# Samme tabel som før
indikator_sammenligning <- tibble(
  Indikator = c("Egen indikator (Opg. 1.3)",
                "Mikro-indikator (Opg. 1.5)",
                "DI’s indikator (Opg. 1.2)"),
  adjR2 = c(0.123, 0.109, 0.44),
  Type = c("Blandet (mikro + makro)", "Kun mikro", "Teoretisk/ekstern")
)

# Plot – nu med labels inde i søjlerne
p_indikator_pretty <- ggplot(indikator_sammenligning,
                             aes(x = reorder(Indikator, adjR2), y = adjR2, fill = Type)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.3f", adjR2)),
            hjust = 1.2, color = "white", size = 4.2, fontface = "bold") +  # inde i søjlen
  coord_flip() +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3")) +
  labs(
    title = "Sammenligning af indikatorernes forklaringsgrad",
    subtitle = "Justeret R² for tre typer indikatorer",
    x = NULL,
    y = "Justeret R²",
    fill = "Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    axis.text.y = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

print(p_indikator_pretty)

# Gem som PNG til rapport
ggsave("indikator_sammenligning_OLA3_pretty.png", p_indikator_pretty,
       width = 8, height = 4.5, dpi = 300)
cat("Gemte: indikator_sammenligning_OLA3_pretty.png\n")

