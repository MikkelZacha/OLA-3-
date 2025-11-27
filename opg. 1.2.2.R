#1.2 sammenligning
############################################################
# DI’s forbrugertillidsindikator (DI-FTI) – ALENE DENNE DEL
# Forudsætninger (allerede i dit miljø fra dine scripts):
#   - ftiq       : data.frame med 'realvaekst' + de 12 FT-spørgsmål (safe navne) + 'tid'
#   - nm        : tibble med navneopslag (nm$pretty = pæne navne, nm$safe = safe navne)
#   - spg_safe  : chr-vector med de 12 FT-kolonner (safe navne)
#
# Mål:
#   1) Udpeg DI’s 4 spørgsmål i dine data
#   2) Konstruér DI-FTI som simpelt gennemsnit
#   3) Kør lm(realvaekst ~ DI_FTI) og udskriv R²/adj.R²
############################################################

# (0) Lille hjælpefunktion til R²/adjR² (laver ikke om på noget i dit miljø)
if (!exists("safe_glm_r2")) {
  safe_glm_r2 <- function(formula, data) {
    tryCatch({
      s <- summary(lm(formula, data = data))
      c(R2 = unname(s$r.squared), adjR2 = unname(s$adj.r.squared))
    }, error = function(e) c(R2 = NA_real_, adjR2 = NA_real_))
  }
}

# (1) Angiv DI’s 4 spørgsmål med "pæne navne" (samme stil som i din konsol)
#     Vigtigt: Teksterne skal matche nm$pretty. Justér stavning hvis dine præcise labels afviger.
di_pretty <- c(
  "Familiens oekonomiske situation i dag, sammenlignet med for et aar siden",
  "Danmarks oekonomiske situation i dag, sammenlignet med for et aar siden",
  "Set i lyset af den oekonomiske situation, mener du, at det for oejeblikket er fordelagtigt at anskaffe stoerre forbrugsgoder, eller er det bedre at vente",
  "Anskaffelse af stoerre forbrugsgoder, inden for de naeste 12 mdr."
)

# (2) Map pæne navne -> safe navne via din navneopslagstabel 'nm'
di_safe <- nm$safe[match(di_pretty, nm$pretty)]

# (2a) Safety check: Advar, hvis et spørgsmål ikke blev matchet (så kan du kopiere den præcise tekst fra nm$pretty)
if (anyNA(di_safe)) {
  mis <- di_pretty[is.na(di_safe)]
  warning("Fandt ikke disse DI-spørgsmål i nm$pretty (tjek stavning/tegn):\n - ", paste(mis, collapse = "\n - "))
}

# (3) Konstruér DI-FTI som simpelt gennemsnit af de 4 spørgsmål
#     (DI bruger i praksis lige vægte; vend evt. fortegn på 'kontra'-spørgsmål hvis opsætning kræver det)
ftiq <- ftiq %>%
  mutate(
    # Hvis du VED at nogle af de fire skal vendes, kan du gøre det her med across(all_of(...), ~ -.x)
    DI_FTI = rowMeans(across(all_of(di_safe)), na.rm = TRUE)
  )

# (4) Kør regression: realvaekst ~ DI_FTI (fjern NA først)
#     Vi laver INGEN ændringer i din periode – bruger præcist de rækker, hvor begge variable findes.
tmp_di <- ftiq %>% select(realvaekst, DI_FTI) %>% tidyr::drop_na()

# (5) Udtræk R² og justeret R²
di_r2_vals <- safe_glm_r2(realvaekst ~ DI_FTI, tmp_di)

# (6) Udskriver et kort, rapportklart resumé
cat("\n================ DI’s forbrugertillidsindikator ================\n")
cat("Spørgsmål (pæne navne):\n")
for (i in seq_along(di_pretty)) cat(sprintf("  %d) %s\n", i, di_pretty[i]))
cat("\nFormel: realvaekst ~ DI_FTI  (DI_FTI = gennemsnit af de 4 spørgsmål)\n")
cat(sprintf("Observationer i regressionen: %d\n", nrow(tmp_di)))
cat(sprintf("R²        : %.3f\n", di_r2_vals[['R2']]))
cat(sprintf("Justeret R²: %.3f\n", di_r2_vals[['adjR2']]))
cat("===============================================================\n")


