############################################################
# Opgave 1.4 – Scenarie-forecast for 2025K3 og 2025K4
# Mulighed B: Brug seneste kvartal + simple antagelser
#
# Idé:
#  - Vi tager seneste observerede kvartal (typisk 2025K2)
#  - Vi "skubber" FT-spørgsmålene med nogle få point (scenarieantagelser)
#  - Vi bruger mod_best til at forudsige realvækst for 2025K3 og 2025K4
#  - (Valgfrit) Hvis mod_di & DI_FTI findes, laver vi også DI-baserede forecasts
############################################################

library(dplyr)
library(tidyr)
library(tibble)
library(readr)

## 0) Safety: tjekker at de nødvendige objekter findes
stopifnot(exists("ftiq"), exists("best_safe_vec"), exists("mod_best"))

## 1) Finder seneste kvartal i vores data 
seneste_kvartal <- max(ftiq$tid)
cat("Seneste kvartal i datasættet: ", seneste_kvartal, "\n")

# Vi forventer at seneste_kvartal == "2025K2".
# Hvis det ikke er det, kører koden stadig – vi bruger bare det, der faktisk er senest.

## 2) Henter FT-værdierne for vindermodellens variabler i det seneste kvartal
#    (Det er det "baseline"-niveau, vi justerer ud fra)
baseline_row <- ftiq %>%
  filter(tid == seneste_kvartal) %>%
  select(all_of(best_safe_vec)) %>%
  # Forsikre numerik
  mutate(across(everything(), ~ as.numeric(as.character(.x))))

if (nrow(baseline_row) != 1) {
  stop("Forventede præcis 1 række for baseline-kvartalet. Tjek dine data/filtre.")
}

## 3) Definér SIMPLE SCENARIER for K3 og K4
#    A) En kort recession/afdæmpning i K3: -2 point på alle 5 FT-variabler
#    B) En lille bedring i K4: +1 point på alle 5 FT-variabler
#    -> Du kan ændre disse tal efter behov.
delta_K3 <- -2   # anvendes på alle variabler i best_safe_vec
delta_K4 <- +1   # anvendes på alle variabler i best_safe_vec

## 4) Byg "fremtidsrækker" for 2025K3 og 2025K4 ud fra baseline + deltas
future_inputs <- bind_rows(
  baseline_row %>% mutate(across(everything(), ~ .x + delta_K3)) %>% mutate(tid = "2025K3"),
  baseline_row %>% mutate(across(everything(), ~ .x + delta_K4)) %>% mutate(tid = "2025K4")
) %>%
  relocate(tid, .before = 1)

## 5) Laver forudsigelser med vores bedste model (mod_best)
#    mod_best er estimeret som: lm(realvaekst ~ <best_safe_vec> , data=...)
future_inputs$pred_realvaekst_egen <- predict(mod_best, newdata = future_inputs)

## 6) Forudsigelser med DI’s indikator – hvis muligt
#    Vi håndterer to situationer:
#    6a) Hvis både mod_di OG DI_FTI findes i dit miljø (fra din DI-kode):
#        - Vi laver et simpelt DI-scenarie: DI_FTI_K3 = DI_FTI_seneste + delta_DI_K3 (fx -2)
#                                            DI_FTI_K4 = DI_FTI_seneste + delta_DI_K4 (fx +1)
#    6b) Ellers springer vi DI-forecastet over, uden fejl.
har_mod_di <- exists("mod_di")
har_DI_kol <- "DI_FTI" %in% names(ftiq)

if (har_mod_di && har_DI_kol) {
  # Hent seneste DI_FTI
  di_seneste <- ftiq %>%
    filter(tid == seneste_kvartal) %>%
    select(DI_FTI)
  
  if (nrow(di_seneste) == 1) {
    delta_DI_K3 <- -2   # kan ændres uafhængigt af FT-variablerne
    delta_DI_K4 <- +1
    
    di_future <- tibble(
      tid    = c("2025K3", "2025K4"),
      DI_FTI = c(di_seneste$DI_FTI + delta_DI_K3,
                 di_seneste$DI_FTI + delta_DI_K4)
    )
    
    di_future$pred_realvaekst_DI <- predict(mod_di, newdata = di_future)
    
    # Merge DI-forecast ind i future_inputs for samlet oversigt
    future_inputs <- future_inputs %>%
      left_join(di_future %>% select(tid, pred_realvaekst_DI), by = "tid")
  } else {
    cat("DI-forecast: fandt ikke entydig DI_FTI-værdi for baseline-kvartal – springer DI over.\n")
  }
} else {
  cat("DI-forecast: mod_di/DI_FTI ikke tilgængelig – springer DI over.\n")
}

## 7) Udskriv et kort, rapportklart resumé
out <- future_inputs %>%
  select(tid, starts_with("pred_realvaekst")) %>%
  arrange(tid)

cat("\n================ Forudsigelser (scenarie) ================\n")
print(out, n = nrow(out))
cat("==========================================================\n\n")


## 9) Ekstra: laver en lille tabel med antagelserne
assumptions <- tibble(
  variabel = best_safe_vec,
  delta_K3 = delta_K3,
  delta_K4 = delta_K4
)
readr::write_csv(assumptions, "forecast_antagelser.csv")
cat("Gemte: forecast_antagelser.csv (overblik over antagelser)\n")

