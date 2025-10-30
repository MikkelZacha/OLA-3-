################# Opg. 1.2: Beregn R² for alle kombinationer #################

r2_resultater <- purrr::map_dfr(alle_kombinationer, function(vars) {
  tmp <- ftiq %>%
    select(realvaekst, all_of(vars)) %>%
    drop_na()
  
  if (nrow(tmp) < 5) {
    return(tibble(
      stoerrelse  = length(vars),
      kombi_tekst = paste(nm$pretty[match(vars, nm$safe)], collapse = " | "),
      R2          = NA_real_,
      adjR2       = NA_real_
    ))
  }
  
  s <- summary(lm(reformulate(vars, response = "realvaekst"), data = tmp))
  tibble(
    stoerrelse  = length(vars),
    kombi_tekst = paste(nm$pretty[match(vars, nm$safe)], collapse = " | "),
    R2          = unname(s$r.squared),
    adjR2       = unname(s$adj.r.squared)
  )
})

r2_resultater <- r2_resultater %>% drop_na(R2)

best_R2    <- r2_resultater %>% slice_max(R2, n = 1, with_ties = FALSE)
best_adjR2 <- r2_resultater %>% slice_max(adjR2, n = 1, with_ties = FALSE)

cat("\n— Bedste kombination (R²) —\n"); print(best_R2)
cat("\n— Bedste kombination (adjR²) —\n"); print(best_adjR2)

################# Opg. 1.5: Bedste ENKELT-indikator #################

r2_enkelt <- purrr::map_dfr(spg_safe, function(var) {
  tmp <- ftiq %>%
    select(realvaekst, all_of(var)) %>%
    drop_na()
  
  if (nrow(tmp) < 5) {
    return(tibble(
      indikator = nm$pretty[match(var, nm$safe)],
      R2        = NA_real_,
      adjR2     = NA_real_
    ))
  }
  
  s <- summary(lm(reformulate(var, response = "realvaekst"), data = tmp))
  tibble(
    indikator = nm$pretty[match(var, nm$safe)],
    R2        = unname(s$r.squared),
    adjR2     = unname(s$adj.r.squared)
  )
}) %>%
  drop_na(R2)

bedste_enkelt <- r2_enkelt %>% slice_max(R2, n = 1)

cat("\n=== Bedste enkelt-indikator (én variabel) ===\n")
print(bedste_enkelt)

################# (Valgfrit) Plot #################

r2_resultater %>%
  group_by(stoerrelse) %>%
  summarise(mean_adjR2 = mean(adjR2, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = stoerrelse, y = mean_adjR2)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.8) +
  labs(title = "Gennemsnitlig justeret R² pr. kombinationsstørrelse",
       x = "Antal spørgsmål (k)",
       y = "Gennemsnitlig adjR²") +
  theme_minimal(base_size = 12)

