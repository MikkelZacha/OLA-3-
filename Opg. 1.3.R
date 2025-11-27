################# Opg. 1.3: Spørgsmål i indikatoren + alternativtjek #################
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

## 1) Spørgsmålene i bedste kombination (adjR²)
stopifnot(exists("best_adjR2"), exists("r2_resultater"), exists("nm"))

# 'kombi_tekst' indeholder pæne navne adskilt med " | "
best_pretty_vec <- strsplit(best_adjR2$kombi_tekst, " \\| ")[[1]]

# Map "pæne navne" -> "safe navne" (til modelkald)
best_safe_vec <- nm$safe[match(best_pretty_vec, nm$pretty)]

cat("\n=== Bedste kombination (adjR²) — pæne navne ===\n")
print(best_pretty_vec)

cat("\n=== Bedste kombination (adjR²) — safe navne ===\n")
print(best_safe_vec)

## 2) Fit den bedste model og vis en kompakt koefficienttabel
frm_best <- reformulate(best_safe_vec, response = "realvaekst")
mod_best <- lm(frm_best, data = ftiq %>% select(realvaekst, all_of(best_safe_vec)) %>% drop_na())

sum_best <- summary(mod_best)
coef_tab <- as.data.frame(sum_best$coefficients)
coef_tab <- tibble::rownames_to_column(coef_tab, var = "parameter")
names(coef_tab) <- c("parameter", "Estimate", "Std.Error", "t.value", "Pr(>|t|)")

cat("\n=== Koefficienter for bedste model ===\n")
print(coef_tab, row.names = FALSE)

cat("\nAdj.R²:", unname(sum_best$adj.r.squared), "  |  R²:", unname(sum_best$r.squared), "\n")

## 3) Alternativer — idéer til “giver det mening?” og robusthed

# 3a) Bedste enkelt-indikator (fra r2_enkelt)
stopifnot(exists("r2_enkelt"))
best_single <- r2_enkelt %>% arrange(desc(R2), desc(adjR2)) %>% slice(1)
cat("\n=== Bedste enkelt-indikator ===\n")
print(best_single)

# 3b) Top-5 kombinationer med samme størrelse (k) som vinderen
k_vinder <- length(best_safe_vec)
top_same_k <- r2_resultater %>%
  filter(stoerrelse == k_vinder) %>%
  arrange(desc(adjR2), desc(R2)) %>%
  slice_head(n = 5)

cat("\n=== Top-5 kombinationer med samme k som vinderen ===\n")
print(top_same_k)

# 3c) Leave-one-out (fjern ét ad gangen fra vinderens sæt) og se delta adjR²
loo_res <- purrr::map_dfr(seq_along(best_safe_vec), function(i) {
  vars_loo <- best_safe_vec[-i]
  tmp <- ftiq %>% select(realvaekst, all_of(vars_loo)) %>% drop_na()
  if (nrow(tmp) < (length(vars_loo) + 3)) {
    return(tibble(variant = paste0("Uden: ", best_pretty_vec[i]),
                  adjR2 = NA_real_))
  }
  s <- summary(lm(reformulate(vars_loo, "realvaekst"), data = tmp))
  tibble(variant = paste0("Uden: ", best_pretty_vec[i]),
         adjR2 = unname(s$adj.r.squared))
})

loo_res <- loo_res %>% mutate(delta_adjR2 = adjR2 - best_adjR2$adjR2)
cat("\n=== Leave-one-out ift. vinder (delta adjR²) ===\n")
print(loo_res)

# 3d) Add-one: prøver at tilføje én variabel fra de resterende ()
resten_safe <- setdiff(nm$safe[-1], best_safe_vec)  # alle FT-spørgsmål minus vinderens
add1_res <- purrr::map_dfr(resten_safe, function(v) {
  vars_add <- c(best_safe_vec, v)
  tmp <- ftiq %>% select(realvaekst, all_of(vars_add)) %>% drop_na()
  if (nrow(tmp) < (length(vars_add) + 3)) {
    return(tibble(tilfoejet = nm$pretty[nm$safe == v], adjR2 = NA_real_))
  }
  s <- summary(lm(reformulate(vars_add, "realvaekst"), data = tmp))
  tibble(tilfoejet = nm$pretty[nm$safe == v],
         adjR2 = unname(s$adj.r.squared))
})

add1_res <- add1_res %>% arrange(desc(adjR2)) %>% slice_head(n = 5)
cat("\n=== Add-one: top-5 kandidater at tilføje (adjR²) ===\n")
print(add1_res)


