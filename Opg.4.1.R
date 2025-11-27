
##############################
# Opgave 4.1 – Simple stabilitetstests
##############################
library(dplyr)
library(ggplot2)
library(tidyr)

# Forudsætninger:
# - ftiq: data.frame med 'tid', 'realvaekst' og alle FT-variabler
# - best_safe_vec: de 5 variabler fra jeres bedste indikator

# A) DATA
dat <- ftiq %>%
  select(tid, realvaekst, all_of(best_safe_vec)) %>%
  drop_na()

# Hjælper: lille funktion der returnerer (adj)R² for en model
get_r2 <- function(df) {
  s <- summary(lm(realvaekst ~ ., data = df))
  c(R2 = unname(s$r.squared), adjR2 = unname(s$adj.r.squared))
}

# -----------------------------------------------------------
# 1) RULLENDE adj.R² (vindue = 40 kvartaler ≈ 10 år)
# -----------------------------------------------------------
W <- 40
n <- nrow(dat)
roll <- vector("list", n)

for (i in seq_len(n)) {
  i1 <- max(1, i - W + 1)
  d  <- dat[i1:i, ] %>% select(-tid)
  if (nrow(d) >= (length(best_safe_vec) + 3)) {
    r2 <- tryCatch(get_r2(d), error = function(e) c(R2=NA, adjR2=NA))
  } else {
    r2 <- c(R2=NA, adjR2=NA)
  }
  roll[[i]] <- data.frame(tid = dat$tid[i], R2 = r2[["R2"]], adjR2 = r2[["adjR2"]])
}
roll_metrics <- bind_rows(roll)

# Pænt plot: vis kun hver 6. label og skrå tekst
x_lvls   <- unique(dat$tid)
x_breaks <- x_lvls[seq(1, length(x_lvls), by = 6)]

p_roll <- ggplot(roll_metrics, aes(tid, adjR2, group = 1)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = mean(roll_metrics$adjR2, na.rm = TRUE),
             linetype = "dotted") +
  scale_x_discrete(breaks = x_breaks) +
  labs(title = "Rullende justeret R² (vindue = 40 kvartaler)",
       subtitle = paste0("Gns. adj.R² = ", sprintf("%.3f", mean(roll_metrics$adjR2, na.rm = TRUE))),
       x = NULL, y = "Adj. R²") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())
print(p_roll)

# -----------------------------------------------------------
# 2) SPLIT-SAMPLE (før/efter 2013K1 )
# -----------------------------------------------------------
cut_tid <- "2013K1"
pre  <- dat %>% filter(tid <  cut_tid)
post <- dat %>% filter(tid >= cut_tid)

mod_pre  <- lm(realvaekst ~ ., data = pre  %>% select(-tid))
mod_post <- lm(realvaekst ~ ., data = post %>% select(-tid))

sum_pre  <- summary(mod_pre)
sum_post <- summary(mod_post)

cat("\n=== Split-sample stabilitet ===\n")
cat(sprintf("Pre  (%s … %s): adj.R² = %.3f (N=%d)\n",
            first(pre$tid), last(pre$tid), unname(sum_pre$adj.r.squared), nrow(pre)))
cat(sprintf("Post (%s … %s): adj.R² = %.3f (N=%d)\n",
            first(post$tid), last(post$tid), unname(sum_post$adj.r.squared), nrow(post)))

# Pæn, kort koefficienttabel (uden intercept)
coef_tbl <- function(sobj) {
  as.data.frame(sobj$coefficients) |>
    tibble::rownames_to_column("term") |>
    filter(term != "(Intercept)") |>
    transmute(term,
              Estimate = round(Estimate, 3),
              `Pr(>|t|)` = round(`Pr(>|t|)`, 3))
}
cat("\n— Pre koefficienter —\n");  print(coef_tbl(sum_pre),  row.names = FALSE)
cat("\n— Post koefficienter —\n"); print(coef_tbl(sum_post), row.names = FALSE)

# -----------------------------------------------------------
# 3) OUT-OF-SAMPLE (1-step-ahead) – enkel, robust
# -----------------------------------------------------------
# start efter en kort “opvarmning” (fx når vi har 32 obs.)
start_idx <- max(32, 1 + length(best_safe_vec) + 1)
oos <- vector("list", n - start_idx)

for (t_end in seq(start_idx, n-1)) {
  train <- dat[1:t_end, ]  %>% select(-tid)
  test  <- dat[t_end+1, , drop = FALSE]
  m     <- lm(realvaekst ~ ., data = train)
  yhat  <- predict(m, newdata = test)
  oos[[t_end - start_idx + 1]] <- data.frame(
    tid  = test$tid,
    y    = test$realvaekst,
    yhat = as.numeric(yhat),
    err  = test$realvaekst - as.numeric(yhat)
  )
}
oos <- bind_rows(oos)

oos_metrics <- summarise(oos,
                         RMSE = sqrt(mean(err^2, na.rm=TRUE)),
                         MAE  = mean(abs(err), na.rm=TRUE))
cat("\n=== OOS-fejl (1-step-ahead) ===\n"); print(oos_metrics)

# Plot med færre labels og pæn undertekst
p_oos <- ggplot(oos, aes(tid, y, group = 1)) +
  geom_line(color = "black", linewidth = 0.9) +
  geom_line(aes(y = yhat), color = "#1B9E77", linewidth = 1.1) +
  scale_x_discrete(breaks = x_breaks) +
  labs(title = "Pseudo real-time: 1-step-ahead forudsigelser",
       subtitle = paste0("RMSE = ", sprintf('%.2f', oos_metrics$RMSE),
                         " • MAE = ", sprintf('%.2f', oos_metrics$MAE)),
       x = NULL, y = "Realvækst (å/å)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())
print(p_oos)

