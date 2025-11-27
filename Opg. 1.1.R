################# Pakker #################
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(ggplot2)
library(stringr)

################# Opg. 1.1: FT -> kvartal -> bredt #################

# 1) Månedlige FT-tal til kvartal
ft_mdl_lang <- Ftillid_Q3_25 %>%
  rename(spoergsmaal = 1) %>%
  mutate(across(matches("^\\d{4}M\\d{2}$"),
                ~ suppressWarnings(parse_number(as.character(.x))))) %>%
  pivot_longer(matches("^\\d{4}M\\d{2}$"),
               names_to = "maaned", values_to = "vaerdi") %>%
  mutate(
    tid = paste0(substr(maaned, 1, 4),
                 "K",
                 (as.integer(substr(maaned, 6, 7)) - 1) %/% 3 + 1)
  ) %>%
  group_by(tid, spoergsmaal) %>%
  summarise(vaerdi = mean(vaerdi, na.rm = TRUE), .groups = "drop") %>%
  filter(tid >= "2000K1", tid <= "2025K3")

# 2) Bredt datasæt med 12 spørgsmål
FTDI25Q <- ft_mdl_lang %>%
  pivot_wider(names_from = spoergsmaal, values_from = vaerdi) %>%
  arrange(tid)

# 3) Tjek at vi har 12 spørgsmål
spg_pretty <- setdiff(names(FTDI25Q), "tid")
stopifnot(length(spg_pretty) == 12)

# 4) Lav sikre navne
nm <- tibble(
  pretty = c("tid", spg_pretty),
  safe   = make.names(c("tid", spg_pretty), unique = TRUE)
)

FTDI25Q_safe <- FTDI25Q
names(FTDI25Q_safe) <- nm$safe
spg_safe <- nm$safe[-1]  # alle undtagen tid

################# Opg. 1.2: Forbrug -> realvækst #################

# Dit datasæt har kun én række — vi bruger hele rækken som forbrug
forbrug_P31 <- Forbrug_25 %>%
  rename(indikator = 1) %>%
  mutate(across(matches("^\\d{4}K[1-4]$"),
                ~ suppressWarnings(parse_number(as.character(.x))))) %>%
  pivot_longer(matches("^\\d{4}K[1-4]$"),
               names_to = "tid", values_to = "forbrug") %>%
  arrange(tid) %>%
  mutate(realvaekst = (forbrug / lag(forbrug) - 1) * 100) %>%
  filter(tid >= "2000K1", tid <= "2025K3")

################# Merge + dataklargøring #################

# Join FT + forbrug
ftiq <- FTDI25Q_safe %>%
  left_join(forbrug_P31, by = "tid") %>%
  drop_na(realvaekst)

# Gør alle spørgsmål numeriske
ftiq[, 2:ncol(ftiq)] <- lapply(ftiq[, 2:ncol(ftiq)], function(x) as.numeric(as.character(x)))

# Vend fortegn for spørgsmål 7 og 8
ftiq[, 7] <- ftiq[, 7] * -1
ftiq[, 8] <- ftiq[, 8] * -1

ftiq <- ftiq %>%
  mutate(
    indikator = rowMeans(across(all_of(spg_safe)), na.rm = TRUE)
  )

################# Byg kombinationer #################

K_MAX <- length(spg_safe)  # 12

kombinationer_liste <- lapply(1:K_MAX, function(k) {
  combn(spg_safe, k, simplify = FALSE)
})
names(kombinationer_liste) <- paste0("k_", 1:K_MAX)
alle_kombinationer <- unlist(kombinationer_liste, recursive = FALSE)

################# Plot: antal kombinationer pr. størrelse #################

# Lav et datasæt med størrelsen på hver kombination
komb_plot_df <- tibble(
  stoerrelse = purrr::map_int(alle_kombinationer, length)
)

# Lav barplot over antal kombinationer i hver størrelse
komb_plot_df %>%
  count(stoerrelse) %>%
  mutate(andelen = n / sum(n)) %>%
  ggplot(aes(x = factor(stoerrelse), y = andelen)) +
  geom_col(fill = "#1B9E77") +
  geom_text(aes(label = scales::percent(andelen, accuracy = 0.1)),
            vjust = -0.4, size = 3.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Fordeling af kombinationer efter størrelse (k)",
    subtitle = "Viser andelen af de 4.095 mulige kombinationer af 12 spørgsmål",
    x = "Antal spørgsmål i kombination (k)",
    y = "Andel af alle kombinationer"
  ) +
  theme_minimal(base_size = 13)

