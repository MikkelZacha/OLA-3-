#############################################
# Figur: Faktisk realvækst vs. scenarie (K3–K4 2025)
#############################################
library(dplyr)
library(ggplot2)
library(tidyr)

# 1) Historiske observationer (t.o.m. seneste kvartal i datasættet, fx 2025K2)
hist_df <- ftiq %>%
  select(tid, realvaekst) %>%
  filter(!is.na(realvaekst), tid <= seneste_kvartal) %>%
  mutate(type = "Faktisk (observ.)")

# 2) Scenarie-forudsigelser (fra 'out')
#    - Bruger vores egen indikator (pred_realvaekst_egen)
fc_df <- out %>%
  select(tid, realvaekst = pred_realvaekst_egen) %>%
  mutate(type = "Forudsigelse (egen indikator)")

# (Valgfrit) Hvis DI-forecast findes i 'out', læg det til som ekstra serie
har_di_fc <- "pred_realvaekst_DI" %in% names(out)
if (har_di_fc) {
  fc_di <- out %>%
    select(tid, realvaekst = pred_realvaekst_DI) %>%
    mutate(type = "Forudsigelse (DI-FTI)")
  fc_df <- bind_rows(fc_df, fc_di)
}

# 3) Kombinér til plotting og sæt pæn kvartalsrækkefølge
plot_df <- bind_rows(hist_df, fc_df) %>%
  arrange(tid) %>%
  mutate(tid = factor(tid, levels = unique(.$tid)))

# 4) Tegn figur: fuld linje for faktisk, stiplet for forudsigelser
p <- ggplot() +
  # Faktisk (solid linje + fyldte punkter)
  geom_line(data = filter(plot_df, type == "Faktisk (observ.)"),
            aes(x = tid, y = realvaekst, group = 1), linewidth = 1) +
  geom_point(data = filter(plot_df, type == "Faktisk (observ.)"),
             aes(x = tid, y = realvaekst), size = 2) +
  # Forudsigelser (stiplet + hule punkter)
  geom_line(data = filter(plot_df, type != "Faktisk (observ.)"),
            aes(x = tid, y = realvaekst, group = type, linetype = type), linewidth = 1) +
  geom_point(data = filter(plot_df, type != "Faktisk (observ.)"),
             aes(x = tid, y = realvaekst, shape = type), size = 2.5, stroke = 1) +
  # Lodret markering af seneste “faktiske” kvartal
  geom_vline(xintercept = which(levels(plot_df$tid) == seneste_kvartal),
             linetype = "dotted") +
  annotate("text",
           x = which(levels(plot_df$tid) == seneste_kvartal),
           y = max(plot_df$realvaekst, na.rm = TRUE),
           label = paste0(" Seneste observation: ", seneste_kvartal),
           hjust = 0, vjust = -0.5, size = 3.3) +
  labs(title = "Husholdningernes realvækst i forbrug: faktisk vs. forudsigelser",
       subtitle = "Forudsigelser baseret på egen indikator (og DI-FTI hvis tilgængelig)",
       x = NULL,
       y = "Realvækst i pct.-point (å/å)") +
  scale_linetype_discrete(name = NULL) +
  scale_shape_discrete(name = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5),
        legend.position = "top",
        panel.grid.minor = element_blank())

print(p)

# 5) (Valgfrit) Gem figur til rapport
ggsave("forecast_plot_OLA3.png", p, width = 9, height = 4.8, dpi = 300)

#############################################
# Pænere figur: Faktisk realvækst vs. forudsigelser
#############################################
library(ggplot2)
library(dplyr)

# Vi bruger det samme plot_df som før
plot_df <- bind_rows(
  ftiq %>%
    select(tid, realvaekst) %>%
    filter(!is.na(realvaekst), tid <= seneste_kvartal) %>%
    mutate(type = "Faktisk (observ.)"),
  out %>%
    select(tid, realvaekst = pred_realvaekst_egen) %>%
    mutate(type = "Forudsigelse (egen indikator)")
) %>%
  arrange(tid) %>%
  mutate(tid = factor(tid, levels = unique(.$tid)))

# Kun vis fx hvert 4. kvartal som label
x_breaks <- levels(plot_df$tid)[seq(1, length(levels(plot_df$tid)), by = 4)]

# Plot
p <- ggplot(plot_df, aes(x = tid, y = realvaekst, group = type, color = type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = which(levels(plot_df$tid) == seneste_kvartal),
             linetype = "dotted", color = "black") +
  annotate("text",
           x = which(levels(plot_df$tid) == seneste_kvartal),
           y = max(plot_df$realvaekst, na.rm = TRUE),
           label = paste0(" Seneste observation: ", seneste_kvartal),
           hjust = 0, vjust = -0.5, size = 3.3) +
  scale_x_discrete(breaks = x_breaks) +  # færre labels
  scale_color_manual(values = c("black", "#1B9E77")) +  # pænere farver
  labs(
    title = "Husholdningernes realvækst i forbrug: faktisk vs. forudsigelser",
    subtitle = "Scenarie: fald på 2 point i K3, stigning på 1 point i K4",
    x = "Kvartal",
    y = "Realvækst i pct.-point (å/å)",
    color = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),  # skrå tekst
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

print(p)

