############ Opg. 2.4 find årlig realvækst fra 2015 til 2016 ################

library(dplyr)
library(ggplot2)

df_sammenl <- df_sammenl %>%
  mutate(tid = factor(tid, levels = tid))

# skaler forbrug til højre akse
rng_pct  <- range(df_sammenl$realvaekst, na.rm = TRUE)
rng_forb <- range(df_sammenl$forbrug,    na.rm = TRUE)
fac <- diff(rng_pct) / diff(rng_forb)

df_sammenl <- df_sammenl %>%
  mutate(forbrug_scaled = (forbrug - rng_forb[1]) * fac + rng_pct[1])

# hvor skal tallene stå?
y_bottom <- min(df_sammenl$realvaekst, na.rm = TRUE) - 0.4

ggplot(df_sammenl, aes(x = tid)) +
  # Søjler = realvækst
  geom_col(aes(y = realvaekst, fill = "Realvækst i privatforbruget"), width = 0.55) +
  geom_text(
    aes(y = y_bottom, label = round(realvaekst, 1)),
    size = 3, color = "grey25", vjust = 1
  ) +
  
  # Linje + punkter = forbrug (forbundet)
  geom_line(
    aes(y = forbrug_scaled,
        colour = "Privatforbruget, 2015-priser",
        group = 1),              # ← det er den vigtige
    linewidth = 1
  ) +
  geom_point(
    aes(y = forbrug_scaled, colour = "Privatforbruget, 2015-priser"),
    size = 2
  ) +
  
  scale_y_continuous(
    name = "Realvækst i forbrug (%)",
    sec.axis = sec_axis(~ (. - rng_pct[1]) / fac + rng_forb[1],
                        name = "Forbrug (niveau)")
  ) +
  scale_fill_manual(values = c("Realvækst i privatforbruget" = "#2A96D5"), name = NULL) +
  scale_color_manual(values = c("Privatforbruget, 2015-priser" = "grey20"), name = NULL) +
  labs(
    title = "Fortsat fremgang i privatforbruget, 2015–2016",
    subtitle = "Kvartalsvis realvækst (søjler) og forbrugsniveau (linje)",
    x = NULL
  ) +
  expand_limits(y = y_bottom - 0.15) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    axis.title.y.right = element_text(color = "grey20"),
    axis.title.y.left  = element_text(color = "#2A96D5"),
    plot.margin = margin(10, 10, 35, 10)
  ) +
  annotate(
    "text",
    x = -Inf, y = -Inf,
    label = "Kilde: Danmarks Statistik, egne beregninger",
    hjust = -0.01, vjust = -2, size = 3
  )
