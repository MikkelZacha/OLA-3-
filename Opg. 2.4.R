############ Opg. 2.4 find årlig realvækst fra 2015 til 2016 ################

library(dplyr)

#⃣## Udvælg 2015 og 2016
forbrug_sum <- ftiq %>%
  filter(grepl("2015|2016", tid)) %>%        # kun de to år
  mutate(år = substr(tid, 1, 4)) %>%         # udtræk år fra fx "2016K3"
  group_by(år) %>%                           # gruppér pr. år
  summarise(forbrug_sum = sum(forbrug, na.rm = TRUE))  # summer kvartalerne

#⃣### Beregn ændringen (væksten) fra 2015 → 2016
forbrug_2016 <- (forbrug_sum$forbrug_sum[forbrug_sum$år == "2016"] -
                     forbrug_sum$forbrug_sum[forbrug_sum$år == "2015"]) /
  forbrug_sum$forbrug_sum[forbrug_sum$år == "2015"] * 100

### Se resultatet
forbrug_2016

# Stigning i forbrug på 3.277857% - lidt i overkanten ift. 2% der er nævnt andetsteds

library(ggplot2)

