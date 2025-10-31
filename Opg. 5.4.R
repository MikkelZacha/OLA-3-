## Stormen okt 2023 – vindstyrke + vindretning med pile og labels
#######################

# nødvendige pakker
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)

## === 0) Opsætning ===
ak      <- "9253d782-9484-4918-87db-619d73b96f30"   # <- brug din egen nøgle
baseurl <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?"

# to stationer – byt dem hvis du vil
stations <- c(
  "06068" = "Aarhus Havn",
  "06080" = "Anholt"
)

# periode: stormen i oktober 2023
from_dt <- "2023-10-19T00:00:00Z"
to_dt   <- "2023-10-22T00:00:00Z"

# parametre (vi henter én ad gangen)
p_speed <- "wind_speed_past1h"   # time-middel vindhastighed
p_dir   <- "wind_dir_past1h"     # tilsvarende vindretning


## === 1) Hjælpefunktion: hent ÉN parameter for ÉN station ===
fetch_one <- function(station_id, param_id) {
  # byg query til DMI-API
  q <- paste0(
    "stationId=", station_id,
    "&datetime=", from_dt, "/", to_dt,
    "&parameterId=", param_id,
    "&api-key=", ak
  )
  url <- paste0(baseurl, q)
  cat("Henter:", url, "\n")   # lille log på konsollen
  
  # kald API
  res <- httr::GET(url)
  if (res$status_code != 200) {
    # hvis vi ikke får OK retur, så stop pænt
    warning("Kunne ikke hente ", param_id, " for station ", station_id,
            " (status ", res$status_code, ")")
    return(NULL)
  }
  
  # læs JSON som tekst og parse til liste
  raw <- httr::content(res, as = "text", encoding = "UTF-8")
  js  <- jsonlite::fromJSON(raw)
  
  # hvis der ikke er features, så sig til
  if (length(js$features) == 0) {
    warning("Ingen features for ", param_id, " / station ", station_id)
    return(NULL)
  }
  
  # træk properties ud som data.frame
  df <- as.data.frame(js$features$properties)
  df$time <- js$features$properties$observed  # tid ligger også dér
  df$stationId <- station_id                  # så vi ved hvorfra
  df$parameterId <- param_id                  # og hvilken parameter
  return(df)
}


## === 2) Hent alle stationer ===
all_list <- list()   # vi gemmer data for hver station her

for (sid in names(stations)) {
  # 2a) vindstyrke
  spd <- fetch_one(sid, p_speed)
  # 2b) vindretning
  dir <- fetch_one(sid, p_dir)
  
  # kun hvis vi fik begge dele, slår vi dem sammen
  if (!is.null(spd) && !is.null(dir)) {
    df <- spd %>%
      select(time, stationId, value_speed = value) %>%     # omdøb value -> value_speed
      left_join(
        dir %>% select(time, stationId, value_dir = value), # omdøb value -> value_dir
        by = c("time", "stationId")
      ) %>%
      mutate(station = stations[sid])  # pæn titel på stationen
    all_list[[sid]] <- df
  }
}

# læg alle stationer ned i ét samlet data frame
obs <- bind_rows(all_list)

# hvis du vil se hvad vi fik:
head(obs)

## === 3) Datoformat + klassificér vindretning ===
obs <- obs %>%
  mutate(
    # lav tid om til rigtig POSIXct
    time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    value_dir = as.numeric(value_dir)
  ) %>%
  arrange(station, time)

# lav 8-dels kompas-labels ud fra vindretning
obs <- obs %>%
  mutate(
    wind_label = case_when(
      is.na(value_dir)                               ~ NA_character_,  # hvis ingen retning
      value_dir >= 22.5  & value_dir < 67.5          ~ "ØNØ",
      value_dir >= 67.5  & value_dir < 112.5         ~ "Ø",
      value_dir >= 112.5 & value_dir < 157.5         ~ "ØSØ",
      value_dir >= 157.5 & value_dir < 202.5         ~ "S",
      value_dir >= 202.5 & value_dir < 247.5         ~ "SV",
      value_dir >= 247.5 & value_dir < 292.5         ~ "V",
      value_dir >= 292.5 & value_dir < 337.5         ~ "NV",
      TRUE                                            ~ "N"            # ellers regner vi den som nord
    )
  )

### Trin 4 - lav særskilt data til pilene (så vi ikke viser én pil pr. time)
arrows_df <- obs %>%
  group_by(station) %>%
  slice(seq(1, n(), by = 4)) %>%   # tag hver 4. måling = tyndere mængde pile
  ungroup() %>%
  mutate(
    len  = 0.5,                      # længde på pil
    rad  = value_dir * pi / 180,     # grader -> radianer
    xstart = time,                   # pilens start-x er tidspunktet
    ystart = value_speed,            # pilens start-y er vindhastigheden
    # vi lader x være tiden, og flytter kun i y-retning (pilen peger "opad" ift. retning)
    xend   = time,
    yend   = value_speed + len * cos(rad)
  )

### Trin 5 - PLOT

# find højeste vindstød (her: højeste value_speed) og tidspunktet
max_row <- obs[which.max(obs$value_speed), ]
max_val  <- max_row$value_speed
max_time <- max_row$time

ggplot(obs, aes(x = time, y = value_speed, colour = station)) +
  # selve tidsserien pr. station
  geom_line(linewidth = 1) +
  # pile (vindretning)
  geom_segment(
    data = arrows_df,
    aes(x = xstart, y = ystart, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.12, "cm")),
    colour = "black",
    linewidth = 0.4,
    inherit.aes = FALSE
  ) +
  # tekst over pilene (8-dels kompas)
  geom_text(
    data = arrows_df,
    aes(x = xstart, y = ystart + 0.7, label = wind_label),
    colour = "black",
    size = 3,
    vjust = 0
  ) +
  # tekst om dagens / periodens maksimale vindstød
  annotate(
    "text",
    x = max_time,
    y = max_val + 1,  # lidt over toppen, så vi kan se teksten
    label = paste0("Højest målte vindstød: ", round(max_val, 1), " m/s"),
    colour = "black",
    hjust = 0.5
  ) +
  labs(
    title = "Vind under stormen i oktober 2023",
    subtitle = "Pile + label = vindretning (8-dels kompas)",
    x = "Tid (UTC)",
    y = "Vindhastighed (m/s)",
    colour = "station"
  ) +
  theme_minimal(base_size = 13)
