# INSTALLATION AF NØDVENDIGE PAKKER ------------------------------------------

install.packages("dplyr")
install.packages("readr")
install.packages("forecast")
install.packages("tidyverse")

# INDLÆS PAKKER
library(dplyr)
library(readr)
library(tidyverse)
library(forecast)

# FIND FEJL I ENCODING ERSTATTE STJERNE MED Æ,Ø,Å
guess_encoding("Forv1.csv",  n_max = 1e5)
guess_encoding("DETA11.csv", n_max = 1e5)

# INDLÆS DATA MED KORREKT ENCODING

# Forbrugerforventninger
Forv1 <- read_delim("Forv1.csv",
                    delim = ";",
                    locale = locale(encoding = "Latin1"),
                    escape_double = FALSE,
                    trim_ws = TRUE)

# Detailhandel
DETA11 <- read_delim("DETA11.csv",
                     delim = ";",
                     locale = locale(encoding = "Latin1"),
                     escape_double = FALSE,
                     trim_ws = TRUE)


############# OPGAVE 3.1 – FORBRUGERTILLID OG JULEHANDEL###########


# 1. Filtrér kun den samlede forbrugertillidsindikator
forbrugertillid <- Forv1 %>%
  filter(...1 == "Forbrugertillidsindikatoren")

# 2. Vend data (måneder som rækker)
fbi <- forbrugertillid %>%
  pivot_longer(
    cols = -...1,
    names_to = "Tid",
    values_to = "Indikator"
  ) %>%
  mutate(
    År = as.numeric(substr(Tid, 1, 4)),
    Måned = as.numeric(substr(Tid, 6, 7)),
    Dato = as.Date(paste0(År, "-", Måned, "-01"))
  ) %>%
  select(Dato, Indikator)

# 3. Opret tidsserie (månedlige data)
ts_data <- ts(fbi$Indikator, start = c(2015, 1), frequency = 12)

# 4. Byg model og lav prognose
model <- auto.arima(ts_data)
forecast_2025 <- forecast(model, h = 12)

# 5. Visualisér prognose
autoplot(forecast_2025) +
  labs(title = "Forbrugertillid – prognose for 2025",
       y = "Forbrugertillidsindikator",
       x = "Tid") +
  theme_minimal()

# 6. Sammenlign gennemsnit 2024 vs. 2025
mean_2024 <- mean(tail(ts_data, 12))          
mean_2025 <- mean(forecast_2025$mean)         

if (mean_2025 > mean_2024) {
  print("Forbrugertilliden forventes at stige → julehandlen sandsynligvis større")
} else {
  print("Forbrugertilliden forventes at falde → julehandlen sandsynligvis lavere")
}

# 7. Model-diagnosticering
summary(model)           
checkresiduals(model)    
accuracy(model)          



################ OPGAVE 3.2 – DETAILHANDEL OG SAMMENHÆNG MED FORBRUGERTILLID ##############

# 1. Vend DETA11-data (måneder som rækker)
retail <- DETA11 %>%
  pivot_longer(
    cols = -...1,
    names_to = "Tid",
    values_to = "Omsætning"
  ) %>%
  mutate(
    År = as.numeric(substr(Tid, 1, 4)),
    Måned = as.numeric(substr(Tid, 6, 7)),
    Dato = as.Date(paste0(År, "-", Måned, "-01"))
  ) %>%
  select(Branche = ...1, Dato, Omsætning)

# 2. Filtrér brancher, som er relevante for julehandel
alle_brancher <- c(
  "4700 Detailhandel i alt",
  "471120 Supermarkeder",
  "471130 Discountforretninger",
  "475930 Detailhandel med køkkenudstyr, glas, porcelæn, bestik, vaser, lysestager mv.",
  "Detailhandel med spil og legetøj samt musik- og videooptagelser",
  "477110 Tøjforretninger",
  "477120 Babyudstyrs- og børnetøjsforretninger",
  "Sko- og lædervareforretninger (477210, 477220)",
  "477700 Detailhandel med ure, smykker og guld- og sølvvarer",
  "479111 Detailhandel med dagligvarer via internet",
  "479112 Detailhandel med elektroniske eller elektriske apparater samt fotoudstyr via internet",
  "479114 Detailhandel med bøger, kontorartikler, musik eller film via internet",
  "479115 Detailhandel med hobbyartikler, musikinstrumenter, sportsudstyr, legetøj, cykler via internet",
  "479116 Detailhandel med tøj, sko, lædervarer, ure eller babyudstyr via internet"
)

julebrancher <- retail %>%
  filter(Branche %in% alle_brancher)

# 3. Opret tidsserie for "Detailhandel i alt"
detail_total <- julebrancher %>%
  filter(Branche == "4700 Detailhandel i alt")

ts_detail <- ts(detail_total$Omsætning, start = c(2015, 1), frequency = 12)
forecast_detail <- forecast(auto.arima(ts_detail), h = 12)

# 4. Plot udviklingen og prognose for 2025
autoplot(forecast_detail) +
  labs(title = "Prognose for detailomsætning 2025",
       y = "Detailomsætning (indeks)", 
       x = "Tid") +
  theme_minimal()

# 5. Sammenlign udviklingen i forbrugertillid og detailhandel
combined <- left_join(fbi, detail_total, by = "Dato")

ggplot(combined, aes(x = Dato)) +
  geom_line(aes(y = scale(Indikator), color = "Forbrugertillid")) +
  geom_line(aes(y = scale(Omsætning), color = "Detailomsætning")) +
  labs(title = "Sammenhæng mellem forbrugertillid og detailhandel",
       y = "Indeks (standardiseret)", 
       x = "Tid", 
       color = "Serie") +
  theme_minimal()
