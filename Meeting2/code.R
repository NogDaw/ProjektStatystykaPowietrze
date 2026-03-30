library(dplyr)
library(tidyr)
library(readr)

# --- LOADING DATA ---
dane <- read_csv2("../AirQualityUCI.csv")

dane <- dane %>%
  drop_na(Time)

dane_wybrane <- dane %>% 
  select(Time, T, RH, `CO(GT)`, `PT08.S1(CO)`)
# --- DATA LOADED ---

# --- SECTION ---
# --- ENDSECTION ---