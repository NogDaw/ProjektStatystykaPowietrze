library(dplyr)
library(tidyr)
library(readr)
library(Hmisc)

# --- LOADING DATA ---
data <- read_csv2("AirQualityUCI.csv")

data <- data %>%
  drop_na(Time)

selected_data <- data %>% 
  select(Time, T, RH, `CO(GT)`, `PT08.S1(CO)`)
# --- DATA LOADED ---

# --- TEST ---
numeric_data <- selected_data %>%
  select(where(is.numeric)) %>%
  # We need to remove missing values so the correlation matrix calculates cleanly
  drop_na()

# 2. Run the correlation test on the entire matrix
full_correlation_test <- rcorr(as.matrix(numeric_data))

# 3. Display the results
# This will show the correlation coefficients (r)
print("--- Correlation Coefficients (r) ---")
print(round(full_correlation_test$r, 2))

# This will show the p-values (P) for statistical significance
print("--- P-values ---")
print(round(full_correlation_test$P, 4))
# --- END TEST ---