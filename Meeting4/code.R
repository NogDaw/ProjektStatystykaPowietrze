library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(writexl)

# --- LOADING DATA ---
data <- read_csv2("AirQualityUCI.csv")

data <- data %>%
  drop_na(Time)

selected_data <- data %>% 
  select(Time, T, RH, `CO(GT)`, `PT08.S1(CO)`)
# --- DATA LOADED ---

# --- MEAN CALCULATIONS ---

# 1. FIXING DATA (-200 indicates sensor error, replace with NA)
selected_data <- selected_data %>%
  mutate(across(where(is.numeric), ~na_if(., -200)))

data_with_intervals <- selected_data %>%
  mutate(
    Hour = as.numeric(substr(Time, 1, 2)),
    Interval = case_when(
      Hour >= 0  & Hour <= 5  ~ "1. Night",
      Hour >= 6  & Hour <= 11 ~ "2. Morning",
      Hour >= 12 & Hour <= 17 ~ "3. Afternoon",
      Hour >= 18 & Hour <= 23 ~ "4. Evening"
    )
  )

# --- END MEAN CALCULATIONS ---

# --- INTERVAL HISTOGRAMS ---

# --- VISUAL NORMALITY CHECK: SELECTED CASES ONLY WITH NORMAL CURVES ---

# 1. Filtering data for the 3 specific cases
selected_normality_data <- data_with_intervals %>%
  select(Interval, T, RH) %>%
  pivot_longer(cols = c(T, RH), names_to = "Variable", values_to = "Value") %>%
  filter(
    (Variable == "RH" & Interval %in% c("1. Night", "2. Morning")) |
      (Variable == "T" & Interval == "2. Morning")
  ) %>%
  filter(!is.na(Value)) # Ensure no NAs for calculations

# 2. PRE-CALCULATING NORMAL CURVE DATA
# We need this to draw different red lines for each facet
normal_curves <- selected_normality_data %>%
  group_by(Variable, Interval) %>%
  summarise(
    mean = mean(Value),
    sd = sd(Value),
    min = min(Value),
    max = max(Value),
    .groups = "drop"
  ) %>%
  group_by(Variable, Interval) %>%
  # Generate 100 points per curve to make it smooth
  do(data.frame(
    x = seq(.$min, .$max, length.out = 100),
    y = dnorm(seq(.$min, .$max, length.out = 100), .$mean, .$sd)
  ))

# 3. GENERATING HISTOGRAMS WITH RED LINES
spec_hist_plot <- ggplot(selected_normality_data, aes(x = Value)) +
  # Histogram bars
  geom_histogram(aes(y = ..density..), bins = 25, fill = "skyblue", color = "white") +
  # Adding the red normal curves from our calculated data frame
  geom_line(data = normal_curves, aes(x = x, y = y), color = "#FF00009F", linewidth = 1) +
  facet_wrap(~Variable + Interval, scales = "free") +
  theme_minimal() +
  labs(
    title = "Histograms for Approximately Normal Distributions",
    subtitle = "Red lines represent theoretical normal distribution for each group",
    x = "Measured Value",
    y = "Density"
  )

print(spec_hist_plot)

# 4. GENERATING Q-Q PLOTS (No changes needed here, they are already correct)
spec_qq_plot <- ggplot(selected_normality_data, aes(sample = Value)) +
  stat_qq(color = "steelblue", alpha = 0.5) +
  stat_qq_line(color = "red") +
  facet_wrap(~Variable + Interval, scales = "free") +
  theme_minimal() +
  labs(
    title = "Q-Q Plots for Approximately Normal Distributions",
#    subtitle = "I don't know what to put here",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

print(spec_qq_plot)

# Save plots
ggsave("Meeting4/Normality_Histograms.png", plot = spec_hist_plot, width = 10, height = 4, dpi = 300)
ggsave("Meeting4/Normality_QQ-Plots.png",   plot = spec_qq_plot,   width = 10, height = 4, dpi = 300)