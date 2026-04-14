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


# 2. CREATING INTERVALS AND AGGREGATING
daily_analysis <- data_with_intervals %>%
  
  # B. Group by the newly created interval
  group_by(Interval) %>%
  
  # C. Calculate mean values for each metric
  summarise(
    Avg_Temp = mean(T, na.rm = TRUE),
    Avg_Humidity = mean(RH, na.rm = TRUE),
    Avg_CO = mean(`CO(GT)`, na.rm = TRUE),
    Avg_PT08 = mean(`PT08.S1(CO)`, na.rm = TRUE)
  ) %>%
  
  mutate(across(where(is.numeric), ~round(., 2)))

# Display the final summary
print(daily_analysis)
write_csv2(daily_analysis, "Meeting3/Daily_Analysis_Summary.csv")
write_xlsx(daily_analysis, "Meeting3/Daily_Analysis_Summary.xlsx")

# --- END MEAN CALCULATIONS ---

# --- BIG PLOTTING ---

hourly_trend <- selected_data %>%
  
  mutate(
    Hour = as.numeric(substr(Time, 1, 2)),
    Interval = case_when(
      Hour >= 0  & Hour <= 5  ~ "1. Night",
      Hour >= 6  & Hour <= 11 ~ "2. Morning",
      Hour >= 12 & Hour <= 17 ~ "3. Afternoon",
      Hour >= 18 & Hour <= 23 ~ "4. Evening"
    )
  ) %>%
  
  group_by(Hour, Interval) %>%
  summarise(
    Temp = mean(T, na.rm = TRUE),
    Humidity = mean(RH, na.rm = TRUE),
    `CO(GT)` = mean(`CO(GT)`, na.rm = TRUE),           
    `PT08.S1(CO)` = mean(`PT08.S1(CO)`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  pivot_longer(
    cols = c(Temp, Humidity, `CO(GT)`, `PT08.S1(CO)`), # FIX: Referencing the exact names
    names_to = "Variable",
    values_to = "Value"
  )

# 2. CREATING THE PLOT
my_plot <- ggplot(hourly_trend, aes(x = Hour, y = Value, color = Interval)) +
  # Add lines and points
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  
  # Split into 4 separate charts based on the 'Variable', letting Y-axis scale freely
  facet_wrap(~ Variable, scales = "free_y") +
  
  # Improve aesthetics
  theme_minimal() +
  scale_x_continuous(breaks = 0:23) + # Show every hour on X-axis
  labs(
    title = "Average Hourly Changes of Air Quality Parameters",
    subtitle = "Grouped by 6-hour intervals",
    x = "Hour of the Day (0-23)",
    y = "Average Value",
    color = "Time of Day"
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 12)
  )

# Display the plot
print(my_plot)
ggsave("Meeting3/Combined_AirQuality_Plot.png", plot = my_plot, width = 10, height = 8, dpi = 300)

# --- END BIG PLOTTING ---

# --- DETAILED PLOTTING ---

# ==========================================
# PLOT 1: TEMPERATURE
# ==========================================
plot_temp <- hourly_trend %>%
  filter(Variable == "Temp") %>%  # <--- Here we filter only Temperature data
  ggplot(aes(x = Hour, y = Value, color = Interval)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Average Hourly Temperature",
    x = "Hour of the Day",
    y = "Temperature (°C)",  # Custom unit for this specific plot
    color = "Time of Day"
  ) +
  theme(legend.position = "bottom")

# Display in RStudio
print(plot_temp)

# Save directly to your project folder
ggsave("Meeting3/Temperature.png", plot = plot_temp, width = 8, height = 5, dpi = 300)

# ==========================================
# PLOT 2: RELATIVE HUMIDITY
# ==========================================
plot_humidity <- hourly_trend %>%
  filter(Variable == "Humidity") %>%
  ggplot(aes(x = Hour, y = Value, color = Interval)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Average Hourly Relative Humidity",
    x = "Hour of the Day",
    y = "Relative Humidity (%)",
    color = "Time of Day"
  ) +
  theme(legend.position = "bottom")

print(plot_humidity)
ggsave("Meeting3/Humidity.png", plot = plot_humidity, width = 8, height = 5, dpi = 300)

# ==========================================
# PLOT 3: CARBON MONOXIDE CO(GT)
# ==========================================
plot_co <- hourly_trend %>%
  filter(Variable == "CO(GT)") %>%
  ggplot(aes(x = Hour, y = Value, color = Interval)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Average Hourly Carbon Monoxide Concentration - CO(GT)",
    x = "Hour of the Day",
    y = "CO Concentration (mg/m3)",
    color = "Time of Day"
  ) +
  theme(legend.position = "bottom")

print(plot_co)
ggsave("Meeting3/Carbon_Monoxide.png", plot = plot_co, width = 8, height = 5, dpi = 300)

# ==========================================
# PLOT 4: SENSOR PT08.S1(CO)
# ==========================================
plot_pt08 <- hourly_trend %>%
  filter(Variable == "PT08.S1(CO)") %>%
  ggplot(aes(x = Hour, y = Value, color = Interval)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Average Hourly Carbon Monoxide Concentration - PT08.S1(CO)",
    x = "Hour of the Day",
    y = "Sensor Response (Nominal)",
    color = "Time of Day"
  ) +
  theme(legend.position = "bottom")

print(plot_pt08)
ggsave("Meeting3/PT08_Sensor.png", plot = plot_pt08, width = 8, height = 5, dpi = 300)

# --- END DETAILED PLOTTING ---

# --- SMIRNOV TEST
ks_test_all_variables <- data_with_intervals %>%
  
  # 1. Select only the columns we want to test, plus the Interval
  select(Interval, T, RH, `CO(GT)`, `PT08.S1(CO)`) %>%
  
  # 2. Convert the table from wide to long format (perfect for batch processing)
  pivot_longer(
    cols = c(T, RH, `CO(GT)`, `PT08.S1(CO)`),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  
  # 3. Remove missing values so the test doesn't crash
  filter(!is.na(Value)) %>%
  
  # 4. Group by BOTH the time of day and the parameter name
  group_by(Interval, Variable) %>%
  
  # 5. Perform the K-S test for each specific group
  summarise(
    # We calculate only the p-value to keep the final table clean
    KS_p_value = ks.test(Value, "pnorm", mean(Value), sd(Value))$p.value,
    .groups = "drop"
  ) %>%
  
  # 6. Pivot back to wide format to make it look like a nice report table
  pivot_wider(
    names_from = Variable,
    values_from = KS_p_value
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

# Display the final summary table
print(ks_test_all_variables)
write_xlsx(ks_test_all_variables, "Meeting3/Kolmogorov_Smirnov_Results.xlsx")
write_csv2(ks_test_all_variables, "Meeting3/Kolmogorov_Smirnov_Results.csv")
# --- END SMIRNOV TEST