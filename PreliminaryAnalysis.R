# ----------------- Reading the data ----------------
df_full <- read.csv("AirQualityUCI.csv", sep = ";", dec = ",", stringsAsFactors = FALSE, check.names = FALSE)

# ------------------ Selecting features --------------
my_cols <- c("Time", "T", "RH", "CO(GT)", "PT08.S1(CO)")
df <- df_full[, my_cols]

# -------------------- Data clean -------------
df[df == -200] <- NA
df$Time <- as.POSIXct(df$Time, format="%H.%M.%S")

# ------------------- Basic descrpitive stats(min, max, mean, NAs) -----------
str(df)
summary(df)

# ------------------------ Plots ------------------------
num_df <- df[, c("T", "RH", "CO(GT)", "PT08.S1(CO)")]

# Plot Histograms
par(mfrow = c(2, 2))
for(col_name in names(num_df)) {
  hist(num_df[[col_name]], 
       main = paste("Histogram of", col_name), 
       xlab = col_name, 
       col = "blue", 
       border = "white")
}



# Plot Boxplots
par(mfrow = c(1, 1))
boxplot(scale(num_df), 
        main = "Boxplots of Scaled Variables", 
        col = "lightblue", 
        las = 1)


# Correlation matrix, we ignore missing values
cor_matrix <- cor(num_df, use = "complete.obs")
print("Correlation Matrix:")
print(round(cor_matrix, 2))