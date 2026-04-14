library(readr)
library(e1071)

# Data extracting
df <- read_delim("C:/Users/Kamil/Desktop/AirQualityUCI.csv", 
                 delim = ";", escape_double = FALSE, 
                 locale = locale(date_format = "%d/%m/%Y", 
                                 decimal_mark = ",", grouping_mark = "."), 
                 na = "empty", trim_ws = TRUE)


# Replace the -200 placeholder with NA for the column you want to plot
df$'RH'[df$'RH' == -200] <- NA

# Generate the Q-Q Plot
qqnorm(df$'RH', 
       main = "Normal Q-Q Plot for RH", 
       xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", 
       col = "steelblue", 
       pch = 1)

# Add the reference line to see how close it is to a normal distribution
qqline(df$'RH', col = "red", lwd = 2)

# Calculate skewness
rh_skewness <- skewness(df$RH, na.rm = TRUE)
cat("The skewness for Relative Humidity (RH) is:", rh_skewness, "\n")
