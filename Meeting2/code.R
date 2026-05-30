library(readr)
library(e1071)

#Data extracting
df <- read_delim("AirQualityUCI.csv", 
                 delim = ";", escape_double = FALSE, 
                 locale = locale(date_format = "%d/%m/%Y", 
                                 decimal_mark = ",", grouping_mark = "."), 
                 na = "empty", trim_ws = TRUE)


#Replace the -200 placeholder with NA
df[df == -200] <- NA

#Generate the Q-Q Plot
qqnorm(df$'T', 
       main = "Normal Q-Q Plot for T", 
       xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", 
       col = "steelblue", 
       pch = 1)

qqnorm(df$'RH', 
       main = "Normal Q-Q Plot for RH", 
       xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", 
       col = "steelblue", 
       pch = 1)

qqnorm(df$'CO(GT)', 
       main = "Normal Q-Q Plot for CO(GT)", 
       xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", 
       col = "steelblue", 
       pch = 1)

qqnorm(df$'PT08.S1(CO)', 
       main = "Normal Q-Q Plot for PT08.S1(CO)", 
       xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", 
       col = "steelblue", 
       pch = 1)

#Add the reference line to see how close it is to a normal distribution
qqline(df$'T', col = "red", lwd = 2)
qqline(df$'RH', col = "red", lwd = 2)
qqline(df$'CO(GT)', col = "red", lwd = 2)
qqline(df$'PT08.S1(CO)', col = "red", lwd = 2)

#Calculate skewness
t_skewness <- skewness(df$T, na.rm = TRUE)
cat("The skewness for Temperature (T) is:", t_skewness, "\n")

rh_skewness <- skewness(df$RH, na.rm = TRUE)
cat("The skewness for Relative Humidity (RH) is:", rh_skewness, "\n")

COGT_skewness <- skewness(df$CO(GT), na.rm = TRUE)
cat("The skewness for CO(GT) is:", COGT_skewness, "\n")

pt_skewness <- skewness(df$PT08.S1(CO), na.rm = TRUE)
cat("The skewness for PT08.S1(CO) is:", pt_skewness, "\n")
