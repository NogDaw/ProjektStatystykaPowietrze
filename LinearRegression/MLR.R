#Load Data
df <- read.csv("AirQualityUCI.csv", sep = ";", dec = ",", stringsAsFactors = FALSE, check.names = FALSE)
df[df == -200] <- NA
mlr_data <- na.omit(df[, c("CO(GT)", "PT08.S1(CO)", "T", "RH")])

#Build the Multiple Linear Regression Model, we predict CO based on PT08 sensor + Temperature + Humidity
mlr_model <- lm(`CO(GT)` ~ `PT08.S1(CO)` + T + RH, data = mlr_data)

#Print the results
cat("--- Multiple Linear Regression Results ---\n")
summary(mlr_model)
