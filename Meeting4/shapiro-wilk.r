# 1. Loading data
df <- read.csv2("~/Desktop/AirQualityUCI.csv", na.strings = c("empty", "-200"))

#Deleting empty rows
rh_clean <- na.omit(df$PT08.S1)

#Randomly selecting sample for Shapiro-Wilk test
set.seed(42)
rh_sample <- sample(rh_clean, 5000)

#Performing the test and showing the results
wynik_testu <- shapiro.test(rh_sample)
print(wynik_testu)
