#Loading data
df <- read.csv("AirQualityUCI.csv", sep = ";", dec = ",", stringsAsFactors = FALSE, check.names = FALSE)
df[df == -200] <- NA

df$Hour <- as.numeric(substr(df$Time, 1, 2))

#Cleaning data
num_df <- df[, c("Hour", "T", "RH", "CO(GT)", "PT08.S1(CO)")]
clean_df <- na.omit(num_df)

#Grouping
clean_df$Interval <- cut(clean_df$Hour, 
                         breaks = c(-1, 5, 11, 17, 23), 
                         labels = c("1. Night", "2. Morning", "3. Afternoon", "4. Evening"))

#Analysis
intervals <- levels(clean_df$Interval)

for(int in intervals) {
  #Subset data for the specific interval
  subset_data <- subset(clean_df, Interval == int)
  
  #Chosing only variabless for matrix calc
  vars_only <- subset_data[, c("T", "RH", "CO(GT)", "PT08.S1(CO)")]
  
  cat("\n======================================================\n")
  cat("RESULTS FOR:", int, "(n =", nrow(subset_data), ")\n")
  cat("======================================================\n")
  
  #Correlation Matrix (Using Spearman method)
  cor_matrix <- cor(vars_only, method = "spearman")
  cat("--- Correlation Matrix (Spearman) ---\n")
  print(round(cor_matrix, 2))
  
  #formal corr test for 2 CO sensors
  cat("\n--- Formal Correlation Test: CO(GT) vs PT08.S1(CO) ---\n")
  formal_test <- suppressWarnings(cor.test(subset_data$"CO(GT)", subset_data$"PT08.S1(CO)", method = "spearman"))
  print(formal_test)
}

