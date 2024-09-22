library(tidyverse)
library(ggplot2)
library(dplyr)


cell <- read.csv("Customer churn callibration data file.csv", header = TRUE)

Total_No_Customers <- nrow(cell)
cat("Number of customers:", Total_No_Customers, "\n")
# Total number of customers is 69626


Total_calib <- sum(cell$calibrat == 1)
Total_valid <- sum(cell$calibrat == 0)

cat("Number of customers in the calibration set is", Total_calib, "\n")
cat("Number of customers in the validation set is", Total_valid, "\n")
# Number of customers in the calibration set is 39186 
# Number of customers in the validation set is 30440 


calib_churn_rate <- round(sum(cell$churn[cell$calibrat == 1] == 1) / Total_calib, 4)
cat("Churn rate in the calibration set is", calib_churn_rate, "\n")
# Churn rate in the calibration set is 0.5%


valid_churn_rate <- round(sum(cell$churn[cell$calibrat == 0] == 1) / Total_valid, 4)
cat("Churn rate in the validation set is", valid_churn_rate, "\n")
# Churn rate in the validation set is 


new_cell <- cell %>%
  select(churndep, revenue:retcall, calibrat)


training_data <- new_cell %>%
  filter(calibrat == 1) %>%
  select(-calibrat)

validation_data <- new_cell %>%
  filter(calibrat == 0) %>%
  select(-calibrat)



Logit_model <- glm(churndep ~ ., data = training_data, family = "binomial")
summary(Logit_model)

odds_ratio <- exp(coef(Logit_model)); odds_ratio
p_value <- summary(Logit_model)$coefficients[-1, "Pr(>|z|)"] ; p_value

odds_ratio_and_pvalue <- data.frame(OddsRatio = odds_ratio[-1], PValue = p_value); odds_ratio_and_pvalue

Top_2_high <- head(arrange(odds_ratio_and_pvalue, desc(OddsRatio)), 2); Top_2_high
Top_2_low <- head(arrange(odds_ratio_and_pvalue, OddsRatio), 2); Top_2_low

cat("Two Variables with the largest Odds Ratios is")
print(Top_2_high)

cat("Two Variables with the smallest Odds Ratios is")
print(Top_2_low)


validation_data$attri_prob <- predict(Logit_model, newdata = validation_data, type = "response")
validation_data_desc <- validation_data[order(-validation_data$attri_prob), ]

Top_5_high_attri_probs <- head(validation_data_desc$attri_prob, 5)

cat("Top five Highest Attrition Probabilities are")
print(Top_5_high_attri_probs)


OR <- exp(coef(Logit_model)); OR


pvalues <- coef(summary(Logit_model))[,'Pr(>|z|)'] ; pvalues


df1 <- data.frame(OddsRatio = OR, PValues = pvalues); df1


print(df1)


sd_calibration <- sapply(training_data, sd); sd_calibration


missing_sd <- is.na(sd_calibration); missing_sd


sd_function_na_rm <- function(x) {sd(x, na.rm = TRUE)}; sd_function_na_rm

sd_calibration_new <- sapply(training_data, sd_function_na_rm); sd_calibration_new


df2 <- data.frame( StandardDeviation_new = sd_calibration_new)
print(df2)




df1$VarName <- row.names(df1); df1
df2$VarName <- row.names(df2); df2


merged_df <- merge(df1, df2, by = "VarName", all = FALSE)
print(merged_df)



merged_df[, sapply(merged_df, is.numeric)] <- round(merged_df[, sapply(merged_df, is.numeric)], 5)
merged_df

Importance <- merged_df[merged_df$PValues < 0.05, ]
print(Importance)


write.csv(Importance, file = "Importance.csv", row.names = FALSE)