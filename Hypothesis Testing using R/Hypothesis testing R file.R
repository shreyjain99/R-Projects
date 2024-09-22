rm(list=ls())
cat("\014")


library(readxl)                              #IMPORT readxl LIBRARY
DF_GSS2018 <- read_excel("GSS2018(1).xlsx")  #READ THE DATA
View(DF_GSS2018) 


mean(DF_GSS2018$INCOME , na.rm = TRUE)   #CALCULATING MEAN OF INCOME VARIABLE

median(DF_GSS2018$INCOME , na.rm = TRUE) #CALCULATING MEDIAN OF INCOME VARIABLE

library(moments)                          #IMPORT moments LIBRARY
skewness(DF_GSS2018$INCOME, na.rm = TRUE) #CALCULATING SKEWNESS OF INCOME VARIABLE

hist(DF_GSS2018$INCOME,main= paste("Historgram of Income"), xlab = "Income") # PLOT HISTOGRAM FOR INCOME VARIABLE




range(DF_GSS2018$INCOME, na.rm = TRUE)   #CALCULATING RANGE OF INCOME VARIABLE

sd(DF_GSS2018$INCOME, na.rm = TRUE)   #CALCULATING STANDARD DEVIATION OF INCOME VARIABLE



#DATA
Time <- c(90, 73, 86, 85, 80, 87, 90, 78, 84, 71, 72, 88, 85, 65)
Mark <- c(68, 65, 58, 94, 76, 91, 62, 81, 75, 83, 85, 74, 93, 89)

cor(Time,Mark)    #CALCULATING CORRELATION OF TIME AND MARK
plot(Time, Mark, type = "p", lm(Time, Mark)) 



k <- seq(0,6)
df <- dbinom(k, size = 19, prob = 0.38)
df
sum(df)




dbinom(9, size = 19, prob = 0.38)





ppois(27,lambda = 33)



# LAMBDA = 16/100  * 18
ppois(14,lambda = 12.8)



pnorm(11700, mean = 12500, sd = 820)



qnorm(.03,mean = 12500, sd = 820)
