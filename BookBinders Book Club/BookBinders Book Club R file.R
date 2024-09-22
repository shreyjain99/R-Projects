#Clear memory
rm(list=ls())

#Read required packages
library("readr")
library("dplyr")
library("magrittr")
library("ggplot2")

#Reading the dataset
bbb = read_csv(" BookBinder_Book_Club.csv")
head(bbb)


###################### PART 1 #########################

#transforming the buyer and gender variables into a 0/1 dummy variable
bbb$gender <- ifelse(bbb$gender == 'M', 0, 1)
bbb$buyer <- ifelse(bbb$buyer == 'no', 0, 1)

#Logistic model
logistic_model <- glm(buyer ~ last + total_ + gender + child + youth + cook + do_it + reference + art + geog, 
                      data=bbb, 
                      family="binomial")

summary(logistic_model)


#Converting to Odds Ratio
exp(coef(logistic_model))

#Odds ratios and 95% Confidence Interval
exp(cbind(OR = coef(logistic_model), confint.default(logistic_model)))

#Calculating purchasing probability for every record
bbb$prob <- logistic_model %>% predict(bbb, type = "response")
head(bbb$prob)
head(bbb)

###################### PART 2 ########################


#Assigning each customer to a decile based on predicted probability
bbb$buckets <- ntile(desc(bbb$prob), 10)

#Bar chart plotting the average response rate by decile
ggplot(bbb, aes(x = factor(buckets), y = buyer)) + stat_summary(fun="mean", geom = "bar")

#table showing the number of customers, the number of buyers and the response rate to the offer by decile
table1 <- bbb %>% group_by(buckets) %>% summarise(total_cust = n(), total_buyer = sum(buyer), response_rate = mean(buyer), .groups = 'drop')
table1

#table showing the mean values of variables by probability of purchase decile
table2 <- bbb %>% group_by(buckets) %>% summarise(total_dollars = mean(total_),
                                                  months_since_last_purchase = mean(last),
                                                  mean_child_book = mean(child),
                                                  mean_youth_book = mean(youth),
                                                  mean_cook_book = mean(cook),
                                                  mean_do_it_book = mean(do_it),
                                                  mean_reference_book = mean(reference),
                                                  mean_art_book = mean(art),
                                                  mean_geog_book = mean(geog),
                                                  .groups = 'drop')
table2

#Plots for decile analysis
ggplot(bbb, aes(x = factor(buckets), y = last)) + stat_summary(fun="mean", geom = "bar")

ggplot(bbb, aes(x = factor(buckets), y = total_)) + stat_summary(fun="mean", geom = "bar")

ggplot(bbb, aes(x = factor(buckets), y = child)) + stat_summary(fun="mean", geom = "bar")

ggplot(bbb, aes(x = factor(buckets), y = youth)) + stat_summary(fun="mean", geom = "bar")

ggplot(bbb, aes(x = factor(buckets), y = cook)) + stat_summary(fun="mean", geom = "bar")

ggplot(bbb, aes(x = factor(buckets), y = reference)) + stat_summary(fun="mean", geom = "bar")

ggplot(bbb, aes(x = factor(buckets), y = art)) + stat_summary(fun="mean", geom = "bar")

ggplot(bbb, aes(x = factor(buckets), y = geog)) + stat_summary(fun="mean", geom = "bar")




###################### PART 3 ########################

# Break-even = Cost to mail/net revenue per sale = .5/(18-9-3)= 0.0833
breakeven_rr <- .5/(18-9-3)
breakeven_rr
                                                  
bbb$mailto_logit <- ifelse(bbb$prob >= breakeven_rr, 1, 0)                                                  
                                                  

table3 <- bbb %>% group_by(mailto_logit, buyer) %>% summarise(total_cust = n(), .groups = 'drop')
table3   

# targeting promotion mail = 12250 + 3323 = 15573 customers
table4 <- bbb %>% group_by(mailto_logit) %>% summarise(total_cust = n(), mean_res=mean(buyer), .groups = 'drop')
table4

overall_rr <- mean(bbb$buyer)
overall_rr


table5 <- bbb %>% group_by(mailto_logit) %>% summarise(total_cust = n(), buyer_cnt=sum(buyer), .groups = 'drop')
table5

expected_num_buyers <- 3323*10
expected_num_buyers
