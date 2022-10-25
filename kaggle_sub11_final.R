---
title: "Kaggle project"
author: "Ferra Suryani"
date: "11/10/2021"


## PRELIMINARY CODE


install.packages('ggplot2')
install.packages('skimr')
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library(skimr)
install.packages('GGally')
install.packages('tm')
install.packages("readr")
library(GGally)


## DATA PEPARATION
scoring_data <- read.csv('scoringData.csv', header = TRUE)
analysis_data <- read.csv('analysisData.csv', header = TRUE)

analysisData <- analysis_data %>% mutate(type = "analysis")
scoringData <- scoring_data %>% mutate(type = "scoring")

combinedData <- bind_rows(analysisData, scoringData)
data <- combinedData %>% select(availability_365, bedrooms, bathrooms, 
                                accommodates, number_of_reviews, cleaning_fee, security_deposit, 
                                bed_type, host_has_profile_pic, neighbourhood_cleansed, square_feet, 
                                host_since, zipcode, first_review, last_review, host_is_superhost, 
                                property_type, amenities, room_type, neighbourhood_group_cleansed, 
                                price, review_scores_rating)

### check missing data
missing_col_data = colSums(is.na(data)); missing_col_data[missing_col_data > 0]

## DATA MANIPULATION

### bed_type
data$bed_type <- as.factor(data$bed_type)

### host_has_profile_pic
data$host_has_profile_pic <- as.factor(data$host_has_profile_pic)


### neighbourhood_cleansed
data$neighbourhood_cleansed <- as.factor(data$neighbourhood_cleansed)

### host is superhost
data$host_is_superhost <- as.factor(data$host_is_superhost)

### property type
data$property_type <- as.factor(data$property_type)

### room_type
data$room_type <- as.factor(data$room_type)

### neighbourhood_group_cleansed
data$neighbourhood_group_cleansed <- as.factor(data$neighbourhood_group_cleansed)

### transforming date 

#host_since_length
data$host_since = as.Date(data$host_since)
data$host_length = as.integer(as.Date("2018-11-29") - data$host_since)

#since_first_review
data$first_review = as.Date(data$first_review)
data$since_first_review = as.integer(as.Date("2018-11-29") - data$first_review)

#since_last_review
data$last_review = as.Date(data$last_review)
data$since_last_review = as.integer(as.Date("2018-11-29") - data$last_review)

### transforming zipcode
table(data$zipcode)
unique(combinedData$zipcode)

data$zipcode[data$zipcode=="11103-3233"] = 11103
data$zipcode[data$zipcode=="11249\n11249"] = 11249
data$zipcode[data$zipcode=="10003-8623"] = 10003
data$zipcode[data$zipcode=="11413-3220"] = 11413
data$zipcode[data$zipcode=="10065"] = 10021
data$zipcode[data$zipcode==11249] = 11211
data$zipcode[data$zipcode=="11249"] = 11211
data$zipcode = as.factor(data$zipcode)
str(data)

### check missing values
missing_data <- apply(data,2,function(x) sum(is.na(x))) 

### impute missing values on host_is_superhost
data$host_is_superhost[is.na(data$host_is_superhost)] <- "f"

### impute missing values on cleaning_fee with mean value
data$cleaning_fee[is.na(data$cleaning_fee)] <- mean(data$cleaning_fee, na.rm=TRUE)

### impute NA value in square feet with median value
data$square_feet[is.na(data$square_feet)] <- median(data$square_feet, na.rm=TRUE)

### impute missing values on zipcode
data$zipcode <- as.character(data$zipcode)
data$zipcode[is.na(data$zipcode)]<- "0"
data$zipcode <- as.factor(data$zipcode)

str(data)
missing_data

### impute more missing values
data$host_length[is.na(data$host_length)] <- mean(data$host_length, na.rm=TRUE)
data$since_first_review[is.na(data$since_first_review)] <- mean(data$since_first_review, na.rm=TRUE)
data$since_last_review[is.na(data$since_last_review)] <- mean(data$since_last_review, na.rm=TRUE)
data$security_deposit[is.na(data$security_deposit)] <- mean(data$security_deposit, na.rm=TRUE)

### amenities
amenities_split <- strsplit(as.character(analysis$amenities), ',')
amenities_unique <- sort(unique(unlist(amenities_split)))
amenities_unique <- str_replace_all(string=amenities_unique, pattern=" ", repl="")
amenities_unique <- gsub("\\.", "", amenities_unique)
unique(amenities_unique)

### creating dummy variables
data$Pool <- as.integer(str_detect(data$amenities, "Pool"))
data$Beachfront <- as.integer(str_detect(data$amenities, "Beachfront"))
data$Indoorfireplace <- as.integer(str_detect(data$amenities, "Indoorfireplace"))
data$Gym <- as.integer(str_detect(data$amenities, "Gym"))
data$Elevator <- as.integer(str_detect(data$amenities, "Elevator"))
data$Smartlock <- as.integer(str_detect(data$amenities, "Smartlock"))
data$Breakfast <- as.integer(str_detect(data$amenities, "Breakfast"))
data$Washer_and_Dryer <- as.integer(str_detect(data$amenities, "Washer_and_Dryer"))

str(data)

### setting up the datasets

used_vars <- c("Washer_and_Dryer", "Breakfast", "Indoorfireplace", "Gym", "Elevator", "Smartlock", "Pool", "Beachfront", "availability_365", "bedrooms", "bathrooms", "host_length", "since_first_review", 
               "since_last_review", "accommodates", "number_of_reviews", "cleaning_fee", 
               "security_deposit","bed_type", "host_has_profile_pic", "neighbourhood_cleansed",
               "square_feet","zipcode", "host_is_superhost","property_type", "room_type",
               "neighbourhood_group_cleansed","price", "review_scores_rating")
data_vars <- subset(data, select = used_vars)
str(data_vars)

### 
analysis <- data_vars[!is.na(data_vars$price),]
scoring <- data_vars[is.na(data_vars$price),]

### split the data into train and test
library(caret)
set.seed(100)
split <- createDataPartition(y = analysis$price, p = 0.8,list = F,groups= 2066) # groups are ~5% of total number of rows
train <- analysis[split,]
test <- analysis[-split,]

missing <- apply(analysis,2,function(x) sum(is.na(x)))
missing


## Variables selection with lasso 
library(glmnet)
x = model.matrix(price~.-1, data = analysis)
y = analysis$price
cv_lasso = cv.glmnet(x,y,alpha=1)
coef(cv.lasso)
plot(cv_lasso)

cv_lasso
cv_lasso$lambda.min # lowest mean square error
coef(cv_lasso, s = cv_lasso$lambda.1se) %>%
  round(4)

str(analysis)

missing_data <- apply(analysis,2,function(x) sum(is.na(x)))
missing_data

## BUILDING THE MODEL

### boosting
library(gbm)
boost <- gbm(price~.,
            data=train,
            distribution="gaussian",
            n.trees = 10000,
            interaction.depth = 5,
            shrinkage = 0.005,
            n.minobsinnode = 5)

### predicting train dataset
pred_train <- predict(boost, n.trees=10000)
rmse_train_boost <- sqrt(mean((pred_train - train$price)^2))
rmse_train_boost

### predicting test dataset
pred_test <- predict(boost, newdata = test, n.trees = 10000)
rmse_test_boost <- sqrt(mean((pred_test - test$price)^2))
rmse_test_boost

### predicting scoring dataset
pred_boost_scoring <- predict(boost, newdata = scoring, n.trees = 10000)
rmse_scoring <- sqrt(mean((pred_test - test$price)^2))
rmse_scoring


## convert results into csv
submissionFile <- data.frame(id = scoringData$id, price = pred_boost_scoring)
head(submissionFile)
write.csv(submissionFile, 'submission8.csv',row.names = F)

## CONCLUSION