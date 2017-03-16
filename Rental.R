packages <- c("jsonlite", "dplyr","purrr")

purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

#function to load data from json file
loaddata <- function(data){
  
  
  
  #unlist the variables
  vars <- setdiff(names(data), c("photos", "features"))
  data <- map_at(data, vars, unlist) %>% as_data_frame(.)
  
  
  #add new columns with the count for photos and features
  data$photosCount <- sapply(data$photos, length)
  data$featuresCount <- sapply(data$features, length)
  
  #convert date column to proper format
  data$created <- as.POSIXct(data$created)
  #extract day, month from the created field
  data$month <- month(data$created)
  data$date <- day(data$created)
  data$day <- weekdays(data$created)
  #add weekday column
  data$weekday <- 1
  data[which(data$day == "Saturday" | data$day =="Sunday"), "weekday"] <- 0 
  
  
  #substitute 0 building id with NA (Missing values)
  data[which(data$building_id==0),"building_id"] <- NA
  
  
  #return formatted data
  data
}

# function to convert data types
converttype <- function(data){
  #change data types for a few variables to factor
  data$manager_id <- as.factor(data$manager_id)
  data$building_id <- as.factor(data$building_id)
  data$day <- as.factor(data$day)
  data$month <- as.factor(data$month)
  data$weekday <- as.factor(data$weekday)
  
  data
}


#load train data
df.train <- fromJSON("train.json")
df.train <- loaddata(df.train)

#load test data
df.test <- fromJSON("test.json")
df.test <- loaddata(df.test)

#call function to convert types of variables
df.train <- converttype(df.train)
df.train$interest_level <- as.factor(df.train$interest_level)
df.test <- converttype(df.test)


#1st submission
library(nnet)
df.train$interest_level_2 <- relevel(df.train$interest_level, ref="low")

#multinomial logistic regression
modellogit <- nnet::multinom(interest_level_2 ~ price + photosCount + 
                               featuresCount+ bathrooms + bedrooms+
                               latitude + longitude + day + weekday + month, 
                             data = df.train)
summary(modellogit)
#get the predicted probabilities
logitPredictions <- predict(modellogit, df.test, type = "probs")

library(data.table)
logitPredictions <- as.data.table(logitPredictions)
#append listing_id to predictions
file <- data.table(listing_id = df.test$listing_id, logitPredictions[,.(high,medium,low)])
#write data to csv file
write.csv(file,"submission.csv")

