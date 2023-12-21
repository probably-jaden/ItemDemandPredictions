#Import required Packages
library(dplyr)
library(tidyr)
library(forecast)
library(ggplot2)
library(lubridate)

ring function
SMAPE <- function(actual, predict) {
  mean(abs(actual - predict)/((actual + predict)/2))
}

# Read data
train <- read.csv("../input/train.csv")
train$date <- as.Date(train$date)

# Separate into training and testing set
train_1_1 <- train %>% filter(store==1 & item==1) %>% select(date, sales)
train_1_1_train <- train_1_1[1:1461,]
train_1_1_test <- train_1_1[1462:1826,]

# Check if there is seasonal pattern Quarter over Quater
QoQ <- train_1_1 %>% group_by(date=floor_date(date, "quarter")) %>% summarize(sales=mean(sales))
acf(QoQ$sales)

# Check if there is seasonal pattern Month over Month
MoM <- train_1_1 %>% group_by(date=floor_date(date, "month")) %>% summarize(sales=mean(sales))
acf(MoM$sales)

# Check if there is seasonal pattern Day over Day
DoD <- train_1_1 %>% group_by(date=floor_date(date, "day")) %>% summarize(sales=mean(sales))
acf(DoD$sales)

# build the model and do forecast
mod <- tbats(train_1_1_train$sales, seasonal.periods = c(7,365.25))
fc <- forecast(mod, h=365)
acc <- SMAPE(train_1_1_test$sales, fc$mean)

# create a forecasted dataset
train_1_1_train_fitted <- train_1_1_test
train_1_1_train_fitted$sales <- as.numeric(fc$mean)

#plot the graph
ggplot(data=train_1_1) +
  geom_line(data=train_1_1_test, aes(x=date, y=sales, group=1, col='r')) +
  geom_line(data=train_1_1_train_fitted, aes(x=date, y=sales, group=1, col='b')) +
  scale_color_discrete(name = 'source', labels = c('actual', 'forecast'))

# SMAPE for store_1, item_1 prediction
cat("SMAPE of store-1-item-1 sales: ", toString(acc))

# Nest the data into every store-item pair
nested_train <- nest(train, -c(store,item))

# Creat a dataframe to store overall result
result_all = data.frame(matrix(ncol=6, nrow=0))
names(result_all) = c('date', 'actual', 'forecast', 'store', 'item', 'SMAPE')

# Get and check unique result
unique_result <- result_all %>% group_by(store, item) %>% summarise(SMAPE=mean(SMAPE))
head(unique_result)

# Check the SMAPE of the result
print(SMAPE(result_all$actual, result_all$forecast))

unique_result_by_store <- unique_result %>% group_by(store) %>% summarise(SMAPE=mean(SMAPE))
ggplot(data=unique_result_by_store, aes(x=reorder(store, -SMAPE), y=SMAPE, fill=SMAPE)) + 
  geom_bar(stat='identity') +
  xlab('store')