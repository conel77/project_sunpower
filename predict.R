install.packages('dplyr')
install.packages('zoo')
install.packages("caret")
install.packages("tidyverse")
install.packages('lubridate')
options(timeout=100)
install.packages("lightgbm", repos = "https://cran.r-project.org")
library(dplyr)
library(zoo)
library("lightgbm")
library('tidyverse')
library('lubridate')

setwd("/Users/shinminkyoung/Desktop/energy")

train <- read.csv("train.csv", header = T, fileEncoding = "EUC-KR")
test <- read.csv("test.csv", header = T, fileEncoding = "EUC-KR")

for (i in seq(0, length(test), length(test)%%60)){
  test[is.na(test)] = 0
}


#???ê°? ?????? ì£¼ë????¬ë?? ì¶?ê°?
timest <- list(time_train= train[[2]], time_test = test[[2]])
d_time_train = substr(timest$time_train,12,13)
d_time_test = substr(timest$time_test, 12,13)
train[,"time"] <- d_time_train
test[,"time"] <- d_time_test


w_time_train = substr(timest$time_train,1,11)
w_time_test = substr(timest$time_test,1,11)


w_time_trains <- wday(w_time_train)
w_time_tests <- wday(w_time_test)
train[,"weekday"] <- w_time_trains
test[,"weekday"] <- w_time_tests


weekday_w_train = ifelse(w_time_trains == 7,1,ifelse(w_time_trains ==1,1,0) )
weekday_w_test = ifelse(w_time_tests == 7,1,ifelse(w_time_tests ==1,1,0) )
train[,"weekend"] <- weekday_w_train
test[,"weekend"] <- weekday_w_test
#??????= 2~6, ì£¼ë?? = 1,7

y_train <- train[,3]
x_train_ <- train[,-3]
x_train <- x_train_[,-2]
y_test <- train[,3]
x_test_ <- train[,-3]
x_test <- x_test_[,-2]

trainIndex <- caret::createDataPartition(x_train$time,p=0.75,list=FALSE)
trainIndex <- caret::createDataPartition(x_test$time,p=0.75,list=FALSE)

data_train_x <- x_train[trainIndex,] #training data (75% of data)
data_test_x <- x_train[-trainIndex,] #testing data (25% of data)
data_train_y <- y_train[trainIndex]
data_test_y <- y_train[-trainIndex]

test = approx(x,y, method="linear")


lgb.grid = list(objective = "binary",
                metric = "auc",force_col_wise=TRUE)

lgb.train = lgb.Dataset(data=data.matrix(data_train_x), label = data.matrix(data_train_y))
lgb.test = lgb.Dataset(data=data.matrix(data_test_x), label = data.matrix(data_test_y))
best.iter = 525

dtrain <- lgb.Dataset(data = as.matrix(as.numeric(unlist(train))))
dtest <- lgb.Dataset(data = as.matrix(as.numeric(unlist(test))))
lgb.model = lgb.train(params = lgb.grid, data = lgb.train,nrounds = best.iter)
lgb.model_test = lgb.train(params= lgb.grid, data = lgb.test, nrounds = best.iter)

lgb.importance(model = lgb.model)
lgb.plot.importance(tree_imp = lgb.importance(lgb.model, percentage = TRUE),measure = "Gain")
lgb.plot.interpretation(tree_interpretation_dt = lgb.importance(lgb.model, percentage = TRUE))

lgb.importance(model = lgb.model_test)
lgb.plot.importance(tree_imp = lgb.importance(lgb.model_test, percentage = TRUE),measure = "Gain")
lgb.plot.interpretation(tree_interpretation_dt = lgb.importance(lgb.model_test, percentage = TRUE))
