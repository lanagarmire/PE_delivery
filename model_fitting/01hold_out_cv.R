##reduced model validation
library(glmnet)
library(survival)
library(dplyr)

load("eope_base.RData")
hold_out_percent = 0.2
data = eope_base

# split hold-out testing set
hold_out_ind = sample(1:nrow(data), nrow(data)*hold_out_percent, replace = F)
hold_out = data[hold_out_ind, ]
cv_set = data[-hold_out_ind, ]

write.csv(hold_out, file = "hold_out_set.csv", row.names = FALSE)
write.csv(cv_set, file = "cv_set.csv", row.names = FALSE)

hold_out_x <- hold_out%>%select(-time_to_deliver)
hold_out_y <- hold_out$time_to_deliver
hold_out_y_status <- rep(1, nrow(hold_out))
write.csv(hold_out_x, file = "hold_out_x.csv", row.names = FALSE)
write.csv(hold_out_y, file = "hold_out_y.csv", row.names = FALSE)
write.csv(hold_out_y_status, file = "hold_out_y_status.csv", row.names = FALSE)

cv_x <- cv_set%>%select(-time_to_deliver)
cv_y <- cv_set$time_to_deliver
cv_y_status <- rep(1, nrow(cv_set))
write.csv(cv_x, file = "cv_x.csv", row.names = FALSE)
write.csv(cv_y, file = "cv_y.csv", row.names = FALSE)
write.csv(cv_y_status, file = "cv_y_status.csv", row.names = FALSE)

# 5-fold cross validation
k = 5 #Folds

# sample from 1 to k, nrow times (the number of observations in the data)
cv_set$id <- sample(1:k, nrow(cv_set), replace = TRUE)
list <- 1:k

for (i in 1:k){
  # remove rows with id i from dataframe to create training set
  # select rows with id i to create test set
  train <- subset(cv_set, id %in% list[-i])
  test <- subset(cv_set, id %in% c(i))
  
  # use gestational age as y 
  x_train <- train%>%select(-time_to_deliver, -id)
  ytime_train <- train$time_to_deliver
  ystatus_train <- rep(1, nrow(train))
  
  x_test <- test%>%select(-time_to_deliver, -id)
  ytime_test <- test$time_to_deliver
  ystatus_test <- rep(1,nrow(test))
  
  # produce splited for coxnnet training
  write.csv(x_train, file = sprintf("x_train%d.csv", i), row.names = FALSE)
  write.csv(ytime_train, file = sprintf("ytime_train%d.csv", i), row.names = FALSE)
  write.csv(ystatus_train, file = sprintf("ystatus_train%d.csv", i), row.names = FALSE)
  write.csv(x_test, file = sprintf("x_test%d.csv", i), row.names = FALSE)
  write.csv(ytime_test, file = sprintf("ytime_test%d.csv", i), row.names = FALSE)
  write.csv(ystatus_test, file = sprintf("ystatus_test%d.csv", i), row.names = FALSE)
}







  