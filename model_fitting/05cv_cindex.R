#After obataining the predicted prognosis score from coxnnet using each training set,
# we load them to calculate c-index

library(glmnet)
library(survival)

#initiate cv c-index
k=5
c_index <- data.frame(coxnnet = rep(0,k), coxph = rep(0,k))


# calculate each cv c-index for coxnnet
for (i in 1:k){
  ytime_test_1 <- read.csv(sprintf("./ytime_test%d.csv", i), header = TRUE)
  ystatus_test_1 <- read.csv(sprintf("./ystatus_test%d.csv", i), header = TRUE)
  x_test_1 <- read.csv(sprintf("./x_test%d.csv", i), header = TRUE)
  #ytime_test <- as.matrix(ytime_test_1$x)
  #ystatus_test <- as.matrix(ystatus_test_1)
  y_test_1 <- data.frame(time=ytime_test_1$x, status=ystatus_test_1$x)
  y_test <- as.matrix(y_test_1)
  testdata2 <- data.frame(status=ystatus_test_1$x, time=ytime_test_1$x)
  testdata <- cbind(testdata2, x_test_1)
  
  #coxnnet_dadam_dropout
  theta_coxnnet_adam_dropout <- read.csv(sprintf("SRTR_theta_hcc_new%d.csv", i), header = FALSE)
  theta_coxnnet_adam_dropout <- as.matrix(theta_coxnnet_adam_dropout)
  cindex_coxnnet_adam_dropout <- Cindex(theta_coxnnet_adam_dropout, y_test)
  c_index$coxnnet[i] <- cindex_coxnnet_adam_dropout
  
}


# cv coxph 
for (i in 1:k){
  
  x_train_1 <- read.csv(sprintf("x_train%d.csv", i), header = TRUE)
  x_test_1 <- read.csv(sprintf("x_test%d.csv", i), header = TRUE)
  ytime_train_1 <- read.csv(sprintf("ytime_train%d.csv", i), header = TRUE)
  ytime_test_1 <- read.csv(sprintf("ytime_test%d.csv", i), header = TRUE)
  ystatus_train_1 <- read.csv(sprintf("ystatus_train%d.csv", i), header = TRUE)
  ystatus_test_1 <- read.csv(sprintf("ystatus_test%d.csv", i), header = TRUE)
  
  x_train <- as.matrix(x_train_1)
  x_test <- as.matrix(x_test_1)
  y_train_1 <- data.frame(time=ytime_train_1$x, status=ystatus_train_1$x)
  y_train <- as.matrix(y_train_1)
  ytime_train <- as.matrix(ytime_train_1$x)
  ystatus_train <- as.matrix(ystatus_train_1)
  y_test_1 <- data.frame(time=ytime_test_1$x, status=ystatus_test_1$x)
  y_test <- as.matrix(y_test_1)
  ytime_test <- as.matrix(ytime_test_1$x)
  ystatus_test <- as.matrix(ystatus_test_1)
  traindata2 <- data.frame(status=ystatus_train_1$x, time=ytime_train_1$x)
  traindata <- cbind(traindata2, x_train_1)
  testdata2 <- data.frame(status=ystatus_test_1$x, time=ytime_test_1$x)
  testdata <- cbind(testdata2, x_test_1)
  
  train_ridge <-cv.glmnet(x_train, y_train, nfolds = 5, family = "cox", alpha = 0)
  test_ridge <- predict(train_ridge, x_test,s=train_ridge$lambda.min,type='link')
  
  #c-index
  cindex_ridge <- Cindex(test_ridge, y_test)
  print(cindex_ridge)
  c_index$coxph[i] <- cindex_ridge
  
}


write.csv(c_index, 'c_index.csv', row.names = FALSE)

# calculate c-index for hold-out set
ytime_test_1 <- read.csv("./hold_out_y.csv", header = TRUE)
ystatus_test_1 <- read.csv("./hold_out_y_status.csv", header = TRUE)
x_test_1 <- read.csv("./hold_out_x.csv", header = TRUE)
#ytime_test <- as.matrix(ytime_test_1$x)
#ystatus_test <- as.matrix(ystatus_test_1)
y_test_1 <- data.frame(time=ytime_test_1$x, status=ystatus_test_1$x)
y_test <- as.matrix(y_test_1)
testdata2 <- data.frame(status=ystatus_test_1$x, time=ytime_test_1$x)
testdata <- cbind(testdata2, x_test_1)

#coxnnet_dadam_dropout
theta_coxnnet_adam_dropout <- read.csv("SRTR_theta_hcc_new_holdout2.csv", header = FALSE)
theta_coxnnet_adam_dropout <- as.matrix(theta_coxnnet_adam_dropout)
cindex_coxnnet_adam_dropout <- Cindex(theta_coxnnet_adam_dropout, y_test)
#c_index$coxnnet[i] <- cindex_coxnnet_adam_dropout

write.csv(cindex_coxnnet_adam_dropout, 'hold_out_cindex.csv', row.names = FALSE)