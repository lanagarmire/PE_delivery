library(glmnet)
library(survival)


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