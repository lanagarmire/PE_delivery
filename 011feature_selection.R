# Feature selection with RF
load("final_data_v9_noNA.RData")
library(caret)
df2 = eope
X_var = which(colnames(df2) != "GESTATIONAL_AGE"&
                colnames(df2) != "time_to_deliver"&
                colnames(df2) != "eope")
x = df2[,X_var]
y = df2$GESTATIONAL_AGE
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
rf_results <- rfe(x,y,
                  sizes=c(5:25, 30, 35, 40), 
                  rfeControl=control)
save(rf_results, file = "rf_results.RData")
print(rf_results)
# list the chosen features
predictors(rf_results)
# plot the results
plot(rf_results, type=c("g", "o"))


# Feature selection using backward elimination
library(survival)
surv1 = coxph(Surv(time_to_deliver, rep(1, nrow(df2)))~.-GESTATIONAL_AGE, -eope, df2)

library(MASS)
surv_step = stepAIC(surv1, direction = "backward", trace = F)
summary(surv_step)
save(surv_step, file = "feature_selection.RData")