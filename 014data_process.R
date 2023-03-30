# Data Preparation for EOPE submodel
library(dplyr)
load("final_data_v9_complete.RData")
df = eope_0dBP
# 1 Remove sparse variables
dim(df)# 374 79
nonsparse_var = colnames(df)[colSums(df) >= 10]
df = df[, colnames(df)%in%nonsparse_var]

dim(df)# 374 67

# 2. Remove Hypertension
df = df%>%select(-HypertensionComplicated, -HypertensionUncomplicated)

#3. Remove illegal drug no
df = df%>%select(-IllegalDrugUserStatusSource.No)

#4. Remove highly correlated variables
df2 = df
library(survival)
library(rms)
surv1 = coxph(Surv(time_to_deliver, rep(1, nrow(df2)))~.,  df2)
#detect multicollinearity
cvif = vif(surv1)
cvif
#remove highly correlated features
df2= df2%>%select(-Mean_Corpuscular_Hgb)
surv2 = coxph(Surv(time_to_deliver, rep(1, nrow(df2)))~.,  df2)
vif(surv2)
df2 = df2%>%select(-Red_Blood_Cell_Count)
surv3 = coxph(Surv(time_to_deliver, rep(1, nrow(df2)))~.,  df2)
vif(surv3)
df2 = df2%>%select(-Hemoglobin)
surv4 = coxph(Surv(time_to_deliver, rep(1, nrow(df2)))~.,  df2)
vif(surv4)

df2 = df2%>%select(-BPSysMax)
surv6 = coxph(Surv(time_to_deliver, rep(1, nrow(df2)))~.,  df2)
vif(surv6)
df2 = df2%>%select(-ALT)
surv7 = coxph(Surv(time_to_deliver, rep(1, nrow(df2)))~.,  df2)
vif(surv7)
#df2 = df2%>%select(-BPDiaMax, -BPDiaMin)
df2 = df2%>%select(-BPDiaMax)
surv9 = coxph(Surv(time_to_deliver, rep(1, nrow(df2)))~.,  df2)
vif(surv9)
df2 = df2%>%select(-BPSysMin)
surv10 = coxph(Surv(time_to_deliver, rep(1, nrow(df2)))~.,  df2)
vif(surv10)
df2 = df2%>%select(-BPDiaMin)
surv10 = coxph(Surv(time_to_deliver, rep(1, nrow(df2)))~.,  df2)
vif(surv10)
df2 = df2%>%select(-RRMax)
surv5 = coxph(Surv(time_to_deliver, rep(1, nrow(df2)))~.,  df2)
vif(surv5)
df2 = df2%>%select(-RRMin)
surv5 = coxph(Surv(time_to_deliver, rep(1, nrow(df2)))~.,  df2)
vif(surv5)


surv11 = coxph(Surv(time_to_deliver, rep(1, nrow(df2)))~. -EPIS_GRAVIDA_COUNT,  df2)
summary(surv11)


cox1 = coxph(Surv(time_to_deliver, rep(1, nrow(eope)))~., df2)
summary(cox1)

#4. log_transformation----------------------
library(moments)
apply(df2,2, skewness)
colnames(df2)[skewness(df2)>2]

df2$AST= log(df2$AST)
df2$creatinine_value = log(df2$creatinine_value)
#df2$RRMean = log(df2$RRMean)
df2$RRSD = log(df2$RRSD)
df2$Red_Cell_Distribution_Width = log(df2$Red_Cell_Distribution_Width)
df2$n_BP= log(df2$n_BP)
df2$RRSD[is.infinite(df2$RRSD)] =  -1


#5. scale------------------------
for(i in c(4,5,7,34:50)){
  df2[,i] = scale(df2[,i], center = F)
}

save(df2, file = "eope0dBP_cleaned.RData")