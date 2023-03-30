#07 fill NAs and one-hot-encoding

load("final_data_v7.RData")
library(missForest)
library(dplyr)
final_data_v7 = final_data_v7%>%ungroup()
final_data_v7 = final_data_v7%>%
  select(-DeID_PatientID, -DeID_EncounterID,  -"Diag_EncounterID", -"TermCodeMapped",                             
         -"TermNameMapped",-"DeID_DischargeDate.x",-"DeID_PREGNANCY_EPISODE_ID",-"DeID_Episode_Start_Date",
         - "DeID_Episode_End_Date", -"DeID_Ob_Wrk_Edd_Dt",-"DeID_Last_Menstrual_Period", -"DeID_EncounterID.y",                         
         -"DeID_PatientID_Baby",                        
         -"DeID_EncounterID_Baby",                      
         -"DeID_DELIVERY_EPISODE_ID",-"DeID_Delivery_Time",-"algorithmName",-"TotalScore",-"GenderCode",                                 
         -"DeID_DOB",-"ReligionName",-"diff", -"Social_Date",-"Diag_Date",-"BABY_SEX")


int_var = c(5:8)
for (i in int_var){
  final_data_v7[,i] = as.integer(final_data_v7[,i])
}

final_data_v7$age = as.integer(final_data_v7$age)
final_data_v7$eope = as.integer(final_data_v7$eope)
final_data_v7$SeverePE = as.integer(final_data_v7$SeverePE)
#final_data_v5_rf = missForest(as.matrix(final_data_v5))


library(caret)
dummy <- dummyVars(" ~ .", data=final_data_v7)
final_data_pro <- data.frame(predict(dummy, newdata = final_data_v7)) 
final_data_pro = final_data_pro%>%filter(RaceName == 0 &RaceNamePatient.Refused == 0)
final_data_pro = final_data_pro%>%
  select(- SmokingStatusSourceUnknown, -IllegalDrugUserStatusSourceUnknown,-SexuallyActiveStatusSourceUnknown,
         -RaceName, -RaceNameUnknown, -RaceNamePatient.Refused)
final_data_pro= final_data_pro%>%
  select(-"AlcoholUseStatusSourceUnknown", -"MaritalStatusNameSourceUnknown",
         -"RaceNameOther", -"EthnicityCodeSourceUNKNOWN")
for(i in c(1:nrow(final_data_pro))){
  if(final_data_pro$time_to_deliver[i] == 0){
    final_data_pro$time_to_deliver[i] = 0.5
  }
}

#scale numeric variables
num_var = c("diag_GA","EPIS_GRAVIDA_COUNT", "EPIS_PARA_COUNT", "NUMBER_OF_FETUSES","age","diag_GA", "creatinine","Hematocrit","Hemoglobin",)
for(i in c(1:ncol(final_data_pro))){
  if(colnames(final_data_pro)[i]%in%num_var){
    final_data_pro[,i] = scale(final_data_pro[,i], center = F)
  }
}

final_data_pro = final_data_pro%>%filter(!is.na(EPIS_GRAVIDA_COUNT)&!is.na(EPIS_PARA_COUNT)&!is.na(NUMBER_OF_FETUSES))

save(final_data_pro, file = "final_data_pro.RData")

