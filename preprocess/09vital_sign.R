# Vital Signs
# import standard vital signs
library(dplyr)
load("final_data_v7.RData")
stdVital=  read.csv("NursingStandardVitalSigns.csv")
stdVital$DeID_ObservationDate = as.Date(stdVital$DeID_ObservationDate, format = "%m/%d/%Y %H:%M")

BPDiaNonInvasive = stdVital%>%
  inner_join(final_data_v7,by=c("DeID_PatientID"))%>%
  filter(DeID_ObservationDate<= Diag_Date & DeID_ObservationDate > Diag_Date-5)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(BPDiaMax = max(BPDiaNonInvasive, na.rm = T), 
            BPDiaMin = min(BPDiaNonInvasive, na.rm = T), 
            BPDiaMean = mean(BPDiaNonInvasive, na.rm = T), 
            BPDiaSD = sd(BPDiaNonInvasive, na.rm = T))
BPDiaNonInvasive$BPDiaSD[is.na(BPDiaNonInvasive$BPDiaSD)] = 0

BPSysNonInvasive = stdVital%>%
  inner_join(final_data_v7,by=c("DeID_PatientID"))%>%
  filter(DeID_ObservationDate<= Diag_Date & DeID_ObservationDate > Diag_Date-5)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(BPSysMax = max(BPSysNonInvasive, na.rm = T), 
            BPSysMin = min(BPSysNonInvasive, na.rm = T), 
            BPSysMean = mean(BPSysNonInvasive , na.rm = T), 
            BPSysSD = sd(BPSysNonInvasive , na.rm = T))%>%
  select(DeID_PREGNANCY_EPISODE_ID, BPSysMax, BPSysMin, BPSysMean, BPSysSD)
BPSysNonInvasive$BPSysSD[is.na(BPSysNonInvasive$BPSysSD)] = 0

# SpO2 = stdVital%>%
#   inner_join(final_data_v7,by=c("DeID_PatientID"))%>%
#   filter(DeID_ObservationDate<= Diag_Date & DeID_ObservationDate > Diag_Date-10)%>%
#   group_by(DeID_PREGNANCY_EPISODE_ID)%>%
#   summarise(SpO2Max = max(SpO2, na.rm = T), 
#             SpO2Min = min(SpO2, na.rm = T), 
#             SpO2Mean = mean(SpO2 , na.rm = T), 
#             SpO2SD = sd(SpO2 , na.rm = T))
# 
# RespiratoryRate=stdVital%>%
#   inner_join(final_data_v7,by=c("DeID_PatientID"))%>%
#   filter(DeID_ObservationDate<= Diag_Date & DeID_ObservationDate > Diag_Date-10)%>%
#   group_by(DeID_PREGNANCY_EPISODE_ID)%>%
#   summarise(RRMax = max(RespiratoryRate, na.rm = T), 
#             RRMin = min(RespiratoryRate, na.rm = T), 
#             RRMean = mean(RespiratoryRate , na.rm = T), 
#             RRSD = sd(RespiratoryRate , na.rm = T))
# 
# HeartRate=stdVital%>%
#   inner_join(final_data_v7,by=c("DeID_PatientID"))%>%
#   filter(DeID_ObservationDate<= Diag_Date & DeID_ObservationDate > Diag_Date-5)%>%
#   group_by(DeID_PREGNANCY_EPISODE_ID)%>%
#   summarise(HRMax = max(HeartRate, na.rm = T), 
#             HRMin = min(HeartRate, na.rm = T), 
#             HRMean = mean(HeartRate , na.rm = T), 
#             HRSD = sd(HeartRate , na.rm = T))

final_data_v7 = final_data_v7%>%left_join(BPDiaNonInvasive)
final_data_v7 = final_data_v7%>%left_join(BPSysNonInvasive)
#final_data_v7 = final_data_v7%>%left_join(HeartRate)


final_data_v8 = final_data_v7
save(final_data_v8, file = "final_data_v8.RData")


library(missForest)
library(dplyr)
final_data_v9 = final_data_v9%>%ungroup()
final_data_v9 = final_data_v9%>%
  select(-DeID_PatientID, -DeID_EncounterID,  -"Diag_EncounterID", -"TermCodeMapped",                             
         -"TermNameMapped",-"DeID_DischargeDate.x",-"DeID_PREGNANCY_EPISODE_ID",-"DeID_Episode_Start_Date",
         - "DeID_Episode_End_Date", -"DeID_Ob_Wrk_Edd_Dt",-"DeID_Last_Menstrual_Period", -"DeID_EncounterID.y",                         
         -"DeID_PatientID_Baby",                        
         -"DeID_EncounterID_Baby",                      
         -"DeID_DELIVERY_EPISODE_ID",-"DeID_Delivery_Time",-"algorithmName",-"TotalScore",-"GenderCode",                                 
         -"DeID_DOB",-"ReligionName",-"diff", -"Social_Date",-"Diag_Date",-"BABY_SEX")

final_data_v9$diag_GA = as.numeric(final_data_v9$diag_GA, units = "days")
final_data_v9$age = as.numeric(final_data_v9$age, units = "days")
final_data_v9$time_to_deliver = as.numeric(final_data_v9$time_to_deliver, units = "days")

final_data_v9_char = final_data_v9[sapply(final_data_v9, is.character)] 
final_data_v9_num = final_data_v9[sapply(final_data_v9, is.numeric)] 

final_data_v9_num =do.call(data.frame,lapply(final_data_v9_num, function(x) replace(x, is.infinite(x),NA)))
final_data_v9_rf = missForest(as.matrix(final_data_v9_num))
final_data_v9_noNA = data.frame(final_data_v9_char,final_data_v9_rf$ximp)

# change marital status into married, unmarried
for(i in c(1:nrow(final_data_v9_noNA))){
  if(final_data_v9_noNA$MaritalStatusNameSource[i]%in%c("Single", "Divorced", "Widowed")){
    final_data_v9_noNA$MaritalStatusNameSource[i] = "Unmarried"
  }
}

# one hot encoding
library(caret)
dummy <- dummyVars(" ~ .", data=final_data_v9_noNA)
final_data_v9_noNA <- data.frame(predict(dummy, newdata = final_data_v9_noNA)) 
final_data_v9_noNA= final_data_v9_noNA%>%filter(RaceName == 0 &RaceNamePatient.Refused == 0)
final_data_v9_noNA= final_data_v9_noNA%>%
  select(- SmokingStatusSourceUnknown, -IllegalDrugUserStatusSourceUnknown,-SexuallyActiveStatusSourceUnknown,
         -RaceName, -RaceNameUnknown, -RaceNamePatient.Refused)
final_data_v9_noNA= final_data_v9_noNA%>%
  select(-"AlcoholUseStatusSourceUnknown", -"MaritalStatusNameSourceUnknown",
         -"RaceNameOther", -"EthnicityCodeSourceUNKNOWN")
for(i in c(1:nrow(final_data_v9_noNA))){
  if(final_data_v9_noNA$time_to_deliver[i] == 0){
    final_data_v9_noNA$time_to_deliver[i] = 0.5
  }
}

#scale numeric variables
num_var = c("diag_GA","EPIS_GRAVIDA_COUNT", "EPIS_PARA_COUNT", "NUMBER_OF_FETUSES","age","diag_GA", "creatinine",
            "Hematocrit","Hemoglobin","Mean_Corpuscular_Hgb", "Mean_Corpuscular_Hgb_Conc", "Red_Blood_Cell_Count",                              
            "Red_Cell_Distribution_Width", "White_Blood_Cell_Count",  "Mean_Platelet_Volume", "Platelet_Count",                                    
            "AST", "ALT", "Mean_Corpuscular_Vol","BPDiaMax","BPDiaMin", "BPDiaMean", "BPDiaSD", "BPSysMax", "BPSysMin","BPSysMean", "BPSysSD")
for(i in c(1:ncol(final_data_v9_noNA))){
  if(colnames(final_data_v9_noNA)[i]%in%num_var){
    final_data_v9_noNA[,i] = scale(final_data_v9_noNA[,i], center = F)
  }
}

final_data_v9_noNA = final_data_v9_noNA%>%filter(!is.na(EPIS_GRAVIDA_COUNT)&!is.na(EPIS_PARA_COUNT)&!is.na(NUMBER_OF_FETUSES))

save(final_data_pro, file = "final_data_pro.RData")

