
# 04 Add lab results

########################################################
# Merge Pregnancy Data
# Author: Xiaotong Yang
# Date: 01/24/22
# Input files: 
# Abnormal Flags indicates the normal status of the result. (L: Below low normal, 
# H: Above high normal, LL: Below lower panic limits, HH: Above upper panic limits, 
# N, Normal, A: Abnormal, AA: Very abnormal, S: Susceptible, R: Resistant, I: Intermediate)
# Output files :
#   "diag2.Rdata": diagnoses with admission time after 01012014
########################################################
lab = read.csv("LabResults.csv")
load("final_data_v5.RData")

lab$DeID_COLLECTION_DATE=as.Date(lab$DeID_COLLECTION_DATE, format = "%m/%d/%Y %H:%M")

# top 10 lab test name
lab%>%group_by(ORDER_NAME)%>%summarise(n=n())%>%arrange(desc(n))%>%top_n(10)

# lab result details

# creatinine
# select lab test done around the first diagnosis
creatinine = lab%>%
  filter(ORDER_NAME == "CREATININE LEVEL")%>%
  mutate(creatinine_value = as.numeric(VALUE))%>%
  select(DeID_PatientID, DeID_EncounterID, DeID_COLLECTION_DATE,creatinine_value)%>%
  inner_join(final_data_v5,by=c("DeID_PatientID"))%>%
  filter(DeID_COLLECTION_DATE <=  Diag_Date+10&DeID_COLLECTION_DATE>Diag_Date-11)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(creatinine_value = mean(creatinine_value))%>%
  select(DeID_PREGNANCY_EPISODE_ID, creatinine_value)

final_data_v7 = final_data_v5%>%left_join(creatinine, by=c("DeID_PREGNANCY_EPISODE_ID"))

#blood
Hematocrit = lab%>%
  filter(RESULT_NAME == "Hematocrit")%>%
  mutate(value = as.numeric(VALUE))%>%
  select(DeID_PatientID, DeID_EncounterID, DeID_COLLECTION_DATE,value)%>%
  inner_join(final_data_v5,by=c("DeID_PatientID"))%>%
  filter(DeID_COLLECTION_DATE <=  Diag_Date+10&DeID_COLLECTION_DATE>Diag_Date-11)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(Hematocrit = mean(value))%>%
  select(DeID_PREGNANCY_EPISODE_ID, Hematocrit)

final_data_v7 = final_data_v7%>%left_join(Hematocrit,  by=c("DeID_PREGNANCY_EPISODE_ID"))

Hemoglobin = lab%>%
  filter(RESULT_NAME == "Hemoglobin")%>%
  mutate(value = as.numeric(VALUE))%>%
  select(DeID_PatientID, DeID_EncounterID, DeID_COLLECTION_DATE,value)%>%
  inner_join(final_data_v5,by=c("DeID_PatientID"))%>%
  filter(DeID_COLLECTION_DATE <=  Diag_Date+10&DeID_COLLECTION_DATE>Diag_Date-11)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(Hemoglobin = mean(value))%>%
  select(DeID_PREGNANCY_EPISODE_ID, Hemoglobin)

final_data_v7 = final_data_v7%>%left_join(Hemoglobin,  by=c("DeID_PREGNANCY_EPISODE_ID"))


Mean_Corpuscular_Hgb = lab%>%
  filter(RESULT_NAME == "Mean Corpuscular Hgb")%>%
  mutate(value = as.numeric(VALUE))%>%
  select(DeID_PatientID, DeID_EncounterID, DeID_COLLECTION_DATE,value)%>%
  inner_join(final_data_v5,by=c("DeID_PatientID"))%>%
  filter(DeID_COLLECTION_DATE <=  Diag_Date+10&DeID_COLLECTION_DATE>Diag_Date-11)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(Mean_Corpuscular_Hgb = mean(value))%>%
  select(DeID_PREGNANCY_EPISODE_ID, Mean_Corpuscular_Hgb)

final_data_v7 = final_data_v7%>%left_join(Mean_Corpuscular_Hgb,  by=c("DeID_PREGNANCY_EPISODE_ID"))



Mean_Corpuscular_Hgb_Conc = lab%>%
  filter(RESULT_NAME == "Mean Corpuscular Hgb Conc.")%>%
  mutate(value = as.numeric(VALUE))%>%
  select(DeID_PatientID, DeID_EncounterID, DeID_COLLECTION_DATE,value)%>%
  inner_join(final_data_v5,by=c("DeID_PatientID"))%>%
  filter(DeID_COLLECTION_DATE <=  Diag_Date+10&DeID_COLLECTION_DATE>Diag_Date-11)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(Mean_Corpuscular_Hgb_Conc = mean(value))%>%
  select(DeID_PREGNANCY_EPISODE_ID, Mean_Corpuscular_Hgb_Conc)

final_data_v7 = final_data_v7%>%left_join(Mean_Corpuscular_Hgb_Conc,  by=c("DeID_PREGNANCY_EPISODE_ID"))

Mean_Corpuscular_Vol = lab%>%
  filter(RESULT_NAME == "Mean Corpuscular Volume")%>%
  mutate(value = as.numeric(VALUE))%>%
  select(DeID_PatientID, DeID_EncounterID, DeID_COLLECTION_DATE,value)%>%
  inner_join(final_data_v5,by=c("DeID_PatientID"))%>%
  filter(DeID_COLLECTION_DATE <=  Diag_Date+10&DeID_COLLECTION_DATE>Diag_Date-11)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(Mean_Corpuscular_Vol = mean(value))%>%
  select(DeID_PREGNANCY_EPISODE_ID, Mean_Corpuscular_Vol)

final_data_v7 = final_data_v7%>%left_join(Mean_Corpuscular_Vol,  by=c("DeID_PREGNANCY_EPISODE_ID"))

Red_Blood_Cell_Count = lab%>%
  filter(RESULT_NAME == "Red Blood Cell Count")%>%
  mutate(value = as.numeric(VALUE))%>%
  select(DeID_PatientID, DeID_EncounterID, DeID_COLLECTION_DATE,value)%>%
  inner_join(final_data_v5,by=c("DeID_PatientID"))%>%
  filter(DeID_COLLECTION_DATE <=  Diag_Date+10&DeID_COLLECTION_DATE>Diag_Date-11)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(Red_Blood_Cell_Count = mean(value))%>%
  select(DeID_PREGNANCY_EPISODE_ID, Red_Blood_Cell_Count)

final_data_v7 = final_data_v7%>%left_join(Red_Blood_Cell_Count,  by=c("DeID_PREGNANCY_EPISODE_ID"))

Red_Cell_Distribution_Width= lab%>%
  filter(RESULT_NAME == "Red Cell Distribution Width")%>%
  mutate(value = as.numeric(VALUE))%>%
  select(DeID_PatientID, DeID_EncounterID, DeID_COLLECTION_DATE,value)%>%
  inner_join(final_data_v5,by=c("DeID_PatientID"))%>%
  filter(DeID_COLLECTION_DATE <=  Diag_Date+10&DeID_COLLECTION_DATE>Diag_Date-11)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(Red_Cell_Distribution_Width = mean(value))%>%
  select(DeID_PREGNANCY_EPISODE_ID, Red_Cell_Distribution_Width)
final_data_v7 = final_data_v7%>%left_join(Red_Cell_Distribution_Width,  by=c("DeID_PREGNANCY_EPISODE_ID"))

White_Blood_Cell_Count = lab%>%
  filter(RESULT_NAME == "White Blood Cell Count")%>%
  mutate(value = as.numeric(VALUE))%>%
  select(DeID_PatientID, DeID_EncounterID, DeID_COLLECTION_DATE,value)%>%
  inner_join(final_data_v5,by=c("DeID_PatientID"))%>%
  filter(DeID_COLLECTION_DATE <=  Diag_Date+10&DeID_COLLECTION_DATE>Diag_Date-11)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(White_Blood_Cell_Count = mean(value))%>%
  select(DeID_PREGNANCY_EPISODE_ID, White_Blood_Cell_Count)
final_data_v7 = final_data_v7%>%left_join(White_Blood_Cell_Count,  by=c("DeID_PREGNANCY_EPISODE_ID"))

Mean_Platelet_Volume = lab%>%
  filter(RESULT_NAME == "Mean Platelet Volume")%>%
  mutate(value = as.numeric(VALUE))%>%
  select(DeID_PatientID, DeID_EncounterID, DeID_COLLECTION_DATE,value)%>%
  inner_join(final_data_v5,by=c("DeID_PatientID"))%>%
  filter(DeID_COLLECTION_DATE <=  Diag_Date+10&DeID_COLLECTION_DATE>Diag_Date-11)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(Mean_Platelet_Volume = mean(value))%>%
  select(DeID_PREGNANCY_EPISODE_ID, Mean_Platelet_Volume)
final_data_v7 = final_data_v7%>%left_join(Mean_Platelet_Volume,  by=c("DeID_PREGNANCY_EPISODE_ID"))

Platelet_Count = lab%>%
  filter(RESULT_NAME == "Platelet Count")%>%
  mutate(value = as.numeric(VALUE))%>%
  select(DeID_PatientID, DeID_EncounterID, DeID_COLLECTION_DATE,value)%>%
  inner_join(final_data_v5,by=c("DeID_PatientID"))%>%
  filter(DeID_COLLECTION_DATE <=  Diag_Date+10&DeID_COLLECTION_DATE>Diag_Date-11)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(Platelet_Count = mean(value))%>%
  select(DeID_PREGNANCY_EPISODE_ID,Platelet_Count)
final_data_v7 = final_data_v7%>%left_join(Platelet_Count,  by=c("DeID_PREGNANCY_EPISODE_ID"))

POC_GLUCOSE_RESULT = lab%>%
  filter(RESULT_NAME == "POC GLUCOSE RESULT")%>%
  mutate(value = as.numeric(VALUE))%>%
  select(DeID_PatientID, DeID_EncounterID, DeID_COLLECTION_DATE,value)%>%
  inner_join(final_data_v5,by=c("DeID_PatientID"))%>%
  filter(DeID_COLLECTION_DATE <=  Diag_Date+10&DeID_COLLECTION_DATE>Diag_Date-11)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(POC_GLUCOSE_RESULT = mean(value))%>%
  select(DeID_PREGNANCY_EPISODE_ID,POC_GLUCOSE_RESULT)
final_data_v7 = final_data_v7%>%left_join(POC_GLUCOSE_RESULT,  by=c("DeID_PREGNANCY_EPISODE_ID"))

AST = lab%>%
  filter(RESULT_NAME == "AST")%>%
  mutate(value = as.numeric(VALUE))%>%
  select(DeID_PatientID, DeID_EncounterID, DeID_COLLECTION_DATE,value)%>%
  inner_join(final_data_v5,by=c("DeID_PatientID"))%>%
  filter(DeID_COLLECTION_DATE <=  Diag_Date+10&DeID_COLLECTION_DATE>Diag_Date-11)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(AST = mean(value))%>%
  select(DeID_PREGNANCY_EPISODE_ID,AST)
final_data_v7 = final_data_v7%>%left_join(AST,  by=c("DeID_PREGNANCY_EPISODE_ID"))

ALT = lab%>%
  filter(RESULT_NAME == "ALT")%>%
  mutate(value = as.numeric(VALUE))%>%
  select(DeID_PatientID, DeID_EncounterID, DeID_COLLECTION_DATE,value)%>%
  inner_join(final_data_v5,by=c("DeID_PatientID"))%>%
  filter(DeID_COLLECTION_DATE <=  Diag_Date+10&DeID_COLLECTION_DATE>Diag_Date-11)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(ALT = mean(value))%>%
  select(DeID_PREGNANCY_EPISODE_ID,ALT)
final_data_v7 = final_data_v7%>%left_join(ALT,  by=c("DeID_PREGNANCY_EPISODE_ID"))

# fill NA using mean
colm = colMeans(final_data_v7[73:85],na.rm=T)
for(i in c(73:85)){
  for(j in c(1:nrow(final_data_v7))){
    if(is.na(final_data_v7[j,i])){
      final_data_v7[j,i] = colm[i-72]l
    }
  }
}

# EGFR_Black = lab%>%
#   filter(RESULT_NAME == "Estimated GFR, Black")%>%
#   select(DeID_PatientID, DeID_EncounterID, DeID_COLLECTION_DATE,value)%>%
#   inner_join(final_data_v5,by=c("DeID_PatientID"))%>%
#   filter(DeID_COLLECTION_DATE <=  Diag_Date+10&DeID_COLLECTION_DATE>Diag_Date-11)%>%
#   group_by(DeID_PREGNANCY_EPISODE_ID)%>%
#   summarise(EGFR_Black = mean(value))%>%
#   select(DeID_PREGNANCY_EPISODE_ID,EGFR_Black)
# final_data_v7 = final_data_v7%>%left_join(EGFR_Black,  by=c("DeID_PREGNANCY_EPISODE_ID"))
# 
# EGFR_nonBlack = lab%>%
#   filter(RESULT_NAME == "Estimated GFR, Non-Black")%>%
#   mutate(value = as.numeric(VALUE))%>%
#   select(DeID_PatientID, DeID_EncounterID, DeID_COLLECTION_DATE,value)%>%
#   inner_join(final_data_v5,by=c("DeID_PatientID"))%>%
#   filter(DeID_COLLECTION_DATE <=  Diag_Date+10&DeID_COLLECTION_DATE>Diag_Date-11)%>%
#   group_by(DeID_PREGNANCY_EPISODE_ID)%>%
#   summarise(EGFR_nonBlack = mean(value))%>%
#   select(DeID_PREGNANCY_EPISODE_ID,EGFR_nonBlack )
# final_data_v7 = final_data_v7%>%left_join(EGFR_nonBlack,  by=c("DeID_PREGNANCY_EPISODE_ID"))