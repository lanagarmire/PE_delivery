# 03 Add comorbidity

########################################################
# Merge Pregnancy Data
# Author: Xiaotong Yang
# Date: 01/24/22
# Input files: "pregancy_complete.RData", "DiagnosesComprehensive.csv"
# Output files :
#   "diag2.Rdata": diagnoses with admission time after 01012014
########################################################
como = read.csv("ComorbiditiesElixhauserComprehensive.csv")
encounter = read.csv("EncounterAll.csv")
load("final_data_v1.RData")

como = left_join(como, encounter, by = c("DeID_PatientID","DeID_EncounterID"))
como$DeID_AdmitDate =as.Date(como$DeID_AdmitDate, format = "%m/%d/%Y %H:%M")
sum(is.na(como$DeID_AdmitDate))
#no missing

final_data_v2 = left_join(final_data_v1, como, by = c("DeID_PatientID","DeID_EncounterID.x" = "DeID_EncounterID"))
final_data_v2 = final_data_v2%>%select(-DeID_AdmitDate.y, -DeID_DischargeDate.y, -DeID_PatientID.y)
colnames(final_data_v2)[1] = c("DeID_PatientID")
colnames(final_data_v2)[2] = c("Diag_EncounterID")
colnames(final_data_v2)[5] = c("Diag_Date")

final_data_v2$time_to_deliver = final_data_v2$DeID_Delivery_Time - final_data_v2$Diag_Date

save(final_data_v2, file = "final_data_v2.RData")
