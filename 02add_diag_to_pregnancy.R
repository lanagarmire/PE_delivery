# 02 Add diagnoses info

########################################################
# Merge Pregnancy Data
# Author: Xiaotong Yang
# Date: 01/24/22
# Input files: "pregancy_complete.RData", "DiagnosesComprehensive.csv"
# Output files :
#   "diag2.Rdata": diagnoses with admission time after 01012014
########################################################
library(dplyr)
diag = read.csv("DiagnosesComprehensiveAll.csv")
encounter = read.csv("EncounterAll.csv")
load("pregnancy_complete.Rdata")

# Add date to diagnoses
diag2 = left_join(diag, encounter, by = c("DeID_PatientID","DeID_EncounterID"))
diag2$DeID_AdmitDate =as.Date(diag2$DeID_AdmitDate, format = "%m/%d/%Y %H:%M")
#remove diagnoses with missing time
diag2 = diag2%>%filter(!is.na(DeID_AdmitDate))
#diag2= diag2%>%filter(DeID_AdmitDate>"2010-01-01")
save(diag2, file = "diag2.RData")

# match diagnoses and pregnancy episode
pregnancy_diag = full_join(diag2, pregnancy_complete, by=c("DeID_PatientID"))
pregnancy_diag = pregnancy_diag%>%filter(DeID_AdmitDate <= DeID_Delivery_Time & DeID_AdmitDate >= DeID_Last_Menstrual_Period)

save(pregnancy_diag, file = "pregnancy_diag.RData")

earliest_diag = pregnancy_diag%>%arrange(DeID_PREGNANCY_EPISODE_ID, DeID_AdmitDate)%>%group_by(DeID_PREGNANCY_EPISODE_ID)%>%slice_min(DeID_AdmitDate)

# there are less samples in pregnancy_diag than pregnancy_complete, because many samples in pregnancy_complete have PE history but not PE in current pregnancy
# or is current PE but not delivered so is not shown in pe complete
save(earliest_diag, file = "earliest_diag.RData")

final_data_v1 = earliest_diag%>%group_by(DeID_PREGNANCY_EPISODE_ID)%>%slice(1)
save(final_data_v1)