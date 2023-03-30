
# 03 Add PE types

########################################################
# Merge Pregnancy Data
# Author: Xiaotong Yang
# Date: 01/24/22
# Input files: "pregancy_complete.RData", "DiagnosesComprehensive.csv"
# Output files :
#   "diag2.Rdata": diagnoses with admission time after 01012014
########################################################

load("earliest_diag.RData")
# eope vs lope
final_data_v3 = final_data_v2%>%mutate(eope = ifelse((Diag_Date-DeID_Last_Menstrual_Period<238), 1,0))
# severe vs mild
SeverePE_diag = 
  earliest_diag%>%
  mutate(TermCode = substr(TermCodeMapped, 1, 5))%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  mutate(gd = +(TermCode == "O14.1"|TermCode == "642.5"))%>%
  summarise(SeverePE = ifelse(sum(gd) == 0, 0, 1))

final_data_v3 = inner_join(final_data_v3, SeverePE_diag, by = ("DeID_PREGNANCY_EPISODE_ID"))

save(final_data_v3, file = "final_data_v3.RData")