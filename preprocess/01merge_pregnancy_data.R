########################################################
# Merge Pregnancy Data
# Author: Xiaotong Yang
# Date: 01/24/22
# Input files: "Stork_Pregnancy_Outcomes.csv", "Stork_Pregnancy.csv"
########################################################


#Find earliest diagnoses time
library(dplyr)

pregnancy_outcome = read.csv("Stork_Pregnancy_Outcomes.csv")
pregnancy = read.csv("Stork_Pregnancies.csv")

# Remove baby sex from pregancy_outcome because one pregnancy can have multliple babies
# amd sex is not missing at random
pregnancy_outcome = pregnancy_outcome%>%select(-BABY_SEX)
# One pregnancy episode can include multiple deliveries (twins)
# We only consider them as one observation since their GAs are the same
pregnancy_outcome = pregnancy_outcome%>%group_by(DeID_PREGNANCY_EPISODE_ID)%>%arrange(GESTATIONAL_AGE)%>%slice(1)
pregnancy_complete = inner_join(pregnancy, pregnancy_outcome, by = c("DeID_PatientID","DeID_PREGNANCY_EPISODE_ID"))

#convert date
pregnancy_complete$DeID_Delivery_Time = as.Date(pregnancy_complete$DeID_Delivery_Time, format = "%m/%d/%Y %H:%M")
pregnancy_complete$DeID_Last_Menstrual_Period = as.Date(pregnancy_complete$DeID_Last_Menstrual_Period, format = "%m/%d/%Y %H:%M")

#Fill GA 
for (i in 1:nrow(pregnancy_complete)){
  if(is.na(pregnancy_complete$GESTATIONAL_AGE[i])){
    pregnancy_complete$GESTATIONAL_AGE[i] = pregnancy_complete$DeID_Delivery_Time[i] - pregnancy_complete$DeID_Last_Menstrual_Period[i]
  }
}

#Fill last menstral period
for (i in 1:nrow(pregnancy_complete)){
  if(is.na(pregnancy_complete$DeID_Last_Menstrual_Period[i])){
    pregnancy_complete$DeID_Last_Menstrual_Period[i] = pregnancy_complete$DeID_Delivery_Time[i] - pregnancy_complete$GESTATIONAL_AGE[i]
  }
}

#remove samples with GA that can't be filled
pregnancy_complete = pregnancy_complete%>%filter(!is.na(GESTATIONAL_AGE))
pregnancy_complete = pregnancy_complete%>%filter(GESTATIONAL_AGE>0)
#pregnancy_complete = pregnancy_complete%>%filter(!is.na(DeID_Last_Menstrual_Period))


# # people with multiple gestation
# multi_gest = pregnancy_complete%>%group_by(DeID_PatientID)%>%summarise(n=n())%>%filter(n>1)
# multi_gest = pregnancy_complete%>%filter(DeID_PatientID%in%multi_gest$DeID_PatientID)

# rank pregnancy episodes from the same mother by time
pregnancy_complete2 = pregnancy_complete%>%
  arrange(DeID_PatientID,DeID_Delivery_Time)%>%
  group_by(DeID_PatientID)%>%
  mutate(rank = rank(DeID_Delivery_Time))

save(pregnancy_complete, file = "pregnancy_complete.Rdata")
save(multi_gest, file = "multi_gest.RData")

# Diag = read.csv("DiagnosesEverything.csv")
# Diag_deliverytime = inner_join(Diag, delivery_time, by=c("DeID_PatientID"))
# Diag_deliverytime$DeID_Delivery_Time = as.Date(Diag_deliverytime$DeID_Delivery_Time, format = "%m/%d/%Y %H:%M")
# Diag_deliverytime$DeID_ActivityDate = as.Date(Diag_deliverytime$DeID_ActivityDate, format = "%m/%d/%Y %H:%M")
# Diag_deliverytime$timeToDeliver = Diag_deliverytime$DeID_Delivery_Time- Diag_deliverytime$DeID_ActivityDate
# init_diag = Diag_deliverytime%>%group_by(DeID_PatientID)%>%slice_max(timeToDeliver,  with_ties = F)