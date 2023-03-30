#06 add social history
library(dplyr)
social = read.csv("ClaritySocialHistory.csv")
encounter = read.csv("EncounterAll.csv")
load("final_data_v4.RData")

#add time to social status
social = inner_join(social, encounter)
social$Social_Date = as.Date(social$DeID_AdmitDate, format = "%m/%d/%Y %H:%M")
social = social%>%
  select(DeID_PatientID, DeID_EncounterID, SmokingStatusSource,AlcoholUseStatusSource,
          IllegalDrugUserStatusSource, SexuallyActiveStatusSource, Social_Date)

final_data_v5= social%>%
  right_join(final_data_v4, by="DeID_PatientID")%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  mutate(diff = Diag_Date - Social_Date)%>%
  arrange(DeID_PREGNANCY_EPISODE_ID,abs(diff))%>%
  slice(1)

#final_data_v5 = left_join(final_data_v4,social, by =  c("DeID_EncounterID.y"="DeID_EncounterID"))

table(final_data_v5$SmokingStatusSource)
for(i in(1:nrow(final_data_v5))){
  if(is.na(final_data_v5$SmokingStatusSource[i])){
    final_data_v5$SmokingStatusSource[i] = "Unknown"
  }else if(final_data_v5$SmokingStatusSource[i] == "Current Every Day Smoker"| 
     final_data_v5$SmokingStatusSource[i] == "Current Some Day Smoker"){
    final_data_v5$SmokingStatusSource[i] = "Current Smoker"
  }else if(final_data_v5$SmokingStatusSource[i] == "Heavy Tobacco Smoker"|
           final_data_v5$SmokingStatusSource[i] == "Light Tobacco Smoker"){
    final_data_v5$SmokingStatusSource[i] = "Tobacco Smoker"
  }else if(final_data_v5$SmokingStatusSource[i] == "Never Assessed"|
           final_data_v5$SmokingStatusSource[i] == "Unknown If Ever Smoked"){
    final_data_v5$SmokingStatusSource[i] = "Unknown"
  }
}

table(final_data_v5$AlcoholUseStatusSource)
for(i in(1:nrow(final_data_v5))){
  if(is.na(final_data_v5$AlcoholUseStatusSource[i]) |
     final_data_v5$AlcoholUseStatusSource[i] == ""|
     final_data_v5$AlcoholUseStatusSource[i] == "Not Asked"){
    final_data_v5$AlcoholUseStatusSource[i] = "Unknown"
  }
}

table(final_data_v5$IllegalDrugUserStatusSource)
for(i in(1:nrow(final_data_v5))){
  if(is.na(final_data_v5$IllegalDrugUserStatusSource[i]) |
     final_data_v5$IllegalDrugUserStatusSource[i] == ""|
     final_data_v5$IllegalDrugUserStatusSource[i] == "Not Asked"){
    final_data_v5$IllegalDrugUserStatusSource[i] = "Unknown"
  }
}

table(final_data_v5$SexuallyActiveStatusSource)
for(i in(1:nrow(final_data_v5))){
  if(is.na(final_data_v5$SexuallyActiveStatusSource[i]) |
     final_data_v5$SexuallyActiveStatusSource[i] == ""|
     final_data_v5$SexuallyActiveStatusSource[i] == "Not Asked"){
    final_data_v5$SexuallyActiveStatusSource[i] = "Unknown"
  }
}

final_data_v5$diag_GA = final_data_v5$GESTATIONAL_AGE-final_data_v5$time_to_deliver
pdf("diag_GA.pdf")
ggplot(final_data_v5, aes(x = diag_GA, y = time_to_deliver))+
  geom_point()+
  geom_smooth(method = "lm")
dev.off()

pdf("diagGA_deliverGA.pdf")
ggplot(final_data_v5, aes(x = diag_GA, y = GESTATIONAL_AGE))+
  geom_point()+
  geom_smooth(method = "lm")
dev.off()

save(final_data_v5, file = "final_data_v5.RData")