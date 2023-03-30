# 010 add past PE info
library(dplyr)

setwd("/home/xtyang/extra_comorbid")
load("/home/xtyang/PE01122022/final_data_v7")
load("/home/xtyang/PE01122022/final_data_v8")
encounter = read.csv("EncounterAll.csv")
past_pe = read.csv("past_pe_diag.csv")

past_pe = inner_join(past_pe, encounter)

# select the earliest diagnosis date of the first recorded gestation
first_diagnosis = final_data_v9%>%
  ungroup()%>%
  select(DeID_PatientID, Diag_Date)%>%
  group_by(DeID_PatientID)%>%slice_min(Diag_Date)

past_pe$DeID_AdmitDate = as.Date(past_pe$DeID_AdmitDate, format = "%m/%d/%Y %H:%M")
past_pe = past_pe%>%left_join(first_diagnosis)%>%filter(DeID_AdmitDate<Diag_Date - 120)
past_pe_patient = past_pe%>%group_by(DeID_PatientID)%>%summarise(past_pe=1)
final_data_v8 = final_data_v8%>%left_join(past_pe_patient)
final_data_v8$past_pe[is.na(final_data_v8$past_pe)]=0

# the second gestation in data automatically has past pe
lategest = 
  final_data_v8%>%group_by(DeID_PatientID)%>%
  arrange(Diag_Date, .by_group = T)%>%
  mutate(num_gest = row_number())%>%
  filter(num_gest>1)%>%
  ungroup()%>%
  select(DeID_PREGNANCY_EPISODE_ID)

final_data_v8$past_pe[final_data_v8$DeID_PREGNANCY_EPISODE_ID%in%lategest$DeID_PREGNANCY_EPISODE_ID] = 1
final_data_v9 = final_data_v8

second_gest = 
  final_data_v9%>%
  group_by(DeID_PatientID)%>%
  arrange(Diag_Date, .by_group = T)%>%
  mutate(gestation = row_number())%>%
  filter(gestation == 2)%>%
  select(DeID_PREGNANCY_EPISODE_ID, DeID_PatientID, Diag_Date)
third_gest = 
  final_data_v9%>%
  group_by(DeID_PatientID)%>%
  arrange(Diag_Date, .by_group = T)%>%
  mutate(gestation = row_number())%>%
  filter(gestation == 3)%>%
  select(DeID_PREGNANCY_EPISODE_ID, DeID_PatientID, Diag_Date)
fourth_gest =  final_data_v9%>%
  group_by(DeID_PatientID)%>%
  arrange(Diag_Date, .by_group = T)%>%
  mutate(gestation = row_number())%>%
  filter(gestation == 4)%>%
  select(DeID_PREGNANCY_EPISODE_ID, DeID_PatientID, Diag_Date)

# add past cesarean info
# add csection history before the first recorded pe gestation
past_csection = read.csv("past_csection_diag.csv")
past_csection = inner_join(past_csection, encounter)
past_csection$DeID_AdmitDate = as.Date(past_csection$DeID_AdmitDate, format = "%m/%d/%Y %H:%M")
past_csection1 = past_csection%>%left_join(first_diagnosis)%>%filter(DeID_AdmitDate<Diag_Date - 120)
past_csection1 = past_csection1%>%group_by(DeID_PatientID)%>%summarise(past_csection = 1)
final_data_v9 = final_data_v9%>%left_join(past_csection1)
final_data_v9$past_csection[is.na(final_data_v9$past_csection)]=0
# add csection history before the secound recorded pe gestation
past_csection2 = past_csection%>%
  inner_join(second_gest)%>%
  filter(DeID_AdmitDate<Diag_Date - 120)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(past_csection = 1)
final_data_v9$past_csection[final_data_v9$DeID_PREGNANCY_EPISODE_ID%in%past_csection2$DeID_PREGNANCY_EPISODE_ID] = 1

past_csection3 = past_csection%>%
  inner_join(third_gest)%>%
  filter(DeID_AdmitDate<Diag_Date - 60)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(past_csection = 1)

past_csection4 = past_csection%>%
  inner_join(fourth_gest)%>%
  filter(DeID_AdmitDate<Diag_Date - 60)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(past_csection = 1)

# add renal_disease
renal_disease = read.csv("renal_disease_diag.csv")
renal_disease = inner_join(renal_disease, encounter)
renal_disease$DeID_AdmitDate = as.Date(renal_disease$DeID_AdmitDate, format = "%m/%d/%Y %H:%M")
renal_disease1 = renal_disease%>%left_join(first_diagnosis)%>%filter(DeID_AdmitDate<Diag_Date)
renal_disease1 = renal_disease1%>%group_by(DeID_PatientID)%>%summarise(renal_disease = 1)
final_data_v9 = final_data_v9%>%left_join(renal_disease1)
final_data_v9$renal_disease[is.na(final_data_v9$renal_disease)]=0

renal_disease2 = renal_disease%>%
  inner_join(second_gest)%>%
  filter(DeID_AdmitDate<Diag_Date)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(renal_disease = 1)
final_data_v9$renal_disease[final_data_v9$DeID_PREGNANCY_EPISODE_ID%in%renal_disease2$DeID_PREGNANCY_EPISODE_ID] = 1

renal_disease3 = renal_disease%>%
  inner_join(second_gest)%>%
  filter(DeID_AdmitDate<Diag_Date)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(renal_disease = 1)
final_data_v9$renal_disease[final_data_v9$DeID_PREGNANCY_EPISODE_ID%in%renal_disease2$DeID_PREGNANCY_EPISODE_ID] = 1

# Add extra comorbidities
extra_como = read.csv("extra_comorbid.csv")
extra_como = inner_join(extra_como, encounter)
extra_como$DeID_AdmitDate = as.Date(extra_como$DeID_AdmitDate, format = "%m/%d/%Y %H:%M")
gdm = extra_como%>%
  filter(substr(TermCodeMapped, 1, 3) == "O24")
abruption = extra_como%>%
  filter(substr(TermCodeMapped, 1, 3) == "O45"|substr(TermCodeMapped, 1, 3) =="641")
IUGR = extra_como%>%
  filter(substr(TermCodeMapped, 1, 3) == "764"|substr(TermCodeMapped, 1, 3) =="Z36")
preterm_labor =  extra_como%>%
  filter(substr(TermCodeMapped, 1, 3) == "O60")

gdm1 = gdm%>%left_join(first_diagnosis)%>%filter(DeID_AdmitDate<Diag_Date)
gdm1 = gdm1%>%group_by(DeID_PatientID)%>%summarise(gdm = 1)
final_data_v9 = final_data_v9%>%left_join(gdm1)
final_data_v9$gdm[is.na(final_data_v9$gdm)]=0

gdm2 = gdm%>%
  inner_join(second_gest)%>%
  filter(DeID_AdmitDate<Diag_Date)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(gdm = 1)
final_data_v9$gdm[final_data_v9$DeID_PREGNANCY_EPISODE_ID%in%gdm2$DeID_PREGNANCY_EPISODE_ID] = 1

gdm3 = gdm%>%
  inner_join(third_gest)%>%
  filter(DeID_AdmitDate<Diag_Date)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(gdm = 1)
final_data_v9$gdm[final_data_v9$DeID_PREGNANCY_EPISODE_ID%in%gdm3$DeID_PREGNANCY_EPISODE_ID] = 1

abruption1 = abruption%>%left_join(first_diagnosis)%>%filter(DeID_AdmitDate<Diag_Date)
abruption1 = abruption1%>%group_by(DeID_PatientID)%>%summarise(abruption = 1)
final_data_v9 = final_data_v9%>%left_join(abruption1)
final_data_v9$abruption[is.na(final_data_v9$abruption)]=0
abruption2 = abruption%>%
  inner_join(second_gest)%>%
  filter(DeID_AdmitDate<Diag_Date)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(abruption = 1)
final_data_v9$abruption[final_data_v9$DeID_PREGNANCY_EPISODE_ID%in%abruption2$DeID_PREGNANCY_EPISODE_ID] = 1
abruption3 = abruption%>%
  inner_join(third_gest)%>%
  filter(DeID_AdmitDate<Diag_Date)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(abruption = 1)
final_data_v9$abruption[final_data_v9$DeID_PREGNANCY_EPISODE_ID%in%abruption3$DeID_PREGNANCY_EPISODE_ID] = 1

preterm_labor1 = preterm_labor%>%left_join(first_diagnosis)%>%filter(DeID_AdmitDate<Diag_Date-120)
preterm_labor1 = preterm_labor1%>%group_by(DeID_PatientID)%>%summarise(preterm_labor = 1)
final_data_v9 = final_data_v9%>%left_join(preterm_labor1)
final_data_v9$preterm_labor[is.na(final_data_v9$preterm_labor)]=0
preterm_labor2 = preterm_labor%>%
  inner_join(second_gest)%>%
  filter(DeID_AdmitDate<Diag_Date-120)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(preterm_labor = 1)
final_data_v9$preterm_labor[final_data_v9$DeID_PREGNANCY_EPISODE_ID%in%preterm_labor2$DeID_PREGNANCY_EPISODE_ID] = 1
preterm_labor3 = preterm_labor%>%
  inner_join(third_gest)%>%
  filter(DeID_AdmitDate<Diag_Date-120)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(preterm_labor = 1)
final_data_v9$preterm_labor[final_data_v9$DeID_PREGNANCY_EPISODE_ID%in%preterm_labor3$DeID_PREGNANCY_EPISODE_ID] = 1

IUGR1 = IUGR%>%left_join(first_diagnosis)%>%filter(DeID_AdmitDate<Diag_Date)
IUGR1 = IUGR1%>%group_by(DeID_PatientID)%>%summarise(IUGR = 1)
final_data_v9 = final_data_v9%>%left_join(IUGR1)
final_data_v9$IUGR[is.na(final_data_v9$IUGR)]=0
IUGR2 = IUGR%>%
  inner_join(second_gest)%>%
  filter(DeID_AdmitDate<Diag_Date)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(IUGR = 1)
final_data_v9$IUGR[final_data_v9$DeID_PREGNANCY_EPISODE_ID%in%IUGR2$DeID_PREGNANCY_EPISODE_ID] = 1
IUGR3 = IUGR%>%
  inner_join(third_gest)%>%
  filter(DeID_AdmitDate<Diag_Date)%>%
  group_by(DeID_PREGNANCY_EPISODE_ID)%>%
  summarise(IUGR = 1)
final_data_v9$IUGR[final_data_v9$DeID_PREGNANCY_EPISODE_ID%in%IUGR3$DeID_PREGNANCY_EPISODE_ID] = 1

save(final_data_v9, file = "final_data_v9.RData")