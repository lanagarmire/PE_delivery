# 05 Add Demographics

########################################################
# add demographical data
# Author: Xiaotong Yang
# Date: 01/24/22
# Input files: 
# Output files :
#   "diag2.Rdata": diagnoses with admission time after 01012014
########################################################

load("final_data_v3.RData")
demo = read.csv("DemographicInfo.csv")
final_data_v4 = left_join(final_data_v3, demo)
final_data_v4$DeID_DOB = as.Date(final_data_v4$DeID_DOB, format = "%m/%d/%Y %H:%M")
final_data_v4 = final_data_v4%>%mutate(age = floor((Diag_Date - DeID_DOB)/365))

for(i in 1:nrow(final_data_v4)){
  if(final_data_v4$MaritalStatusNameSource[i] == "M"){
    final_data_v4$MaritalStatusNameSource[i] = "Married"
  }else if(final_data_v4$MaritalStatusNameSource[i] == "S"){
    final_data_v4$MaritalStatusNameSource[i] = "Single"
  }else if(final_data_v4$MaritalStatusNameSource[i] == ""|final_data_v4$MaritalStatusNameSource[i] =="U"|
           final_data_v4$MaritalStatusNameSource[i] =="Other"|final_data_v4$MaritalStatusNameSource[i] == "Z"){
    final_data_v4$MaritalStatusNameSource[i] = "Unknown"
  }
}

table(final_data_v4$MaritalStatusNameSource)

save(final_data_v4, file = "final_data_v4.RData")