#Gabi Rivera
#24Sep2022
#ADS501-01


# Flu Shot Learning: Predict H1N1 and Seasonal Flu Vaccines

#Missing Data Imputation by MICE

#Import Raw Data
fs = read.csv(
  "Flu_Shot_Learning_Predict_H1N1_and_Seasonal_Flu_Vaccines_-_Training_Features.csv", 
  header = TRUE, sep = ",")
head(fs)


#Libraries
library(mice)
library(tidyverse)
library(skimr)
library(ggplot2)

#General Information
skim(fs)
summary(fs)

#Correct data type to binary: True or False (15)
fs$h1n1_vaccine = as.logical(fs$h1n1_vaccine)
fs$seasonal_vaccine = as.logical(fs$seasonal_vaccine)
fs$behavioral_antiviral_meds = as.logical(fs$behavioral_antiviral_meds)
fs$behavioral_avoidance = as.logical(fs$behavioral_avoidance)
fs$behavioral_face_mask = as.logical(fs$behavioral_face_mask)
fs$behavioral_wash_hands = as.logical(fs$behavioral_wash_hands)
fs$behavioral_large_gatherings = as.logical(fs$behavioral_large_gatherings)
fs$behavioral_outside_home = as.logical(fs$behavioral_outside_home)
fs$behavioral_touch_face = as.logical(fs$behavioral_touch_face)
fs$doctor_recc_h1n1 = as.logical(fs$doctor_recc_h1n1)
fs$doctor_recc_seasonal = as.logical(fs$doctor_recc_seasonal)
fs$chronic_med_condition = as.logical(fs$chronic_med_condition)
fs$child_under_6_months = as.logical(fs$child_under_6_months)
fs$health_worker = as.logical(fs$health_worker)
fs$health_insurance = as.logical(fs$health_insurance)
summary(fs)

#Correct data type to Ordinal (10)
fs$h1n1_concern = as.factor(fs$h1n1_concern)
fs$h1n1_knowledge = as.factor(fs$h1n1_knowledge)
fs$opinion_h1n1_vacc_effective = as.factor(fs$opinion_h1n1_vacc_effective)
fs$opinion_h1n1_risk = as.factor(fs$opinion_h1n1_risk)
fs$opinion_h1n1_sick_from_vacc = as.factor(fs$opinion_h1n1_sick_from_vacc)
fs$opinion_seas_vacc_effective = as.factor(fs$opinion_seas_vacc_effective)
fs$opinion_seas_risk = as.factor(fs$opinion_seas_risk)
fs$opinion_seas_sick_from_vacc = as.factor(fs$opinion_seas_sick_from_vacc)
fs$household_adults = as.factor(fs$household_adults)
fs$household_children = as.factor(fs$household_children)
summary(fs)
skim(fs)

#Correct data type to Factor (12)
fs$age_group = as.factor(fs$age_group)
fs$education[fs$education==""] = NA
fs$education = as.factor(fs$education)
fs$race[fs$race==""] = NA
fs$race = as.factor(fs$race)
fs$sex = as.factor(fs$sex)
fs$income_poverty[fs$income_poverty==""] = NA
fs$income_poverty = as.factor(fs$income_poverty)
fs$marital_status[fs$marital_status==""] = NA
fs$marital_status = as.factor(fs$marital_status)
fs$rent_or_own[fs$rent_or_own==""] = NA
fs$rent_or_own = as.factor(fs$rent_or_own)
fs$employment_status[fs$employment_status==""] = NA
fs$employment_status = as.factor(fs$employment_status)
fs$hhs_geo_region[fs$hhs_geo_region==""] = NA
fs$hhs_geo_region = as.factor(fs$hhs_geo_region)
fs$census_msa = as.factor(fs$census_msa)
fs$employment_industry[fs$employment_industry==""] = NA
fs$employment_industry = as.factor(fs$employment_industry)
fs$employment_occupation[fs$employment_occupation==""] = NA
fs$employment_occupation = as.factor(fs$employment_occupation)
summary(fs)
skim(fs)

#MICE imputation
methods(mice)
m_fs = mice(fs, method = 'rf')
plot(m_fs)

#h1n1_concern check
m_fs$imp$h1n1_concern

#income_poverty check
m_fs$imp$income_poverty

#finished data imputation with MICE
c_fs = complete(m_fs, 5)
summary(c_fs)
skim(c_fs)

write.csv(c_fs,"/Users/gabirivera/Desktop/MSADS2/ADS-501-01/Project/Data/
          H1N1_and_Seasonal_Flu_Vaccines_-_Training_Features.csv", 
          row.names = FALSE)







