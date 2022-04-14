setwd("C:/Users/Han/Desktop/Box Sync/Stat 485/Final Project")
library(tidyverse)
library(readxl)

### HOW DO  YOU WANNA TREAT THE NAs IN APPLICANT_INCOME_000S ('Remove" or "Zero")
rows.without.applicant.income = 'Remove'
# rows.without.applicant.income = 'Zero'

### Do you wanna combine denial reasons
combine.denial = 'Yes'
# combine.denial = 'No'

## output csv?
output.csv = 'Yes'
# output.csv = 'No'


## data loading
# original dataset, download at https://www.kaggle.com/datasets/miker400/washington-state-home-mortgage-hdma2016
data.ori = read.csv("Washington_State_HDMA-2016.csv")
# county number dataset for graphic display, download at https://www2.census.gov/programs-surveys/popest/geographies/2016/all-geocodes-v2016.xlsx
all_geocodes_v2016 <- read_excel("all-geocodes-v2016.xlsx", skip = 4)


## remove the as_of_year, state_name, state_abbr, agency_name, msamd_name for redundancy, rate_spread for too many NAs
data.1 <- data.ori %>% select(!c(as_of_year, state_name, state_abbr, agency_name, msamd_name, rate_spread))
## 
data.1 <- data.1 %>% select(!c(respondent_id, application_date_indicator,sequence_number, purchaser_type_name, 
                               preapproval_name, edit_status_name))
                               

## convert some columns to the correct data type
data.1$census_tract_number = as.character(data.1$census_tract_number)

## In our case, we will remove all loans that were sold to secondary institutions since we want to deal directly to primary lenders.
## We will also take-out loans that were withdrawn by applicants Application withdrawn by applicant, as well as anything related to pre-approval
## Finally, we remove teh incompleted files to only focus on the completed ones. 
data.1 <- data.1 %>% filter(!action_taken_name %in% c('Loan purchased by the institution', 'Application withdrawn by applicant','File closed for incompleteness','Preapproval request approved but not accepted','Preapproval request denied by financial institution'))

## We drop the follow rows due to key information incompleteness, i.e. NAs in tract_to_msamd_income,  number_of_owner_occupied_units, hud_median_family_income
data.1 <- data.1 %>% filter(is.na(tract_to_msamd_income) ==F &  is.na(number_of_owner_occupied_units)==F & is.na(hud_median_family_income)==F)

if (rows.without.applicant.income == 'Remove'){
    data.1 <-  data.1 %>% filter(is.na(applicant_income_000s) ==F)
} else if (rows.without.applicant.income == 'Zero'){
    data.1 <-  data.1 %>% mutate(applicant_income_000s = replace(applicant_income_000s, is.na(applicant_income_000s) ==T, 0))
}
## Check the NA situations 
data.1 %>% summarise(across(everything(), ~ sum(is.na(.x))))

## replace the action_taken_name by loan_approved, 
# 
data.1 = data.1 %>% mutate(loan_approved = 'Yes')
data.1 = data.1 %>% mutate(loan_approved = if_else(action_taken_name == 'Application denied by financial institution', 'No', 'Yes'), .keep = 'unused')

## applicant_race_name https://ncrc.org/ncrcs-hmda-2018-methodology-how-to-calculate-race-and-ethnicity/
# copy applicant_race_name_1 to applicant_race
data.1 = data.1 %>% mutate(applicant_race = applicant_race_name_1)
# carry over the Hispanic or Latino and 'Not applicable'
data.1 = data.1 %>% mutate(applicant_race = if_else(applicant_ethnicity_name == 'Not applicable','Not applicable', applicant_race))
data.1 = data.1 %>% mutate(applicant_race = if_else(applicant_ethnicity_name == 'Hispanic or Latino','Hispanic or Latino', applicant_race))
# add claimed.mixed if any of the appplicant_race_name_2 to appplicant_race_name_5 is non-empty
data.1 = data.1 %>% mutate(applicant_race = if_else(applicant_race_name_2 != '' | applicant_race_name_3 != '' |
                                                        applicant_race_name_4 != '' | applicant_race_name_5 != '','Claimed Mixed', applicant_race))
# remove the original applicant ethinicity and race info
data.1 <- data.1 %>% select(!c(applicant_race_name_5, applicant_race_name_4,applicant_race_name_3, applicant_race_name_2, 
                               applicant_race_name_1, applicant_ethnicity_name))

## co-applicant_race_name https://ncrc.org/ncrcs-hmda-2018-methodology-how-to-calculate-race-and-ethnicity/
# copy applicant_race_name_1 to applicant_race
data.1 = data.1 %>% mutate(co_applicant_race = co_applicant_race_name_1)
# carry over the Hispanic or Latino and 'Not applicable'
data.1 = data.1 %>% mutate(co_applicant_race = if_else(co_applicant_ethnicity_name == 'Not applicable','Not applicable', co_applicant_race))
data.1 = data.1 %>% mutate(co_applicant_race = if_else(co_applicant_ethnicity_name == 'Hispanic or Latino','Hispanic or Latino', co_applicant_race))
# add claimed.mixed if any of the appplicant_race_name_2 to appplicant_race_name_5 is non-empty
data.1 = data.1 %>% mutate(co_applicant_race = if_else(co_applicant_race_name_2 != '' | co_applicant_race_name_3 != '' |
                                                    co_applicant_race_name_4 != '' | co_applicant_race_name_5 != '','Claimed Mixed', co_applicant_race))
# remove the original applicant ethinicity and race info
data.1 <- data.1 %>% select(!c(co_applicant_race_name_5, co_applicant_race_name_4,co_applicant_race_name_3, co_applicant_race_name_2, 
                               co_applicant_race_name_1, co_applicant_ethnicity_name))

## Decide if you wanna combine denials
if (combine.denial == 'Yes'){
    data.1 = data.1 %>% mutate(denials = if_else(denial_reason_name_1 == '', '', denial_reason_name_1))
    data.1 = data.1 %>% mutate(denials = if_else(denial_reason_name_2 == '', denials, paste(denials, denial_reason_name_2, sep=',')))
    data.1 = data.1 %>% mutate(denials = if_else(denial_reason_name_3 == '', denials, paste(denials, denial_reason_name_3, sep=',')))
    data.1 = data.1 %>% select(!c(denial_reason_name_1,denial_reason_name_2,denial_reason_name_3))
} 

## merge with FIPS data to add the county code to facilitate EDA's map plotting
fips = all_geocodes_v2016 %>% filter(`State Code (FIPS)` == 53 & `County Code (FIPS)` != '000') %>% select(`County Code (FIPS)`,`Area Name (including legal/statistical area description)`)
fips = rename(fips,  county_code='County Code (FIPS)', county_name= 'Area Name (including legal/statistical area description)' )
# Left join fips to data.1
data.1 = left_join(data.1,fips, by = 'county_name')
remove(fips,all_geocodes_v2016, data.ori)

## glimpse numeric
dplyr::glimpse(select_if(data.1,is.numeric))
# Rows: 311,774
# Columns: 8
# $ tract_to_msamd_income          <dbl> 121.69, 83.37, 91.13, 146.17, 162.47, 87.16, 168.82, 76.20, 86.34, 92.84, 103.06, 126.81, 110.05~
#     $ population                     <int> 8381, 4915, 5075, 5032, 5183, 9326, 3344, 7157, 4742, 5422, 5130, 3690, 7745, 5752, 4680, 6484, ~
#     $ minority_population            <dbl> 23.79, 23.99, 11.82, 8.59, 10.50, 19.40, 17.19, 31.35, 25.37, 14.29, 12.16, 15.04, 49.72, 13.23,~
#     $ number_of_owner_occupied_units <int> 2175, 1268, 1136, 1525, 1705, 2449, 1049, 1653, 187, 1513, 1933, 1365, 1630, 1353, 1195, 1335, 1~
#     $ number_of_1_to_4_family_units  <int> 2660, 1777, 1838, 1820, 2104, 3403, 1106, 2094, 802, 1992, 3182, 1501, 2432, 1873, 1599, 1877, 1~
#     $ loan_amount_000s               <int> 227, 240, 241, 351, 417, 199, 885, 338, 383, 169, 255, 316, 70, 234, 343, 177, 252, 275, 260, 41~
#     $ hud_median_family_income       <int> 73300, 57900, 73300, 73300, 78100, 61400, 90300, 90300, 72300, 78100, 69900, 90300, 48700, 61800~
#     $ applicant_income_000s          <int> 116, 42, 117, 315, 114, 37, 150, 59, 110, 55, 60, 92, 73, 168, 110, 53, 69, 120, 119, 216, 41, 4~
    
## glimpse un-numeric
dplyr::glimpse(select_if(data.1,is.character))
# Rows: 311,774
# Columns: 16
# $ property_type_name    <chr> "One-to-four family dwelling (other than manufactured housing)", "One-to-four family dwelling (other than~
# $ owner_occupancy_name  <chr> "Owner-occupied as a principal dwelling", "Owner-occupied as a principal dwelling", "Owner-occupied as a ~
# $ loan_type_name        <chr> "Conventional", "FHA-insured", "Conventional", "Conventional", "Conventional", "Conventional", "Conventio~
# $ loan_purpose_name     <chr> "Refinancing", "Home purchase", "Refinancing", "Refinancing", "Home improvement", "Home purchase", "Refin~
# $ lien_status_name      <chr> "Secured by a first lien", "Secured by a first lien", "Secured by a first lien", "Secured by a first lien~
# $ hoepa_status_name     <chr> "Not a HOEPA loan", "Not a HOEPA loan", "Not a HOEPA loan", "Not a HOEPA loan", "Not a HOEPA loan", "Not ~
# $ county_name           <chr> "Clark County", "Walla Walla County", "Clark County", "Clark County", "Kitsap County", "Skagit County", "~
# $ co_applicant_sex_name <chr> "Male", "No co-applicant", "Female", "Female", "Male", "No co-applicant", "Female", "No co-applicant", "F~
# $ census_tract_number   <chr> "413.27", "9208.01", "414", "405.1", "907", "9515", "323.22", "513", "615", "929.02", "104.03", "214", "2~
# $ applicant_sex_name    <chr> "Female", "Male", "Male", "Male", "Female", "Male", "Male", "Female", "Male", "Male", "Male", "Informatio~
# $ agency_abbr           <chr> "CFPB", "HUD", "HUD", "NCUA", "FDIC", "FDIC", "CFPB", "HUD", "FDIC", "HUD", "HUD", "HUD", "CFPB", "CFPB",~
# $ loan_approved         <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", ~
# $ applicant_race        <chr> "Information not provided by applicant in mail, Internet, or telephone application", "Hispanic or Latino"~
# $ co_applicant_race     <chr> "Information not provided by applicant in mail, Internet, or telephone application", "No co-applicant", "~
# $ denials               <chr> "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "~
# $ county_code           <chr> "011", "071", "011", "011", "035", "057", "033", "061", "053", "035", "073", "033", "077", "063", "011", ~
    
## divide by loan_approved (yes of 251,758, no of 60,016)
data.yes = data.1 %>% filter(loan_approved == 'Yes') %>% select(!c((denials)))# remove denial reasons
data.no = data.1 %>% filter(loan_approved == 'No')


### NOTE.0: loan_approved is the class label!
### NOTE.1: county_code, county_name are actually redundant, they are there for EDA graphics. census_tract_number gives a more detailed geo info.
### NOTE.2: If wanna study the denial reasons in detail, perhaps one does NOT wanna to combine denial reasons.
### NOTE.3: denials is actually redundant for classification.
### NOTE.4: hoepa_status_name is 100% colinear with the response, hence it is recommended to be removed for loan approval classification.
### NOTE.5: Depends on your chosen algorithm, you probably wanna perform some feature scalings (standardization/normalization) to get better results.

## Output to csv
if (output.csv == 'Yes'){
    write.csv(data.1, file = 'cleaned.data.csv')
    write.csv(data.yes, file = 'cleaned.data.approved.csv')
    write.csv(data.no, file = 'cleaned.data.denied.csv')
}

## Good huning, y'all. 

