# ITHIM-Sacramento Fine Spatial Scale Analysis Project
# Baseline mortality regression analysis: Part 2: Data preparation - Census and CDPH

# Clear the environment:
rm(list=ls())

# library definition
library(tidycensus)
library(tidyverse)
library(tigris)  
options(tigris_use_cache = TRUE, tigris_class="sf")

Main_directory <- "/Users/Dana/Dropbox/NM work/NCST Sacramento ITHIM modeling/github/ITHIM.BaselineMortality.Regression" # Dana: windows location
setwd(Main_directory) 

# install the census api key
# obtain the key from https://api.census.gov/data/key_signup.html
census_api_key("6d083b62f8fafc1bb421d71882b7a1b1f9587be5")

# Data sets:
# 1. 2010 Decenial US census
# 2. ACS 5years 2011 (2007-2011) (5-year average value)

## For initial runs only:
## obtain the variable definitions
#v.decenial.10 <- load_variables(2010,"sf1")
#v.acs5.11 <- load_variables(2011,"acs5")
#
## view the variables
#View(v.decenial.10)
#View(v.acs5.11)

##################### Part I. Extract variables from 2010 decennial Census sf1 (for CA zctas) ###############
# Identify the California 2010 zip code tabulation areas. Keep those zctas that are full (not partial)
CA_ZCTAGeography <- zctas(state = "CA", year = 2010) %>% # zcta geography is only available from the decennial census, so we use this list for both decennial and ACS data below.
  filter(!PARTFLG10 == "Y") # exclude 6 observations that are for part of a zcta (this takes it from 1769 tp 1763 zctas)

### Age and Gender
# For both males and females: tally Census age groups according to GBD convention
# ID.1: 0-4,
# ID.2: 5-14,
# ID.3: 15-29,
# ID.4: 30-44,
# ID.5: 45-59,
# ID.6: 60-69,
# ID.7: 70-79,
# ID.8: >80
# ID.AgeGender_PopDet: Population for which age and gender is determined (Total 2010 population in this case)
# And add in some cumulative variables (variable names reflect age ranges).
# And estimating population shares for all variables.
AgeGenderVariables <-c(paste0("P012000",3:9),paste0("P01200",10:25), paste0("P01200",27:49),"P0010001")
AgeGenderRaw <- get_decennial(geography = "zcta", variables = AgeGenderVariables, year = 2010, state = "CA", output = "wide", sumfile = "sf1")  %>%                 
  filter(GEOID %in% CA_ZCTAGeography$ZCTA5CE10)  # filter for just California, no partial zctas
AgeGender <- transmute(AgeGenderRaw, GEOID = GEOID, AgeGender_PopDet = P0010001,
            # estimating GBD age/gender combinations
            male1 = rowSums(AgeGenderRaw[,3]), male2 = rowSums(AgeGenderRaw[,4:5]), male3 = rowSums(AgeGenderRaw[,6:11]),
            male4 = rowSums(AgeGenderRaw[,12:14]), male5 = rowSums(AgeGenderRaw[,15:17]), male6 = rowSums(AgeGenderRaw[,18:21]),
            male7 = rowSums(AgeGenderRaw[,22:23]), male8 = rowSums(AgeGenderRaw[,24:25]),
            female1 = rowSums(AgeGenderRaw[,26]), female2 = rowSums(AgeGenderRaw[,27:28]), female3 = rowSums(AgeGenderRaw[,29:34]),
            female4 = rowSums(AgeGenderRaw[,35:37]), female5 = rowSums(AgeGenderRaw[,38:40]), female6 = rowSums(AgeGenderRaw[,41:44]),
            female7 = rowSums(AgeGenderRaw[,45:46]), female8 = rowSums(AgeGenderRaw[,47:48]), 
            # and then getting them as population shares
            male1Share = male1/AgeGender_PopDet, male2Share = male2/AgeGender_PopDet, male3Share = male3/AgeGender_PopDet,
            male4Share = male4/AgeGender_PopDet, male5Share = male5/AgeGender_PopDet, male6Share = male6/AgeGender_PopDet,
            male7Share = male7/AgeGender_PopDet, male8Share = male8/AgeGender_PopDet,
            female1Share = female1/AgeGender_PopDet, female2Share = female2/AgeGender_PopDet, female3Share = female3/AgeGender_PopDet,
            female4Share = female4/AgeGender_PopDet, female5Share = female5/AgeGender_PopDet, female6Share = female6/AgeGender_PopDet,
            female7Share = female7/AgeGender_PopDet, female8Share = female8/AgeGender_PopDet, 
            # the combined shares (for male and female combined)
            Both1Share = male1Share + female1Share,
            Both2Share = male2Share + female2Share,
            Both3Share = male3Share + female3Share,
            Both4Share = male4Share + female4Share,
            Both5Share = male5Share + female5Share,
            Both6Share = male6Share + female6Share,
            Both7Share = male7Share + female7Share,
            Both8Share = male8Share + female8Share,
            # estimating some more aggregated age categories as shares (for male, female, and combined.) These mostly correspond to grouped GBD categories and likely have more meaning when modeling zip-level health.
            maleShareUnder30 = male1Share + male2Share + male3Share, 
            maleShare30To60 = male4Share + male5Share,
            maleShareOver50 = male8Share + male7Share + male6Share + (P0120016 + P0120017) / AgeGender_PopDet,
            maleShareOver60 = male8Share + male7Share + male6Share,
            maleShareOver70 = male8Share + male7Share,
            maleShareOver80 = male8Share,
            femaleShareUnder30 = female1Share + female2Share + female3Share, 
            femaleShare30To60 = female4Share + female5Share,
            femaleShareOver50 = female8Share + female7Share + female6Share + (P0120040 + P0120041) / AgeGender_PopDet,
            femaleShareOver60 = female8Share + female7Share + female6Share,
            femaleShareOver70 = female8Share + female7Share,
            femaleShareOver80 = female8Share,
            BothShareUnder30 = maleShareUnder30 + femaleShareUnder30, 
            BothShare30To60 = maleShare30To60 + femaleShare30To60, 
            BothShareOver50 = maleShareOver50 + femaleShareOver50,
            BothShareOver60 = maleShareOver60 + femaleShareOver60,
            BothShareOver70 = maleShareOver70 + femaleShareOver70,
            BothShareOver80 = Both8Share)

### Race
# Race/Ethnicity combinations: non-Hispanic White, non-Hispanic Black, and Hispanic or Latino
# And the population for which race/eth is determined, which is the total 2010 population in this case
# And estimating population shares for all variables.
RaceVariables <-c(RaceEth_PopDet = "P0010001", White = "P0050003", Black = "P0050004", Hispanic = "P0040003")
RaceEth <- get_decennial(geography = "zcta", variables = RaceVariables, year = 2010, state = "CA", output = "wide", sumfile = "sf1") %>% 
  filter(GEOID %in% CA_ZCTAGeography$ZCTA5CE10) %>% # filter for just California, no partial zctas
  select(-(NAME)) %>% #remove the name field
  # estimating the population shares
  mutate(WhiteShare = White/RaceEth_PopDet,  
         BlackShare = Black/RaceEth_PopDet, 
         HispanicShare = Hispanic/RaceEth_PopDet) 
 
##################### Part II. Extract variables from 2007-2011 ACS (for CA zctas) ###############
### Education of persons 25 and older
# ID.1: less than high school graduate
# ID.2: high school graduate (includes equivalency)
# ID.3: Some college or associate's degree
# ID.4: Bachelor's degree or higher
# Estimating some cumulative variables:
# ID.1to2: No college (HS or less)
# ID.1to3: Less than a bachelors (some college/associate's or less)
# ID.Edu_PopDet: Population for which educational attainment is determined 
# And estimating population shares for all variables.
EducVariables <- c(Edu_PopDet = "B16010_001E", Edu1 = "B16010_002E", Edu2 = "B16010_015E", Edu3 = "B16010_028E", Edu4 = "B16010_041E")
Education <- get_acs(geography = "zcta", variables = EducVariables, year = 2011, survey = "acs5", output = "wide") %>%
  filter(GEOID %in% CA_ZCTAGeography$ZCTA5CE10) %>% # filter for just California, no partial zctas
  # Gathering and calculating the relevant population (and cumulative and share) variables
  transmute (GEOID = GEOID, 
             Edu_PopDet = Edu_PopDet, 
             Edu1 = Edu1, Edu2 = Edu2, 
             Edu3 = Edu3, Edu4 = Edu4, 
             Edu1to2 = Edu1 + Edu2, 
             Edu1to3 = Edu1 + Edu2 + Edu3, 
             Edu1Share = Edu1/Edu_PopDet, 
             Edu2Share = Edu2/Edu_PopDet, 
             Edu3Share = Edu3/Edu_PopDet, 
             Edu4Share = Edu4/Edu_PopDet, 
             Edu1to2Share = Edu1to2/Edu_PopDet, 
             Edu1to3Share = Edu1to3/Edu_PopDet)

### Poverty: Income in the past 12 months in relation to poverty level
# ID.poverty: living at poverty or below
# ID.poverty2: living at or below twice the poverty level
# ID.Pov_PopDet: Population for which poverty is determined
# And estimating population shares for all variables.
PovVariables <- c(Pov_PopDet = "C17002_001E",Pov_Under50 = "C17002_002E",Pov_50to99 = "C17002_003E", Pov_2andOver = "C17002_008E")
Poverty <- get_acs(geography = "zcta", variables = PovVariables, year = 2011, survey = "acs5", output = "wide") %>% 
  filter(GEOID %in% CA_ZCTAGeography$ZCTA5CE10) %>% # filter for just California, no partial zctas
  # Gathering and calculating the relevant population (and share) variables
  transmute(GEOID = GEOID, 
            Pov_PopDet = Pov_PopDet, 
            Poverty = Pov_Under50 + Pov_50to99, 
            Poverty2 = Pov_PopDet - Pov_2andOver,
            PovertyShare = Poverty/Pov_PopDet, 
            Poverty2Share = Poverty2/Pov_PopDet)

### Household Income (this is Household-level, not population-level):
# ID.Inc_PopDet: HOUSEHOLDS for which household income is determined
# Also estimating some cumulative income brackets and population shares of those brackets (variable names reflect income ranges).
IncomeVariables <- c(Inc_HHDet = "B19001_001E", Inc_Less10000 = "B19001_002E", Inc_10to14999 = "B19001_003E", 
                     Inc_15to19999 = "B19001_004E", Inc_20to24999 = "B19001_005E", Inc_25to29999 = "B19001_006E", 
                     Inc_30to34999 = "B19001_007E", Inc_35to39999 = "B19001_008E", Inc_40to44999 = "B19001_009E",
                     Inc_45to49999 = "B19001_010E", Inc_50to59999 = "B19001_011E", Inc_60to74999 = "B19001_012E",
                     Inc_75to99999 = "B19001_013E", Inc_100to124999 = "B19001_014E", Inc_125to149999 = "B19001_015E",
                     Inc_150to199999 = "B19001_016E", Inc_200orMore = "B19001_017E") #including all categories for reference or potential future adjustments
Income <- get_acs(geography = "zcta", variables = IncomeVariables, year = 2011, survey = "acs5", output = "wide") %>% 
  filter(GEOID %in% CA_ZCTAGeography$ZCTA5CE10) %>% # filter for just California, no partial zctas
  # Gathering and calculating the relevant household (and share) variables. Could do other categories if needed.
  transmute(GEOID = GEOID, 
            Inc_HHDet = Inc_HHDet,
            Income_Less20K = Inc_Less10000 + Inc_10to14999 + Inc_15to19999,
            Income_Less30K = Income_Less20K + Inc_20to24999 + Inc_25to29999,
            Income_Less40K = Income_Less30K + Inc_30to34999 +Inc_35to39999, 
            Income_Less50K = Income_Less40K + Inc_40to44999 + Inc_45to49999, 
            Income_Less60K = Income_Less50K + Inc_50to59999,
            Income_Less75K = Income_Less60K + Inc_60to74999,
            Income_Less100K = Income_Less75K + Inc_75to99999,
            Income_Less20KShare = Income_Less20K/Inc_HHDet, 
            Income_Less30KShare = Income_Less30K/Inc_HHDet, 
            Income_Less40KShare = Income_Less40K/Inc_HHDet, 
            Income_Less50KShare = Income_Less50K/Inc_HHDet, 
            Income_Less60KShare = Income_Less60K/Inc_HHDet, 
            Income_Less75KShare = Income_Less75K/Inc_HHDet, 
            Income_Less100KShare = Income_Less100K/Inc_HHDet) 
 
### Employment status for the population 16 years and over
# ID.Inc_PopDet: Population for which employment is determined 
# ID.Unemployed: In labor force; Civilian labor force; Unemployed
# And estimating population share for unemployment.
EmpVariables <- c(Emp_PopDet = "B23025_001E", Unemployed = "B23025_005E")
Employment <- get_acs(geography = "zcta", variables = EmpVariables, year = 2011, survey = "acs5", output = "wide") %>% 
  filter(GEOID %in% CA_ZCTAGeography$ZCTA5CE10) %>% # filter for just California, no partial zctas
  transmute(GEOID = GEOID, Emp_PopDet = Emp_PopDet, UnemployedShare = Unemployed/Emp_PopDet) # grab relevant variables and estimate population shares

##################### Part III. Bring in mortality data from CDPH (for CA zip codes) ###############
# This is 3-year mortality data for California for years 2008 - 2010
Mortality <- read.csv("CA_localGBD.allcause.ziplevel.csv") %>%
  transmute(AgeSex = AGE.SEX, DeathPerYear = Death, GEOID = as.character(ZIP)) %>%
  # aggregating deaths across all age/gender categories in a zip.
  group_by(GEOID) %>%
  summarize(AllDeathPerYear = sum(DeathPerYear, na.rm = TRUE)) 

##################### Part IV. Merge Census data with CDPH mortality data (matching zcta to zip) ###############

## Join mortality and demographic data by GEOID
MortalityRegressionDataFull <- full_join (Mortality, AgeGender, by="GEOID") %>% 
  full_join (RaceEth, by="GEOID") %>% 
  full_join (Education, by="GEOID") %>% 
  full_join (Poverty, by="GEOID") %>% 
  full_join (Income, by="GEOID") %>% 
  full_join (Employment, by="GEOID") %>%
  mutate(AnnualDeathPerCap = AllDeathPerYear/AgeGender_PopDet) # estimate per capita deaths based on 2010 decennial total population.

# Take a look at the population totals. Note that income is households, employment is those over 16, and education is those over 25. Also minor differences between 2010 vs 2007-2011 counts.
View(select(MortalityRegressionDataFull, AgeGender_PopDet, RaceEth_PopDet, Edu_PopDet, Pov_PopDet, Inc_HHDet, Emp_PopDet))

# Create a short version that just includes key variables.
MortalityRegressionDataShort <- select(MortalityRegressionDataFull, 
                                       GEOID, 
                                       Pop2010 = AgeGender_PopDet,
                                       AnnualDeathPerCap, 
                                       BothShareUnder30, BothShare30To60, BothShareOver50, BothShareOver60, BothShareOver70, BothShareOver80,
                                       WhiteShare, BlackShare, HispanicShare,
                                       Edu1Share, Edu1to2Share,
                                       PovertyShare, Poverty2Share,
                                       Income_Less20KShare, Income_Less30KShare, Income_Less40KShare, Income_Less50KShare, Income_Less60KShare, Income_Less75KShare, Income_Less100KShare,  
                                       UnemployedShare)


# Export the full and short version of the regression data. I am including the population value in each dataset but NOT excluding low population tracts.
write.csv(filter(MortalityRegressionDataFull),file = "MortalityRegressionDataFull.csv")
write.csv(filter(MortalityRegressionDataShort), file = "MortalityRegressionDataShort.csv")




