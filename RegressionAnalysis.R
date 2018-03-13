# ITHIM Project
# Regression analysis

# library difinition
library(tidycensus)
library(tidyverse)
library(viridis)

# install the census api key
# obtain the key from https://api.census.gov/data/key_signup.html
census_api_key("5a65de552c52e90c3a9a2683d73a38140ccc3082")

# Data sets:
# 1. 2010 Decenial US census
# 2. ACS 5years 2011 (2007-2011) (5-year average value)
# 3. CDPH zip level data: 2008-2010

# obtain the variable definition
v.decenial.10 <- load_variables(2010,"sf1")
v.acs5.11 <- load_variables(2011,"acs5")

# view the variables
View(v.acs5.11)
View(v.decenial.10)

###### Related variables (independent variables)

#####  I. Decenial 2010 #####
# Total population:
# P0010001: total population

# Population - Sex by Age
# P0120003  Male: !! Under 5 years
# P0120004	Male: !! 5 to 9 years
# P0120005	Male: !! 10 to 14 years
# P0120006	Male: !! 15 to 17 years
# P0120007	Male: !! 18 and 19 years
# P0120008	Male: !! 20 years
# P0120009	Male: !! 21 years
# P0120010	Male: !! 22 to 24 years
# P0120011	Male: !! 25 to 29 years
# P0120012	Male: !! 30 to 34 years
# P0120013	Male: !! 35 to 39 years
# P0120014	Male: !! 40 to 44 years

# P0120027	Female: !! Under 5 years	
# P0120028	Female: !! 5 to 9 years	

# Race:
# P0050003: Not Hispanic or Latino: !! White alone
# P0050004: Not Hispanic or Latino: !! Black or African American alone
# P0040003: Hispanic or Latino


#####  II. ACS 5 2007-2011 #####

# Total population:
# P0010001: total population - decenial
# B01003_001E: total population

# Race:
# B01001H_001E: white alone, not Hispanic or Latino
# B03002_004E: black or African American alone, not Hispanic or Latino
# B01001I_001E: Hispanic or Latino

# Eductional attainment for the population 25 years and over
# B16010_002E: less than high school graduate
# B16010_015E: high school graduate (includes equivalency)
# B16010_028E: Some college or associate's degree
# B16010_041E: Bachelor's degree or higher

# Poverty
# B17001_002E: Income in the past 12 months below poverty level

# Household income
# B19001_002E: Less than $10,000
# B19001_003E: $10,000 to $14,999
# B19001_004E: $15,000 to $19,999
# B19001_005E: $20,000 to $24,999
# B19001_006E: $25,000 to $29,999
# B19001_007E: $30,000 to $34,999
# B19001_008E: $35,000 to $39,999
# B19001_009E: $40,000 to $44,999
# B19001_010E: $45,000 to $49,999
# B19001_011E: $50,000 to $59,999
# B19001_012E: $60,000 to $74,999
# B19001_013E: $75,000 to $99,999
# B19001_014E: $100,000 to $124,999
# B19001_015E: $125,000 to $149,999
# B19001_016E: $150,000 to $199,999
# B19001_007E: $200,000 or more

# Employment status for the population 16 years and over
# B23025_005E: In labor force; Civilian labor force; Unemployed


population.total <- get_acs(geography = "county",variables = "B17001_002E",year = 2011,survey = "acs5",state = "CA")
population.total
a<-population.total[which(population.total$GEOID%in%c(90001:96162)),]
class(a)
