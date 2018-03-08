# ITHIM Project
# Regression analysis

# library difinition
library(tidycensus)
library(tidyverse)
library(viridis)

# install the census api key
# obtain the key from https://api.census.gov/data/key_signup.html
census_api_key("5a65de552c52e90c3a9a2683d73a38140ccc3082")

# data set pick up: ACS 5years 2011 (2007-2011) (5-year average value)
# CDPH zip level data: 2008-2010

# obtain the variable definition
v.acs5.11 <- load_variables(2011,"acs5")

# view the variables
View(v.acs5.11)

###### Related variables (independent variables)

# Total population:
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
