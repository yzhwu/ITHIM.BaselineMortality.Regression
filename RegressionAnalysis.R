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


# related variables:
# race:
# B01001H_001E: white alone, not Hispanic or Latino
# B03002_004E: black or African American alone, not Hispanic or Latino
# B01001I_001E: Hispanic or Latino





population.total <- get_acs(geography = "county",variables = "B01001H_001E",year = 2011,survey = "acs5",state = "CA")
population.total
a<-population.total[which(population.total$GEOID%in%c(90001:96162)),]
class(a)
