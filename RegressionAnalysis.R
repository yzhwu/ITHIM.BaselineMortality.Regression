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

population.total <- get_decennial(geography = "zcta",variables = "P0010001",year = 2010,state = "CA")
population.total[1750:1769,]


population.total <- get_acs(geography = "zcta",variables = "B16010_002E",year = 2011,survey = "acs5")

population.total <- get_acs(geography = "county",variables = "B17001_002E",year = 2011,survey = "acs5",state = "CA")

population.total
a<-population.total[which(population.total$GEOID%in%c(90001:96162)),]
class(a)
