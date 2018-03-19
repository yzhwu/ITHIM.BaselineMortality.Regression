# ITHIM Project
# Regression analysis - Data preparation - Census

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

# obtain the variable definition
v.decenial.10 <- load_variables(2010,"sf1")
v.acs5.11 <- load_variables(2011,"acs5")

# view the variables
View(v.decenial.10)
View(v.acs5.11)

##################### Part I. extract the variables from 2010 sf1 (by zcta) ###############
### total population
population.total <- get_decennial(geography = "zcta",variables = "P0010001",year = 2010,state = "CA")

#zcta.CA.all <- unique(population.total$GEOID[which(population.total$GEOID%in%c(90001:96162))])

### population - sex by age

# male
variable.list.male <- c(paste0("P012000",3:9),paste0("P01200",10:25))

pop.male <- NULL

for (i in 1:length(variable.list.male)){
  
  temp <- get_decennial(geography = "zcta",variables = variable.list.male[i],year = 2010,state = "CA")
  
  pop.male <- cbind(pop.male,temp$value)
  
}

# female
variable.list.female <- paste0("P01200",27:49)

pop.female <- NULL

for (i in 1:length(variable.list.female)){
  temp <- get_decennial(geography = "zcta",variables = variable.list.female[i],year = 2010,state = "CA")
  
  pop.female <- cbind(pop.female,temp$value)
  
}

# reorganize the age group, according to GBD rule
# ID.1: 0-4,
# ID.2: 5-14,
# ID.3: 15-29,
# ID.4: 30-44,
# ID.5: 45-59,
# ID.6: 60-69,
# ID.7: 70-79,
# ID.8: >80
pop.male.age <- cbind(pop.male[,1],(rowSums(pop.male[,2:3])),(rowSums(pop.male[,4:9])),(rowSums(pop.male[,10:12])),
                      (rowSums(pop.male[,13:15])),(rowSums(pop.male[,16:19])),(rowSums(pop.male[,20:21])),(rowSums(pop.male[,22:23]))) 
colnames(pop.male.age) <- paste0("male.",1:8)

pop.female.age <- cbind(pop.female[,1],(rowSums(pop.female[,2:3])),(rowSums(pop.female[,4:9])),(rowSums(pop.female[,10:12])),
                      (rowSums(pop.female[,13:15])),(rowSums(pop.female[,16:19])),(rowSums(pop.female[,20:21])),(rowSums(pop.female[,22:23]))) 
colnames(pop.female.age) <- paste0("female.",1:8)

# check the equation: total population = pop.male + pop.female
identical(population.total$value,rowSums(pop.female.age)+rowSums(pop.male.age))

### Race (non-Hispanic White, non-Hispanic Black, Hispanic or Latino)
pop.white <- get_decennial(geography = "zcta",variables = "P0050003",year = 2010,state = "CA")
pop.black <- get_decennial(geography = "zcta",variables = "P0050004",year = 2010,state = "CA")
pop.Hisp <- get_decennial(geography = "zcta",variables = "P0040003",year = 2010,state = "CA")

pop.race.percentage <- cbind(pop.white$value/population.total$value,
                             pop.black$value/population.total$value,pop.Hisp$value/population.total$value)
colnames(pop.race.percentage) <- c("White%","Black%","Hisp%")

##################### Part II. extract the variables from ACS 5 years 2007-2011 (by zcta) ###############
### Education
# ID.1: less than high school graduate
# ID.2: high school graduate (includes equivalency)
# ID.3: Some college or associate's degree
# ID.4: Bachelor's degree or higher
pop.edu.1 <- get_acs(geography = "zcta",variables = "B16010_002E",year = 2011,survey = "acs5")
pop.edu.1.ca <- pop.edu.1[which(pop.edu.1$GEOID%in%ca.zip.list$zcta),]
pop.edu.2 <- get_acs(geography = "zcta",variables = "B16010_015E",year = 2011,survey = "acs5")
pop.edu.2.ca <- pop.edu.2[which(pop.edu.2$GEOID%in%ca.zip.list$zcta),]
pop.edu.3 <- get_acs(geography = "zcta",variables = "B16010_028E",year = 2011,survey = "acs5")
pop.edu.3.ca <- pop.edu.3[which(pop.edu.3$GEOID%in%ca.zip.list$zcta),]
pop.edu.4 <- get_acs(geography = "zcta",variables = "B16010_041E",year = 2011,survey = "acs5")
pop.edu.4.ca <- pop.edu.4[which(pop.edu.4$GEOID%in%ca.zip.list$zcta),]

### Poverty: Income in the past 12 months below poverty level
pop.poverty <- get_acs(geography = "zcta",variables = "B17001_002E",year = 2011,survey = "acs5")
pop.poverty.ca <- pop.poverty[which(pop.poverty$GEOID%in%ca.zip.list$zcta),]

### Household Income:
variable.list.income <- c(paste0("B19001_00",2:9,"E"),paste0("B19001_0",10:17,"E"))

pop.income.ca <- NULL

for (i in 1:length(variable.list.income)){
  
  temp <- get_acs(geography = "zcta",variables = variable.list.income[i],year = 2011,survey = "acs5")
  temp.ca <- temp[which(temp$GEOID%in%ca.zip.list$zcta),]
  
  pop.income.ca <- cbind(pop.income.ca,temp.ca$estimate)
  
}



### Employment status for the population 16 years and over
#  In labor force; Civilian labor force; Unemployed
pop.unemployed <- get_acs(geography = "zcta",variables = "B23025_005E",year = 2011,survey = "acs5")
pop.unemployed.ca <- pop.unemployed[which(pop.unemployed$GEOID%in%ca.zip.list$zcta),]


#population.total <- get_acs(geography = "zcta",variables = "B16010_002E",year = 2011,survey = "acs5")

#population.total <- get_acs(geography = "county",variables = "B17001_002E",year = 2011,survey = "acs5",state = "CA")

#population.total
#a<-population.total[which(population.total$GEOID%in%c(90001:96162)),]
#class(a)


### merge census datasets with death rates from CDPH
setwd("/Users/Yizheng/Documents/02_Work/17_ITHIM-Phase II/04_Data/00_baseline mortality/01_Regression")
death.cdph <- read.csv("CA_localGBD.allcause.ziplevel.csv")
colnames(death.cdph) <- c("ID","GEOID","AGE.SEX","DeathCount")

ca.zip.list <- read.csv("ITHIM.BaselineMortality.Regression/zip_list_CA.csv")
no.zip <- nrow(ca.zip.list)

pop.age.gender <- matrix(data = NA,nrow = no.zip * 16, ncol = 3)
colnames(pop.age.gender) <- c("ZIP","AGE.SEX","POP")
head(pop.age.gender)

pop.age.gender[,1] <- rep(ca.zip.list$zcta,each = 16)
pop.age.gender[,2] <- rep(1:16,no.zip)

pop.total.ca <- matrix(data = NA,nrow = no.zip, ncol = 2)

race.percentage.ca <- matrix(data = NA,nrow = no.zip, ncol = 4)
pop.total.ca[,1] <-  race.percentage.ca[,1] <- ca.zip.list$zcta

for (i in 1:no.zip){
  
  temp.male <- pop.male.age[which(ca.zip.list[i,2]==population.total$GEOID),]
  temp.female <- pop.female.age[which(ca.zip.list[i,2]==population.total$GEOID),]
  
  pop.age.gender[(16*i-15):(16*i),3] <- c(temp.male,temp.female)
  
  temp.race <- pop.race.percentage[which(ca.zip.list[i,2]==population.total$GEOID),]
  race.percentage.ca[i,2:4]<-temp.race
  
  temp.pop.total <- population.total[which(ca.zip.list[i,2]==population.total$GEOID),]
  pop.total.ca[i,2] <- temp.pop.total$value
  
}

death.cdph <- merge(death.cdph,population.total,by = "GEOID")

#merge all variables into one table
regression.data <- cbind(death.cdph[,c(1:3)],(death.cdph$DeathCount/pop.age.gender[,3]*1000),race.percentage.ca[,2:4][rep(1:no.zip,rep(16,no.zip)),],rep(pop.edu.1.ca$estimate/pop.total.ca[,2],each=16),rep(pop.edu.2.ca$estimate/pop.total.ca[,2],each=16),
                         rep(pop.edu.3.ca$estimate/pop.total.ca[,2],each=16),rep(pop.edu.4.ca$estimate/pop.total.ca[,2],each=16),rep(pop.poverty.ca$estimate/pop.total.ca[,2],each = 16),(pop.income.ca[,1:16]/pop.total.ca[,2])[rep(1:no.zip,rep(16,no.zip)),],
                        rep(pop.unemployed.ca$estimate/pop.total.ca[,2],each=16))

head(regression.data)

colnames(regression.data) <- c("ZIP","ID","AGE.SEX","Death.Rate","White%","Black%","Hisp%","EDU.1",
                               "EDU.2","EDU.3","EDU.4","Poverty%",paste0("Income.",1:16),"Unemployed%")

write.csv(regression.data,file = "regression_data.csv")




