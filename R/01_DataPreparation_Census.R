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

# compute the percentage of each race category
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
pop.edu.1.ca <- pop.edu.1[which(pop.edu.1$GEOID%in%ca.zip.list$zcta),] # target zcta
pop.edu.2 <- get_acs(geography = "zcta",variables = "B16010_015E",year = 2011,survey = "acs5")
pop.edu.2.ca <- pop.edu.2[which(pop.edu.2$GEOID%in%ca.zip.list$zcta),] # target zcta
pop.edu.3 <- get_acs(geography = "zcta",variables = "B16010_028E",year = 2011,survey = "acs5")
pop.edu.3.ca <- pop.edu.3[which(pop.edu.3$GEOID%in%ca.zip.list$zcta),] # target zcta
pop.edu.4 <- get_acs(geography = "zcta",variables = "B16010_041E",year = 2011,survey = "acs5")
pop.edu.4.ca <- pop.edu.4[which(pop.edu.4$GEOID%in%ca.zip.list$zcta),] # target zcta

### Poverty: Income in the past 12 months below poverty level
pop.poverty <- get_acs(geography = "zcta",variables = "B17001_002E",year = 2011,survey = "acs5")
pop.poverty.ca <- pop.poverty[which(pop.poverty$GEOID%in%ca.zip.list$zcta),] # target zcta

### Household Income:
# variables related to household income
variable.list.income <- c(paste0("B19001_00",2:9,"E"),paste0("B19001_0",10:17,"E"))

# obtain all variables
pop.income.ca <- NULL

for (i in 1:length(variable.list.income)){
  
  temp <- get_acs(geography = "zcta",variables = variable.list.income[i],year = 2011,survey = "acs5")
  temp.ca <- temp[which(temp$GEOID%in%ca.zip.list$zcta),] # target zcta
  
  pop.income.ca <- cbind(pop.income.ca,temp.ca$estimate)
  
}



### Employment status for the population 16 years and over
#  In labor force; Civilian labor force; Unemployed
pop.unemployed <- get_acs(geography = "zcta",variables = "B23025_005E",year = 2011,survey = "acs5")
pop.unemployed.ca <- pop.unemployed[which(pop.unemployed$GEOID%in%ca.zip.list$zcta),] # target zcta

##################### Part III. merge census datasets with death rates from CDPH (by zcta) ###############

setwd("/Users/Yizheng/Documents/02_Work/17_ITHIM-Phase II/04_Data/00_baseline mortality/01_Regression")
# input death files
death.cdph <- read.csv("CA_localGBD.allcause.ziplevel.csv")
colnames(death.cdph) <- c("ID","GEOID","AGE.SEX","DeathCount")

# input target ZCTAs
ca.zip.list <- read.csv("ITHIM.BaselineMortality.Regression/zip_list_CA.csv")

# number of zips (1763)
no.zip <- nrow(ca.zip.list)

# obtain the information of target zctas from decennial 2010
# pop by age and gender for each zcta
pop.age.gender <- matrix(data = NA,nrow = no.zip * 16, ncol = 3)
colnames(pop.age.gender) <- c("ZIP","AGE.SEX","POP")
head(pop.age.gender)
pop.age.gender[,1] <- rep(ca.zip.list$zcta,each = 16)
pop.age.gender[,2] <- rep(1:16,no.zip)

# total pop for each zcta
pop.total.ca <- matrix(data = NA,nrow = no.zip, ncol = 2)

# race percentage for each zcta
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

# merge all variables into one table
death.cdph <- merge(death.cdph,population.total,by = "GEOID")

regression.data <- cbind(death.cdph[,c(1:3)],(death.cdph$DeathCount/pop.age.gender[,3]),race.percentage.ca[,2:4][rep(1:no.zip,rep(16,no.zip)),],rep(pop.edu.1.ca$estimate/pop.total.ca[,2],each=16),rep(pop.edu.2.ca$estimate/pop.total.ca[,2],each=16),
                         rep(pop.edu.3.ca$estimate/pop.total.ca[,2],each=16),rep(pop.edu.4.ca$estimate/pop.total.ca[,2],each=16),rep(pop.poverty.ca$estimate/pop.total.ca[,2],each = 16),(pop.income.ca[,1:16]/pop.total.ca[,2])[rep(1:no.zip,rep(16,no.zip)),],
                        rep(pop.unemployed.ca$estimate/pop.total.ca[,2],each=16))

head(regression.data)

colnames(regression.data) <- c("ZIP","ID","AGE.SEX","Death.Rate","White","Black","Hisp","EDU.1",
                               "EDU.2","EDU.3","EDU.4","Poverty",paste0("Income.",1:16),"Unemployed")

# delete those records with NA's 
delete.zcta <- unique(regression.data$ZIP[which(is.na(regression.data$Death.Rate)==TRUE|is.infinite(regression.data$Death.Rate)==TRUE)])   

# delete those sparsely-populated zctas (less than 500 population)
less500.zcta <- unique(population.total$GEOID[which(population.total$value<500)])

regression.data.v <- regression.data[-(which(regression.data$ZIP%in%c(delete.zcta,less500.zcta))),]

#regression.data.v <- 

# output the data sets
write.csv(regression.data.v,file = "regression_data_v.csv")

##################### Part IV. Prepare the datasets for Model 1 ###############

#total population
population.total <- get_decennial(geography = "zcta",variables = "P0010001",year = 2010,state = "CA")

# total pop for each zcta
zcta.ca <- matrix(data = NA,nrow = no.zip, ncol = 24)

colnames(zcta.ca)<-c("zcta","total population","Death Count","non-Hisp White","non-Hisp Black","Hisp or Latino",
                     "Poverty","Edu1",paste0("male",1:8),paste0("female",1:8))

zcta.ca[,1] <- ca.zip.list$zcta

for (i in 1:no.zip){
  # total population
  temp.pop.total <- population.total[which(ca.zip.list[i,2]==population.total$GEOID),]
  zcta.ca[i,2] <- temp.pop.total$value
  
  # total death count (three-year average)
  temp.death.count <- sum(death.cdph$DeathCount[which(ca.zip.list[i,2]==death.cdph$GEOID)])
  zcta.ca[i,3] <- temp.death.count
  
  # non-Hisp White
  temp.white <- pop.white[which(ca.zip.list[i,2]==pop.white$GEOID),]
  zcta.ca[i,4] <- temp.white$value
  
  # non-Hisp Black
  temp.black <- pop.black[which(ca.zip.list[i,2]==pop.black$GEOID),]
  zcta.ca[i,5] <- temp.black$value
  
  # non-Hisp Hisp or Latino
  temp.hisp <- pop.Hisp[which(ca.zip.list[i,2]==pop.Hisp$GEOID),]
  zcta.ca[i,6] <- temp.hisp$value
  
  # poverty: Income in the past 12 months below poverty level
  temp.poverty <- pop.poverty.ca[which(ca.zip.list[i,2]==pop.poverty.ca$GEOID),]
  zcta.ca[i,7] <- temp.poverty$estimate
  
  # Edu1: less than high school graduate
  temp.Edu1 <- pop.edu.1.ca[which(ca.zip.list[i,2]==pop.edu.1.ca$GEOID),]
  zcta.ca[i,8] <- temp.Edu1$estimate
  
  # Male
  temp.male <- pop.male.age[which(ca.zip.list[i,2]==population.total$GEOID),]
  zcta.ca[i,9:16]<-temp.male
  
  # Female
  temp.female <- pop.female.age[which(ca.zip.list[i,2]==population.total$GEOID),]
  zcta.ca[i,17:24]<-temp.female
  
}

write.csv(zcta.ca,file = "regression_data_model_1.csv")

