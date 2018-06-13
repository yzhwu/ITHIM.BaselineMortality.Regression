# ITHIM Project
# Regression analysis - Data preparation - CDPH

# library difinition
library(foreign)

##################### Part I. extract the death numbers from CDPH (by zcta) ###############

# Set working drectory
setwd("/Users/Yizheng/Documents/02_Work/17_ITHIM-Phase II/04_Data/00_baseline mortality/01_Regression")

# input the data file

cdph <- read.spss('TEMPB.sav',to.data.frame = TRUE)
head(cdph)

# Function Definition -------------------------------------------------------------------------
processCDPH <- function(zip.code,year){
  
  # obtain the target data according to county of resident and year of death
  cdph.local.year <- cdph[which(cdph$zipcode==zip.code & cdph$yod%in%year),]
  
  local.gbd <- matrix(NA,nrow = 16,ncol = 1)
  
  for (j in 1:2){ # sex
    for (i in 1:8){ # age group
      #obtain the all-cause mortality for each group
      local.gbd[8*(j-1)+i,1] <- nrow(cdph.local.year[which(cdph.local.year$sex == j & cdph.local.year$age8cat == i),])
    }
  }
  
  
  # defina the dimension names for both matrices
  dimnames(local.gbd) = list(c(paste0("maleAgeClass ",1:8),
                               paste0("femaleAgeClass ",1:8)),zip.code)
  
  # return the matrices
  return(local.gbd)
  
}

# Part 3 Output the result in .csv file --------------------------------------------------------

# CA zip code list
ca.zip.list <- read.csv("ITHIM.BaselineMortality.Regression/zip_list_CA.csv")

no.zip <- nrow(ca.zip.list)
year <- c("2008","2009","2010")

result <- matrix(data = NA,nrow = no.zip * 16, ncol = 3)
colnames(result) <- c("ZIP","AGE.SEX","Death")
head(result)

result[,1] <- rep(ca.zip.list$zcta,each = 16)
result[,2] <- rep(1:16,no.zip)

for (i in 1:no.zip){
  
  print(i)
  temp <- processCDPH(ca.zip.list$zcta[i],year)
  
  result[(16*i-15):(16*i),3] <- temp/3 # annual average of three years result
  
}

write.csv(result,file = "CA_localGBD.allcause.ziplevel_0319.csv")


