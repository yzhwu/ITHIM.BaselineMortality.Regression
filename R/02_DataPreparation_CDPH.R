# ITHIM Project
# Regression analysis - Data preparation - CDPH

# library difinition
library(foreign)

##################### Part III. extract the death numbers from CDPH (by zcta) ###############

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

# CA zip code list (range: 90000)

no.zip <- length(zcta.CA.all)
year <- c("2008","2009","2010")

result <- NULL
for (i in 1:10){
  #print(i)
  temp <- processCDPH(zip.code.all[i],year)
  result <- cbind(result,temp)
}

write.csv(result/3,file = "CA_localGBD.allcause.ziplevel.csv")


