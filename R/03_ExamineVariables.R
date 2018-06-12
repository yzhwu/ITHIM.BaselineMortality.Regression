# ITHIM-Sacramento Fine Spatial Scale Analysis Project
# Checking the new demographic / mortality data 

# Clear the environment:
rm(list=ls())

# library definition
library(tidyverse)
library(corrplot)

Main_directory <- "/Users/Dana/Dropbox/NM work/NCST Sacramento ITHIM modeling/github/ITHIM.BaselineMortality.Regression" # Dana: windows location
setwd(Main_directory) 

#### 1.0 Bring in data ####
MortalityRegressionDataFull <- read.csv("MortalityRegressionDataFull.csv")
MortalityRegressionDataShort <- read.csv("MortalityRegressionDataShort.csv")

## @knitr ExamineData
#### 2.0 Examine the data ####
# Picking a few variables to eyeball their distributions
ggplot(MortalityRegressionDataShort, aes(x = AnnualDeathPerCap)) +
  geom_histogram(stat = "bin", binwidth = 0.001)
ggplot(MortalityRegressionDataShort, aes(x = BothShareUnder30)) +
  geom_histogram(stat = "bin", binwidth = 0.005)
ggplot(MortalityRegressionDataShort, aes(x = WhiteShare)) +
  geom_histogram(stat = "bin", binwidth = 0.005)
ggplot(MortalityRegressionDataShort, aes(x = Edu1Share)) +
  geom_histogram(stat = "bin", binwidth = 0.005)
ggplot(MortalityRegressionDataShort, aes(x = PovertyShare)) +
  geom_histogram(stat = "bin", binwidth = 0.005)
ggplot(MortalityRegressionDataShort, aes(x = Poverty2Share)) +
  geom_histogram(stat = "bin", binwidth = 0.005)
ggplot(MortalityRegressionDataShort, aes(x = Income_Less20KShare)) +
  geom_histogram(stat = "bin", binwidth = 0.005)
ggplot(MortalityRegressionDataShort, aes(x = UnemployedShare)) +
  geom_histogram(stat = "bin", binwidth = 0.005)
# And a few scatter plots to see about correlations
MortalityRegressionDataShortCorrel <- cor(MortalityRegressionDataShort, use = "complete.obs") # added the "use" parameter because it looks like there are some NAs in the ACS fields. Consider removing those observations.
head(round(MortalityRegressionDataShortCorrel,2))
corrplot(MortalityRegressionDataShortCorrel, type="upper")

## @knitr ModelExploration
#### 3.0 Try to model the new data, briefly? ####
# Start with a basic model based on some intuition.
model.test = glm(AnnualDeathPerCap ~ BlackShare + Poverty2Share + BothShareOver60 + Edu1to2Share,
                 data=MortalityRegressionDataShort,
                 family = binomial(link="logit")
)
summary(model.test)

### What would happen if I transform the data?
TestTransform <- mutate(MortalityRegressionDataShort, logDeathRate = log(AnnualDeathPerCap), DeathRate = Pop2010 * AnnualDeathPerCap)
ggplot(TestTransform, aes(x = logDeathRate)) +
  geom_histogram(stat = "bin", binwidth = 0.05) # This kind of looks normal. Can we do OLS on logDeathRate?
# Is the transformed data normal?
library(ggpubr)
TestTransformSubset <- filter(TestTransform, DeathRate > 0)  # Excluding zips with 0 deaths
ggqqplot(TestTransformSubset$logDeathRate) # doesn't look great.
shapiro.test(TestTransformSubset$logDeathRate) # very significant - so def not normal.
# by the way - is untransformed death rate normal?
UntransformedSubset <- filter(MortalityRegressionDataShort, Pop2010 > 0)  # Excluding zips with 0 population
ggqqplot(UntransformedSubset$AnnualDeathPerCap) # doesn't look great.
shapiro.test(UntransformedSubset$AnnualDeathPerCap) # definitely not!

# Curious about an OLS with logDeathRate.
TestModel <- lm(logDeathRate ~ WhiteShare + PovertyShare + BothShareOver60, data=TestTransformSubset)
summary(TestModel)
# This has significance, but we know that its not a proper specification.

# What about OLS on the untransformed data, out of curiousity?
TestModel2 <- lm(AnnualDeathPerCap ~ WhiteShare + PovertyShare + BothShareOver60, data=MortalityRegressionDataShort)
summary(TestModel2)
# This one also has results.  I don't think this is a propor model either, but I'm leaning toward revisiting the specification.

# Some discussion of model spec here: https://www.theanalysisfactor.com/proportions-as-dependent-variable-in-regression-which-type-of-model/
# logistic or probit, treat proportion as a binary response 
# two-limit tobit model
# Other online discussions mention a beta distribution.
# Also - would it make sense to weight each zip inversely proportional to its population, since small pop zips are noisier?
# NEEDS MORE READING.  

# In the meantime just for kicks, trying a Cox proportional hazard.
library(survival)
TestCox <- coxph(AnnualDeathPerCap ~ WhiteShare + PovertyShare + BothShareOver60, data=MortalityRegressionDataShort)
# doesn't work, I think because because the dependent variable (death rate) isn't the status of an individual.

# For kicks, trying a two-limit tobit.
library(censReg)
TestTobit <- censReg(AnnualDeathPerCap ~ WhiteShare + PovertyShare + BothShareOver60, left = 0, right = 1, data=MortalityRegressionDataShort)
summary(TestTobit)
# This has explanatory power.  Need to read more to figure out if this is an appropriate specification.

# Looking again - does the dependent variable look censored from two sides (like a tobit would?)
ggplot(MortalityRegressionDataShort, aes(x = AnnualDeathPerCap)) +
  geom_histogram(stat = "bin", binwidth = 0.005)
