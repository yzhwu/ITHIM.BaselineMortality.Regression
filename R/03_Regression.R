# ITHIM Project
# Regression analysis - Multivariate logistic regression

# Set working drectory
setwd("/Users/Yizheng/Documents/02_Work/17_ITHIM-Phase II/04_Data/00_baseline mortality/01_Regression")

# input the data
data <- read.csv("regression_data_v.csv",header = TRUE)
head(data)

# create new data frame with all missing values removed 
Data.omit <- na.omit(data)

# Define full and null models and do step procedure
model.null <- glm(Death.Rate~1,
                  data = Data.omit,
                  family = binomial(link = "logit")
                  )

model.full = glm(Death.Rate ~ as.factor(AGE.SEX) + White + Black + Hisp + EDU.1 + EDU.2 + 
                   EDU.3 + EDU.4 + Poverty + Income.1 + Income.2 + Income.3 + Income.4 +  Income.5 +
                   Income.6 + Income.7 + Income.8 + Income.9 + Income.10 + Income.11 + Income.12 + 
                   Income.13 + Income.14 +  Income.15 + Income.16 + Unemployed,
                 data=Data.omit,
                 family = binomial(link="logit")
)

step(model.null,
     scope = list(upper=model.full),
     direction="both",
     test="Chisq",
     data=Data.omit)

model.final <- glm(Death.Rate ~ as.factor(AGE.SEX),
                   data = Data.omit,
                   family = binomial(link = "logit")
                   )
summary(model.final)


plot(fitted(model.final),rstandard(model.final))

