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

logistic.regression.or.ci <- function(regress.out, level=0.95)
{
  ################################################################
  #                                                              #
  #  This function takes the output from a glm                   #
  #  (logistic model) command in R and provides not              #
  #  only the usual output from the summary command, but         #
  #  adds confidence intervals for all coefficients and OR's.    #
  #                                                              #
  #  This version accommodates multiple regression parameters    #
  #                                                              #
  ################################################################
  usual.output <- summary(regress.out)
  z.quantile <- qnorm(1-(1-level)/2)
  number.vars <- length(regress.out$coefficients)
  OR <- exp(regress.out$coefficients[-1])
  temp.store.result <- matrix(rep(NA, number.vars*2), nrow=number.vars)
  for(i in 1:number.vars)
  {
    temp.store.result[i,] <- summary(regress.out)$coefficients[i] +
      c(-1, 1) * z.quantile * summary(regress.out)$coefficients[i+number.vars]
  }
  intercept.ci <- temp.store.result[1,]
  slopes.ci <- temp.store.result[-1,]
  OR.ci <- exp(slopes.ci)
  output <- list(regression.table = usual.output, intercept.ci = intercept.ci,
                 slopes.ci = slopes.ci, OR=OR, OR.ci = OR.ci)
  return(output)
}

logistic.regression.or.ci(model.final)


plot(fitted(model.final),rstandard(model.final))

