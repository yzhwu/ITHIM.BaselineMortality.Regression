# ITHIM Project
# Regression analysis - Multivariate logistic regression

# Set working drectory
setwd("/Users/Yizheng/Documents/02_Work/17_ITHIM-Phase II/04_Data/00_baseline mortality/01_Regression")

# input the data
data <- read.csv("regression_data_v.csv",header = TRUE)
head(data)

# split the data (90% train, 10% test)
# set.seed(100)
# index <- sample(1:nrow(data), 0.9*nrow(data))

# data.train <- data[index,]
# data.test <- data[-index,]

# create new data frame with all missing values removed 
data.c <- na.omit(data)
#data.test <- na.omit(data.test)


######## Regression in a single equation
# select variables
data.v <- cbind(data.c$Death.Rate,data.c$AGE.SEX,data.c$Black,data.c$Hisp,data.c$EDU.4,data.c$Poverty,data.c$Unemployed)

#data.v <- cbind(data.c$Death.Rate,data.c$AGE.SEX,data.c$White,data.c$EDU.4,(data.c$Income.14+data.c$Income.15+data.c$Income.16),data.c$Unemployed)


colnames(data.v) <- c("Death.Rate","AGE.SEX","Black","Hisp","EDU.4","Poverty","Unemployed")
head(data.v)
data.v <- as.data.frame(data.v)

# Define full and null models and do step procedure
model.null <- glm(Death.Rate~1,
                  data = data.v,
                  family = binomial(link = "logit")
                  )

model.full = glm(Death.Rate ~ Black + as.factor(AGE.SEX) + Hisp + EDU.4 + Poverty + Unemployed,
                 data=data.v,
                 family = binomial(link="logit")
)

step(model.null,
     scope = list(upper=model.full),
     direction="both",
     test="Chisq",
     data=data.v)

model.final <- glm(Death.Rate ~ as.factor(AGE.SEX),
                   data = data.v,
                   family = binomial(link = "logit")
                   )
summary(model.final)

####### Regression seperately for each group

# age.sex group 1

data.c.1 <- data.c[which(data.c$AGE.SEX==1),]
head(data.c.1)

data.v.1 <- cbind(data.c.1$Death.Rate,data.c.1$White,data.c.1$Hisp,data.c.1$EDU.4,(data.c.1$Income.15+data.c.1$Income.16),data.c.1$Unemployed)
head(data.v.1)

colnames(data.v.1) <- c("Death.Rate","White","Hisp","EDU.4","HighIncome","Unemployed")
head(data.v.1)
data.v.1 <- as.data.frame(data.v.1)

model.null <- glm(Death.Rate~1,
                  data = data.v.1,
                  family = binomial(link = "logit")
)

model.full = glm(Death.Rate ~ White + Hisp + EDU.4 + HighIncome + Unemployed,
                 data=data.v.1,
                 family = binomial(link="logit")
)

step(model.null,
     scope = list(upper=model.full),
     direction="both",
     test="Chisq",
     data=data.v.1)

# age.sex group 2

data.c.2 <- data.c[which(data.c$AGE.SEX==2),]
head(data.c.2)

data.v.2 <- cbind(data.c.2$Death.Rate,data.c.2$White,data.c.2$Hisp,data.c.2$EDU.4,(data.c.2$Income.15+data.c.2$Income.16),data.c.2$Unemployed)
head(data.v.2)

colnames(data.v.2) <- c("Death.Rate","White","Hisp","EDU.4","HighIncome","Unemployed")
head(data.v.2)
data.v.2 <- as.data.frame(data.v.2)

model.null <- glm(Death.Rate~1,
                  data = data.v.2,
                  family = binomial(link = "logit")
)

model.full = glm(Death.Rate ~ White + Hisp + EDU.4 + HighIncome + Unemployed,
                 data=data.v.2,
                 family = binomial(link="logit")
)

step(model.null,
     scope = list(upper=model.full),
     direction="both",
     test="Chisq",
     data=data.v.2)



##################################################################
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


temp <- cbind(data.train$Death.Rate,fitted.values(model.final))

a <- (fitted.values(model.final)-data.train$Death.Rate)/data.train$Death.Rate
head(fitted.values(model.final))
head(data.train$Death.Rate)

