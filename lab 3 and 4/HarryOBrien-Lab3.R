#=========================================================

# THIS IS JUST COPY PASTED CODE FROM LAB 3
# SCROLL DOWN TO SEE CODE WRITTEN FOR LAB 4

#=========================================================
# lab 3, harry o brien, 18323075
install.packages("glmnet")

#multiple linear regression model
#import dataset
setwd("C:\\Users\\harry\\OneDrive\\Documents\\college\\4th year\\semester 1\\data analytics\\lab 3\\wd")
Data <- read.delim("Income Data.csv",header=TRUE, sep = ",")
#build model
mlrm <- lm(Income ~ white+educ_r+age60m+ssi+welfare+immig+r_sex+charity+assets, data = Data)
#get model info
mlrm_summ <- summary(mlrm)
mlrm_mse <- mean(mlrm_summ$residuals^2)
mlrm_mse
mlrm_summ$r.squared

#LASSO model
#reference: https://www.statology.org/lasso-regression-in-r/
#remove NA rows
#must be done, otherwise the method wont work
DataClean <- na.omit(Data)
#define response variable
y <- DataClean$Income
#define matrix of predictor variables
x <- data.matrix(DataClean[, c('white', 'educ_r', 'age60m', 'ssi', 'welfare', 'immig', 'r_sex', 'charity', 'assets')])

library(glmnet)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_model) 

#find summary and coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
LASSO_summ <- summary(best_model)
LASSO_summ


#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = x)

#find MSE of LASSO model
LASSO_mse <- mean((y_predicted - DataClean$Income)^2)
LASSO_mse

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

#===========================================================

# LAB 4

#===========================================================

#lab 4, harry o brien
install.packages("mice")
library(mice)
library(dplyr)
install.packages("ivreg")
library(ivreg)

#impute values for all NA for all variables except income
imp_data_except_income <- mice(Data[, c(1,2,3,4,5,7,8,9,10)])
imp_data_except_income_com <- complete(imp_data_except_income)
#running this imputation removed the income variable, so we will add it again
imp_data_except_income_com$Income <- Data$Income

#2 stage regression
#first we make our instrumental variable, IV, which is =1 when Income >0, and
#is =0 when Income =0
#source; https://www.marsja.se/r-add-column-to-dataframe-based-on-other-columns-conditions-dplyr/
data_with_IV <- imp_data_except_income_com %>%
  mutate(IV = if_else(
    Income > 0, 1, 0
  ))

#define the variables as integers or factors
data_with_IV$white <- as.factor(data_with_IV$white)
data_with_IV$educ_r <- as.factor(data_with_IV$educ_r)
data_with_IV$age60m <- as.factor(data_with_IV$age60m)
data_with_IV$rsex <- as.factor(data_with_IV$rsex)
data_with_IV$IV <- as.factor(data_with_IV$IV)

data_with_IV$Income <- as.integer(data_with_IV$Income)
data_with_IV$assets <- as.integer(data_with_IV$assets)
data_with_IV$welfare <- as.integer(data_with_IV$welfare)
data_with_IV$ssi <- as.integer(data_with_IV$ssi)
data_with_IV$charity <- as.integer(data_with_IV$charity)


#run logistic regression for IV
#we must remove the income variable and append it afterwards, otherwise it will
#impute values for income which we do not want to do yet
dataLR <- mice(data_with_IV[,c(1:9,11)], method = "logreg")
dataLR_com <- complete(dataLR)

#append income column
dataLR_com$Income <- Data$Income

#where IV = 0, we assume Income = 0
dataImpIncome0 <- dataLR_com %>%
  mutate(Income = ifelse(
    IV == 0, 0, Income
  ))

#answer for question 1 - how many values were imputed for Income = 0
q1 <- (sum(is.na(dataLR_com$Income))) - (sum(is.na(dataImpIncome0$Income)))
q1 #44

#now finally we will run a multiple regression model to impute the remaining NA 
#Income values. we will not use IV as a predictor in the function, which will
#remove it as a column. this is fine, we no longer need to keep it as a column
final_data <- mice(dataImpIncome0[, c(1:9,11)])
final_data_com <- complete(final_data)

#create a dataset composed entirely of imputed Income values
nonZeroImpIncome <- (final_data[["imp"]][["Income"]])
#we will also remove all values = 0
nonZeroImpIncome <- nonZeroImpIncome[!(nonZeroImpIncome$`1` == 0),]
#we only want the first column, as these were the imputed values used.
nonZeroImpIncome <- nonZeroImpIncome$`1`
q2 <- mean(nonZeroImpIncome)
q2 #103093.2
