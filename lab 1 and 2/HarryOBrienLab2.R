# PART 1
# Assume variable 'Group' doesn't exist
# 1. Use all x variables as predictors to build a classification decision tree
# 2. Use all y variables as predictors to build a classification decision tree
# 3. Use all x and y as predictors to build a decision tree

# PART 2 
# What is the role of the group column (assume it is gender)
# 1. Split data file based on group
# 2. Apply what you have done in part 1 for each groups 
# 3. Get a confusion matrix for group 1 and group 2
# 4. Combine both confusion matrices to see can you get a more precise result

# For both parts you can leave the dataset as is, don't have to remove missing data
# Must experiment with different control parameters (minbucket etc.) to achieve what you believe to be the optimal tree
# Due in 2 weeks (Wed 17th Nov)
# Can upload Rscript (not marked) 
# Will need to complete a questionnaire that will be posted on blackboard asking for figures based on the results of your code (marked)
# Worth 6%

# Lab 1
setwd("C:\\Users\\harry\\OneDrive\\Documents\\college\\4th year\\semester 1\\data analytics\\lab 1\\wd")
Data <- read.delim("Data for Lab 1.csv",header=TRUE, sep = ",")
Data

#imputationg the missing values uaing predictive mean matching
install.packages("mice")
library("mice")

Data <- mice(Data, m=1, meth = "pmm")

Data <- complete(Data,1)

# Eliminating the first column of the Data- the ID column
Data <- Data[,-1]
# Elimination the group column
Data <- Data[,-2]
# Creating 2 new datasets - one for just x and one for just y
Datax <- Data[,-c(9:16)]
Datax
Datay <- Data[,-c(2:8)]
Datay

### Model 1
# Examining structure of data
str(Datax)
set.seed(123)

# Tells R we want to work with this dataset
#attach(Datax)

# Downloading and Installing R package
install.packages("rpart")
library("rpart")
install.packages("partykit")
library("partykit")



# Picking out the response column
cols <- c(1)

# Telling R that response factors
Datax[cols] <- lapply(Datax[cols], factor)
# Examining structure of data
str(Datax)
# Creating our decision tree model - use maxdepth = 2 as the extra variables being used are not worth the small increase in accuracy I think
DT_Modelx <- rpart(Response~., data = Datax, control = rpart.control(minsplit = 20, minbucket = 10 , maxdepth = 2))
# Printing out the model
print(DT_Modelx)

# Plotting the model- this will print out the decision tree to the console
plot(as.party(DT_Modelx))

# Percent correct 
pred <- predict(DT_Modelx, Datax, type="class") 
CMx <- table(Datax$Response, pred)
CMx
227/296 = 0.7669

### Model 2
# Examining structure of data
str(Datay)
set.seed(123)

# Tells R we want to work with this dataset
#attach(Datay)

# Picking out the response column
cols <- c(1)

# Telling R that response factors
Datay[cols] <- lapply(Datay[cols], factor)
# Examining structure of data
str(Datay)
# Creating our decision tree model - use maxdepth = 2 as the extra variables being used are not worth the small increase in accuracy I think
DT_Modely <- rpart(Response~., data = Datay, control = rpart.control(minsplit = 20, minbucket = 10 , maxdepth = 2))
# Printing out the model
print(DT_Modely)

# Plotting the model- this will print out the decision tree to the console
plot(as.party(DT_Modely))

# Percent correct 
pred <- predict(DT_Modely, Datay, type="class") 
CMy <- table(Datay$Response, pred)
CMy
214/296


### Model 3
# Examining structure of data
str(Data)
set.seed(123)

# Tells R we want to work with this dataset
#attach(Data)

# Picking out the response column
cols <- c(1)

# Telling R that response factors
Data[cols] <- lapply(Data[cols], factor)
# Examining structure of data
str(Data)
# Creating our decision tree model - use maxdepth = 2 as the extra variables being used are not worth the small increase in accuracy I think
DT_Model <- rpart(Response~., data = Data, control = rpart.control(minsplit = 20, minbucket = 10 , maxdepth = 2))
# Printing out the model
print(DT_Model)

# Plotting the model- this will print out the decision tree to the console
plot(as.party(DT_Model))

# Percent correct 
pred <- predict(DT_Model, Data, type="class") 
CM <- table(Data$Response, pred)
CM
229/296

### Model 4
Data <- read.delim("Data for Lab 1.csv",header=TRUE, sep = ",")

# impute data
Data <- mice(Data, m=1, meth = "pmm")
Data <- complete(Data,1)

# Eliminating the first column of the Data- the ID column
Data <- Data[,-1]
Group1 <- Data[Data$Group == 0, ]
Group2 <- Data[Data$Group == 1, ]

# Elimination the group column
Group1 <- Group1[,-2]

# Creating 2 new datasets - one for just x and one for just y
Group1x <- Group1[,-c(9:16)]
Group1x
Group1y <- Group1[,-c(2:8)]
Group1y

# Elimination the group column
Group2 <- Group2[,-2]
# Creating 2 new datasets - one for just x and one for just y
Group2x <- Group2[,-c(9:16)]
Group2x
Group2y <- Group2[,-c(2:8)]
Group2y

# Examining structure of data
str(Group1x)
set.seed(123)

# Picking out the response column
cols <- c(1)

# Telling R that response factors
Group1x[cols] <- lapply(Group1x[cols], factor)
# Examining structure of data
str(Group1x)
# Creating our decision tree model - use maxdepth = 2 as the extra variables being used are not worth the small increase in accuracy I think
DT_Model4 <- rpart(Response~., data = Group1x, control = rpart.control(minsplit = 20, minbucket = 10 , maxdepth = 2))
# Printing out the model
print(DT_Model4)

# Plotting the model- this will print out the decision tree to the console
plot(as.party(DT_Model4))

# Percent correct 
pred <- predict(DT_Model4, Group1x, type="class") 
CM4 <- table(Group1x$Response, pred)
CM4
70/96

### Model 5
# Examining structure of data
str(Group2x)
set.seed(123)

# Picking out the response column
cols <- c(1)

# Telling R that response factors
Group2x[cols] <- lapply(Group2x[cols], factor)
# Examining structure of data
str(Group2x)
# Creating our decision tree model - use maxdepth = 2 as the extra variables being used are not worth the small increase in accuracy I think
DT_Model5 <- rpart(Response~., data = Group2x, control = rpart.control(minsplit = 20, minbucket = 10 , maxdepth = 2))
# Printing out the model
print(DT_Model5)

# Plotting the model- this will print out the decision tree to the console
plot(as.party(DT_Model5))

# Percent correct 
pred <- predict(DT_Model5, Group2x, type="class") 
CM5 <- table(Group2x$Response, pred)
CM5
161/200

### Model 6
# Examining structure of data
str(Group1y)
set.seed(123)

# Picking out the response column
cols <- c(1)

# Telling R that response factors
Group1y[cols] <- lapply(Group1y[cols], factor)
# Examining structure of data
str(Group1y)
# Creating our decision tree model
DT_Model6 <- rpart(Response~., data = Group1y, control = rpart.control(minsplit = 20, minbucket = 10 , maxdepth = 2))
# Printing out the model
print(DT_Model6)

# Plotting the model- this will print out the decision tree to the console
plot(as.party(DT_Model6))

# Percent correct 
pred <- predict(DT_Model6, Group1y, type="class") 
CM6 <- table(Group1y$Response, pred)
CM6
70/96

### Model 7
# Examining structure of data
str(Group2y)
set.seed(123)

# Picking out the response column
cols <- c(1)

# Telling R that response factors
Group2y[cols] <- lapply(Group2y[cols], factor)
# Examining structure of data
str(Group2y)
# Creating our decision tree model
DT_Model7 <- rpart(Response~., data = Group2y, control = rpart.control(minsplit = 20, minbucket = 10 , maxdepth = 2))
# Printing out the model
print(DT_Model7)

# Plotting the model- this will print out the decision tree to the console
plot(as.party(DT_Model7))

# Percent correct 
pred <- predict(DT_Model7, Group2y, type="class") 
CM7 <- table(Group2y$Response, pred)
CM7
154/200

### Model 8
# Examining structure of data
str(Group1)
set.seed(123)

# Picking out the response column
cols <- c(1)

# Telling R that response factors
Group1[cols] <- lapply(Group1[cols], factor)
# Examining structure of data
str(Group1)
# Creating our decision tree model - use maxdepth = 2 as the extra variables being used are not worth the small increase in accuracy I think
DT_Model8 <- rpart(Response~., data = Group1, control = rpart.control(minsplit = 20, minbucket = 10 , maxdepth = 2))
# Printing out the model
print(DT_Model8)

# Plotting the model- this will print out the decision tree to the console
plot(as.party(DT_Model8))

# Percent correct 
pred <- predict(DT_Model8, Group1, type="class") 
CM8 <- table(Group1$Response, pred)
CM8
70/96

### Model 9
# Examining structure of data
str(Group2)
set.seed(123)

# Picking out the response column
cols <- c(1)

# Telling R that response factors
Group2[cols] <- lapply(Group2[cols], factor)
# Examining structure of data
str(Group2)
# Creating our decision tree model
DT_Model9 <- rpart(Response~., data = Group2, control = rpart.control(minsplit = 20, minbucket = 10 , maxdepth = 2))
# Printing out the model
print(DT_Model9)

# Plotting the model- this will print out the decision tree to the console
plot(as.party(DT_Model9))

# Percent correct 
pred <- predict(DT_Model9, Group2, type="class") 
CM9 <- table(Group2$Response, pred)
CM9
161/200

# Overall correct % = a%A%+b%B% for X predictors(same as X and Y)
(((96/296)*(70/96)) + ((200/296)*(161/200)))
# Model 1
241/296

# Overall correct % = a%A%+b%B% for Y predictors
(((96/296)*(68/96)) + ((200/296)*(154/200)))
# Model 2
229/296

# Overall correct % = a%A%+b%B% for Y predictors (same as X)
(((96/296)*(70/96)) + ((200/296)*(161/200))) 
# Model 3 (highest of 1,2,3)
242/296




