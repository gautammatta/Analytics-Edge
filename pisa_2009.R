getwd()
setwd("C://Users/i0318/Downloads/Datasets/Analytics Edge/Unit 2/Climate change")
require(dplyr)
pisa_train <- read.csv("pisa2009train.csv")
pisa_test <- read.csv("pisa2009test.csv")

glimpse(pisa_train)
## What is the average reading test score of males?
#mean(pisa_train$readingScore,pisa_train$male == 1)


tapply(pisa_train$readingScore,pisa_train$male, FUN =mean)

# Which variables are missing data in at least one observation in the training set? 


colnames(pisa_train)[colSums(is.na(pisa_train))>0 ]

summary(pisa_train)

##Removing missing values

pisa_train <- na.omit(pisa_train)
pisa_test <- na.omit(pisa_test)

## How many values are left in training and test set? 

glimpse(pisa_train)
glimpse(pisa_test)

# Which is an unordered factor with at least 3 levels ? 

unique(pisa_test$grade)

# Set the reference level of raceeth to be White 

pisa_train$raceeth = relevel(pisa_train$raceeth, "White")

pisa_test$raceeth = relevel(pisa_test$raceeth, "White")

# create the model 
lmScore <- lm(readingScore ~., data = pisa_train)
summary(lmScore)


SSE <- sum(lmScore$residuals^2)
RMSE <- sqrt(SSE/nrow(pisa_train))


predict <- predict(lmScore)

tapply(predict,pisa_train$grade,mean)


# Predicting on the unseen data 

predTest <- predict(lmScore, newdata = pisa_test)
summary(predTest)

SSE <- sum((predTest- pisa_test$readingScore)^2)
SST <- sum((mean(pisa_train$readingScore) - pisa_test$readingScore)^2)
RMSE <- sqrt(mean((predTest-pisa_test$readingScore)^2))

R2 <- 1- SSE/SST