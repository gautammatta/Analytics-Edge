getwd()

setwd('C:/Users/i0318/Downloads/Datasets/Analytics Edge/Unit 4/Data')


##### Load the data 

letters <- read.csv('letters_ABPR.csv')
str(letters)

letters$isB <- as.factor(letters$letter == 'B')

library('caTools')
set.seed(1000)
split <- sample.split(letters$isB, SplitRatio = 0.5)
train <- subset(letters, split == TRUE)
test <- subset(letters, split == FALSE)

table(test$isB)

library(rpart)
library(rpart.plot)


CARTb <- rpart(isB ~ . - letter, data=train, method="class")
prp(CARTb)

predB <- predict(CARTb,newdata = test, type = 'class')
nrow(predB)
table(test$isB, predB)
