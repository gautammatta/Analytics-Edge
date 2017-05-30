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
library(randomForest)

CARTb <- rpart(isB ~ . - letter, data=train, method="class")
prp(CARTb)

predB <- predict(CARTb,newdata = test, type = 'class')
nrow(predB)
table(test$isB, predB)



letters$letter = as.factor( letters$letter )
str(letters)
set.seed(2000)

split <- sample.split(letters$letter, SplitRatio = 0.5)
train <- subset(letters, split == TRUE)
test <- subset(letters, split == FALSE)

table(train$letter)

table( test$letter == 'P')

LettersCART <- rpart(letter ~ . - isB, data = train, method = 'class')
prp(LettersCART)

predLettersCART <- predict(LettersCART, newdata = test, type ='class')
table(test$letter, predLettersCART)

set.seed(1000)

randomLetters <- randomForest(letter ~ . - isB, data = train)

predrandomLetters <- predict(randomLetters, newdata = test)
table(test$letter, predrandomLetters)

