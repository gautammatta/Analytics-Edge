getwd()

setwd('C:/Users/i0318/Downloads/Datasets/Analytics Edge/Unit 4/Data')


#### Load data for census

census <- read.csv('census.csv')
str(census)

set.seed(2000)
library('caTools')
library('ROCR')
split <- sample.split(census$over50k, SplitRatio= 0.6)
train <- subset(census, split == TRUE)
test <- subset(census, split == FALSE)


logModel <- glm(over50k~ . , data = train, family= 'binomial')
summary(logModel)

predLogModel <- predict(logModel, newdata = test, type ='response')

table(test$over50k, predLogModel >0.5)

table(test$over50k)
 
ROCRpred <- prediction(predLogModel, test$over50k)
AUC <- as.numeric(performance(ROCRpred, "auc")@y.values)
prf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")

plot(prf)
###### CART model


library(rpart)
library(rpart.plot)

censusCART <- rpart(train$over50k~., data = train, method = 'class')

prp(censusCART)

predCensusCART <- predict(censusCART, newdata = test)

table(test$over50k, predCensusCART )

#ROC <- predict(censusCART, newdata = test)[,2]

ROCRCARTpred <- prediction( predict(censusCART, newdata = test)[,2], test$over50k)
predCART <- performance(ROCRCARTpred, measure = "tpr", x.measure = "fpr")
plot(predCART)

 
AUC <- as.numeric(performance(ROCRCARTpred,"auc")@y.values)
