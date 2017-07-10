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

predCensusCART <- predict(censusCART, newdata = test, type = 'class')

table(test$over50k, predCensusCART)


ROC <- predict(censusCART, newdata = test)[,2]

ROCRCARTpred <- prediction(ROC, test$over50k)
predCART <- performance(ROCRCARTpred, measure = "tpr", x.measure = "fpr")
plot(predCART)


AUC <- as.numeric(performance(ROCRCARTpred,"auc")@y.values)



########  A Random Forest Model

set.seed(1)

trainSmall <- train[sample(nrow(train), 2000), ]

library(randomForest)

randomForestModel <- randomForest(over50k~., data = trainSmall )


RFPred <- predict(randomForestModel, newdata = test)

table(test$over50k, RFPred)

vu <- varUsed(randomForestModel, count=TRUE)
vusorted <- sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(randomForestModel$forest$xlevels[vusorted$ix]))


### impurity - most imp variable in terms of mean reduction in impurity! 

varImpPlot(randomForestModel)

######### Selecting cp by Cross-Validation
library(caret)

library(e1071)
set.seed(2)


numFolds <- trainControl( method = "cv", number = 10 )
cartGrid <- expand.grid(.cp = seq(0.002,0.1,0.002))

train(over50k ~. , data = train, method = 'rpart' , trControl= numFolds, tuneGrid = cartGrid)



censusCARTcp <- rpart(over50k ~. , data = train, cp =0.002, method = 'class')

censusCARTcpPred <- predict(censusCARTcp, newdata = test, type = 'class')

table(test$over50k, censusCARTcpPred)
prp(censusCARTcp)
