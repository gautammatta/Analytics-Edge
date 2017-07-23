setwd("C:/Users/G-Machine/Downloads/Analytics Edge/Unit 6")


## Cluster then predict 

stocks <- read.csv("StocksCluster.csv")

str(stocks)


prop.table(table(stocks$PositiveDec))



sort(cor(stocks))

summary(stocks)

library(caTools)

set.seed(144)

spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)

stocksTrain = subset(stocks, spl == TRUE)

stocksTest = subset(stocks, spl == FALSE)


StocksModel <- glm(PositiveDec~., data = stocksTrain, family = 'binomial')

stocksPredict <- predict(StocksModel, newdata = stocksTest, type = 'response')

head(stocksPredict)

table(stocksTest$PositiveDec, stocksPredict>0.5)



#### Clustering Stocks 


limitedTrain = stocksTrain

limitedTrain$PositiveDec = NULL

limitedTest = stocksTest

limitedTest$PositiveDec = NULL

library(caret)

preproc = preProcess(limitedTrain)

normTrain = predict(preproc, limitedTrain)

normTest = predict(preproc, limitedTest)


summary(normTrain)
summary(normTest)


set.seed(144)

km <- kmeans(normTrain, center=3 )

km$size


install.packages('flexclust')
library(flexclust)

km.kcca = as.kcca(km, normTrain)

clusterTrain = predict(km.kcca)

clusterTest = predict(km.kcca, newdata=normTest)

table(clusterTest)

stocksTrain1 <- subset(stocksTrain, clusterTrain == 1)
stocksTrain2 <- subset(stocksTrain, clusterTrain == 2)
stocksTrain3 <- subset(stocksTrain, clusterTrain == 3)


stocksTest1 <- subset(stocksTest, clusterTest == 1)
stocksTest2 <- subset(stocksTest, clusterTest == 2)
stocksTest3 <- subset(stocksTest, clusterTest == 3)


summary(stocksTrain1$PositiveDec)
summary(stocksTrain2$PositiveDec)
summary(stocksTrain3$PositiveDec)



StocksModel1 <- glm(PositiveDec~., data = stocksTrain1, family = 'binomial')
StocksModel2 <- glm(PositiveDec~., data = stocksTrain2, family = 'binomial')
StocksModel3 <- glm(PositiveDec~., data = stocksTrain3, family = 'binomial')

stocksPredict1 <- predict(StocksModel1, newdata = stocksTest1, type = 'response')
stocksPredict2 <- predict(StocksModel2, newdata = stocksTest2, type = 'response')
stocksPredict3 <- predict(StocksModel3, newdata = stocksTest3, type = 'response')

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)



table(stocksTest1$PositiveDec, stocksPredict1>0.5)

table(stocksTest2$PositiveDec, stocksPredict2>0.5)

table(stocksTest3$PositiveDec, stocksPredict3>0.5)



AllPredictions = c(stocksPredict1, stocksPredict2, stocksPredict3)

AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllOutcomes,AllPredictions > 0.5)
