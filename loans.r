

getwd()
setwd("~Analytics Edge/Unit 3/Data")


########### Load the loans data ############

loans <- read.csv("loans.csv")
prop.table(table(loans$not.fully.paid))

####### Summary - missing observations ########

summary(loans)

####### Preparing the data set- if result differs use the imputed csv #####
library(mice)
set.seed(144)
vars.for.imputation <- setdiff(names(loans), "not.fully.paid")
imputed <- complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] <- imputed

######## Use this dataset ###########
loans_imputed<- read.csv("loans_imputed.csv")


######## Verify your results for the two datasets ##########
summary(loans_imputed)
summary(loans[vars.for.imputation])


####### Split the data set into Train and Test data set #########
set.seed(144)
split <- sample.split(loans_imputed$not.fully.paid, SplitRatio  = 0.7)
train <- subset(loans_imputed, split == TRUE)
test <- subset(loans_imputed, split == FALSE)


####### Create the model with all the independent variables ########
LoansModelFit <- glm(not.fully.paid ~. , data = train, family = "binomial")
summary(LoansModelFit)


####### Predict the risk ######
predicted.risk <- predict(LoansModelFit, newdata = test, type = "response")
test$predicted.risk <-predicted.risk

####### Confusion matrix for the model #######
table(test$not.fully.paid, predicted.risk >= 0.5)

#### Accuracy of the baseline model ########
table(test$not.fully.paid)


####### Compute the AUC of the model #######
library(ROCR)

ROCRpred <- prediction(predicted.risk , test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

######## A "Smart Baseline"- prepare the model, it's confusion matrix and AUC ########
SmartBaselineModelFit <- glm(not.fully.paid ~ int.rate, data = train, family = "binomial")
summary(SmartBaselineModelFit)
predict.test.smart <- predict(SmartBaselineModelFit, newdata = test, type = "response")
summary(predict.test.smart)

table(test$not.fully.paid, predict.test.smart >= 0.5)

ROCRbasepred <- prediction(predict.test.smart,test$not.fully.paid)
as.numeric(performance(ROCRbasepred, "auc")@y.values)

########  Computing the Profitability of an Investment ########
test$profit <- exp(test$int.rate * 3) - 1
test$profit[test$not.fully.paid == 1 ] <- -1

summary(test$profit)

########  An Investment Strategy Based on Risk ######
highInterest <- subset(test, test$int.rate >0.15) 
#highInterest <- subset(train, train$int.rate >0.15) 
highInterest$profit <- exp(highInterest$int.rate * 3) - 1
sum(highInterest$profit[highInterest$not.fully.paid == 1 ] )<- -1
summary(highInterest$profit)

######## A Simple Investment Strategy #######
cutoff <- sort(highInterest$predicted.risk, decreasing=FALSE)[100]


####### An Investment Strategy Based on Risk ########
selectedLoans <- subset(highInterest, highInterest$predicted.risk <= cutoff)
summary(selectedLoans$profit)
sum(selectedLoans$not.fully.paid==1) 



