getwd()
setwd("C://Users/i0318/Downloads/Datasets/Analytics Edge/Unit 3/Data")


########### Load the loans data ############

loans <- read.csv("loans.csv")
prop.table(loans$not.fully.paid)

prop.table(table(loans$not.fully.paid))

####### Summary - missing observations ########

summary(loans)

library(mice)
set.seed(144)
vars.for.imputation <- setdiff(names(loans), "not.fully.paid")
imputed <- complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] <- imputed

loans_imputed<- read.csv("loans_imputed.csv")

summary(loans_imputed)
summary(loans[vars.for.imputation])

set.seed(144)

split <- sample.split(loans_imputed$not.fully.paid, SplitRatio  = 0.7)
train <- subset(loans_imputed, split == TRUE)
test <- subset(loans_imputed, split == FALSE)

LoansModelFit <- glm(not.fully.paid ~. , data = train, family = "binomial")
summary(LoansModelFit)

predicted.risk <- predict(LoansModelFit, newdata = test, type = "response")
test$predicted.risk <-predicted.risk

table(test$not.fully.paid, predicted.risk >= 0.5)

table(test$not.fully.paid)

library(ROCR)

ROCRpred <- prediction(predicted.risk , test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)


SmartBaselineModelFit <- glm(not.fully.paid ~ int.rate, data = train, family = "binomial")
summary(SmartBaselineModelFit)

predict.test.smart <- predict(SmartBaselineModelFit, newdata = test, type = "response")
summary(predict.test.smart)

table(test$not.fully.paid, predict.test.smart >= 0.5)

ROCRbasepred <- prediction(predict.test.smart,test$not.fully.paid)
as.numeric(performance(ROCRbasepred, "auc")@y.values)