getwd()

############## Load the data 

gerber <- read.csv("gerber.csv")
str(gerber)
prop.table(table(gerber$voting))

summary(gerber[gerber$voting==1,])

tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$neighbors, mean)



##### build a Logistic Regression model with 4 treatment groups 

gerberLog <- glm(voting~hawthorne + civicduty + neighbors+ self , data = gerber, family = 'binomial')
summary(gerberLog)

##### Accuracy of the model

predLog <- predict(gerberLog, type ='response')

table(gerber$voting, predLog >= 0.3)


table(gerber$voting, predLog >= 0.5)

ROCRpred <- prediction(predLog,gerber$voting)
AUC <- as.numeric(performance(ROCRpred, "auc")@y.values)



######## Build a Regression Tree for Treatment group
library(rpart)
library(rpart.plot)

cartModel <- rpart(voting~hawthorne + civicduty+ neighbors+ self, data = gerber, cp =0.0)
prp(cartModel)



cartModel1 <- rpart(voting~hawthorne + civicduty+ neighbors+ self + sex, data = gerber, cp =0.0)

prp(cartModel1)



######## Build a Regression Tree for Control group

cartModelCtrl <- rpart(voting ~ control, data = gerber)
prp(cartModelCtrl)


cartModelCtrl1 <- rpart(voting ~ control, data = gerber, cp = 0.0)
prp(cartModelCtrl1, digits = 6)


#### Logistic Regression for Control group with sex

LogControlModel <- glm(voting~   sex  + control, data = gerber , family = 'binomial')
summary(LogControlModel)

cartModelCtrl2 <- rpart(voting ~ control + sex, data = gerber, cp = 0.0)
prp(cartModelCtrl2 , digits = 6)



###### Interaction Terms

Possibilities <- data.frame(sex=c(0,0,1,1), control = c(0,1,0,1))
predict(LogControlModel, newdata = Possibilities, type = 'response')


LogModel2 <- glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")

summary(LogModel2)

predict(LogModel2, newdata=Possibilities, type="response")