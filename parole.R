getwd()
setwd("C://Users/i0318/Downloads/Datasets/Analytics Edge/Unit 3/Data")


########### Load the Parole data ############

parole <- read.csv("parole.csv")
str(parole)

########### Number of violators ############
nrow(parole[parole$violator == 1,])



########### Conveting unordered variable with more than 3 values as factors #########
parole$crime <- as.factor(parole$crime)
parole$state <- as.factor(parole$state)

summary(parole)


########## Run the code to split it into Training and testing set ########## 

set.seed(144)

library(caTools)

split <- sample.split(parole$violator, SplitRatio = 0.7)

train <- subset(parole, split == TRUE)

test <- subset(parole, split == FALSE)


#' If you set a random seed, split, set the seed again to the same value, and then split again, you will get the same split. 
#' However, if you set the seed and then split twice, you will get different splits. If you set the seed to different values, you will get different splits.
#' You can also verify this by running the specified code in R. If you have training sets train1 and train2, 
#' the function sum(train1 != train2) will count the number of values in those two data frames that are different.




########### Building a Logistic Regression Model ###########

ParoleModelLog <- glm(violator ~ . , data = train, family = "binomial")
summary(ParoleModelLog)


## For the example of 50 yr old guy in Problem 4.1 
#' exp( -4.2411574 + 0.3869904* 1 + 0.8867192 * 1 -0.0001756 *50  - 0.1238867*3 + 0.0802954 *12 +   0.6837143*1  )
#' 1/(1+0.1825687)


########## Apply predict function to test set #########

predTest <- predict(ParoleModelLog , newdata = test, type = "response")

#### Confusion Matrix
table(test$violator, predTest >0.5 )


####     FALSE TRUE
####  0   167   12
####  1    11   12

###Sensitivity = 12/(12+11) -> 0.5217391
###Specificity = 167/(167+12) -> 0.9329609
###Accuracy = (167+12)/(167+12+12+11) -> 0.8861386


######### Baseline Model that every parolee is a non violater 

table(test$violator)

######### Evaluating the AUC on testing set 


library(ROCR)

ROCRpred <- prediction(predTest, test$violator)
as.numeric(performance(ROCRpred,"auc")@y.values)



########## Identifying Bias in the Observational Data #########

######### Selection Bias : The dataset contains all individuals released from parole in 2004, either due to 
######### completing their parole term or violating the terms of their parole. However, it does not contain parolees who 
######### neither violated their parole nor completed their term in 2004, causing non-violators to be underrepresented. 
######### This is called "selection bias" or "selecting on the dependent variable," because only a subset of all relevant 
######### parolees were included in our analysis, based on our dependent variable in this analysis (parole violation). 

