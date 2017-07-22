setwd("C:/Users/G-Machine/Downloads/Analytics Edge/Unit 6")

airlines <- read.csv("AirlinesCluster.csv")

summary(airlines)

library(caret)

preproc <- preProcess(airlines)

airlinesNorm <- predict(preproc, airlines)

summary(airlinesNorm)


distance <- dist(airlinesNorm, method = 'euclidean')

airlinesHClust <- hclust(distance , method="ward.D")

plot(airlinesHClust)

airlinesHClustTree <- cutree(airlinesHClust, k = 5)

table(airlinesHClustTree)

tapply(airlines$Balance,airlinesHClustTree, mean )

tapply(airlines$QualMiles,airlinesHClustTree, mean )
tapply(airlines$BonusMiles,airlinesHClustTree, mean )
tapply(airlines$BonusTrans,airlinesHClustTree, mean )
tapply(airlines$FlightMiles,airlinesHClustTree, mean )
tapply(airlines$FlightTrans,airlinesHClustTree, mean )
tapply(airlines$DaysSinceEnroll,airlinesHClustTree, mean )




## Kmeans clustering 
set.seed(88)
KMC <- kmeans(airlinesNorm, center =5, iter.max=1000)
KMC$size

tapply(airlines)

