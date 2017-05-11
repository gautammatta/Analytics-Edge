getwd()
setwd("C://Users/i0318/Downloads/Datasets/Analytics Edge/Unit 3/Data")


########### Load the Songs data ############

songs <- read.csv("songs.csv")
str(songs)


####### Understanding the data ##########
sum(songs$artistname== "Michael Jackson")

songs[songs$artistname == "Michael Jackson" & songs$Top10 == 1, ]

sort(unique(songs$timesignature))

songs[songs$tempo == max(songs$tempo), ]


####### Creating our Prediction model ########

SongsTrain <- subset(songs,songs$year<=2009)

SongsTest<- subset(songs,songs$year > 2009)

str(SongsTrain)  ## 7201 
str(SongsTest)   ## 373

SongsTrain <- SongsTrain[, !(names(SongsTrain) %in% c("year","songtitle", "artistname", "songID", "artistID"))]
SongsTest <- SongsTest[, !(names(SongsTest) %in% c("year","songtitle", "artistname", "songID", "artistID"))]

Model1 <- glm(Top10 ~. , data = SongsTrain, family = binomial)
summary(Model1)


cor(SongsTrain$loudness, SongsTrain$energy)

######### Create Model 2, which is Model 1 without the independent variable "loudness". ##########
Model2 <- glm(Top10 ~. -loudness, data = SongsTrain, family = binomial)
summary(Model2)


######### Create Model 3, which is Model 1 without the independent variable "energy". ##########
Model3 <- glm(Top10 ~. -energy, data = SongsTrain, family = binomial)
summary(Model3)


predTest <- predict(Model3,  newdata = SongsTest ,type ="response")

table(SongsTest$Top10, predTest >= 0.45)

###### use a baseline model #########
table(SongsTest$Top10) # ~84%


nrow(SongsTest[SongsTest$Top10== 0 & predTest >0.45, ])[]




########## Find out the sensitivity and specificity of the model ###########


table(SongsTest$Top10, predTest >= 0.45)
## Sensitivity = 0.32220339
## Specificity = 0.9840764

