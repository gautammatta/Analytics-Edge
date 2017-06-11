setwd("C:/Users/G-Machine/Downloads/Analytics Edge/Unit 1")

##########

mvt <- read.csv('mvtWeek1.csv')
str(mvt)
summary(mvt)

DateConvert <- as.Date(strptime(mvt$Date,"%m/%d/%y %H:%M"))

str(DateConvert)
median(DateConvert)

mvt$Month <- months(DateConvert)
mvt$Weekday <- weekdays(DateConvert)

mvt$Date <- DateConvert
which.min(table(mvt$Month))
which.max(table(mvt$Weekday))

which.max(table(mvt$Month,mvt$Arrest)[,2])

hist(mvt$Date, breaks=100)

boxplot(mvt$Date~mvt$Arrest)
mvt$Year <- years(mvt$Date)
table(format(mvt$Date,"%Y"),mvt$Arrest)[,2]/(table(format(mvt$Date,"%Y"),mvt$Arrest)[,1]

table(format(mvt$Date,"%Y"),mvt$Arrest)
table(mvt$Year,mvt$Arrest)

str(mvt)

str(as.Numeric(format(mvt$Date,"%Y")))
?sort
head(sort(table(mvt$LocationDescription),decreasing = TRUE))
x <- names(head(sort(table(mvt$LocationDescription),decreasing = TRUE)))
Top5 <- subset(mvt, mvt$LocationDescription %in% names(head(sort(table(mvt$LocationDescription),decreasing = TRUE)))& 
                 mvt$LocationDescription != 'OTHER')
Top5 <- subset(mvt, mvt$LocationDescription %in% x & 
                 mvt$LocationDescription != 'OTHER')

str(Top5)
unique(Top5$LocationDescription)


Top5$LocationDescription <- factor(Top5$LocationDescription)
str(Top5)

summary(Top5)
prop.table(table(Top5$LocationDescription, Top5$Arrest))

which.min(names(table(Top5$LocationDescription, Top5$Weekday)))