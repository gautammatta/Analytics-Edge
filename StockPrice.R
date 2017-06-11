setwd("C:/Users/G-Machine/Downloads/Analytics Edge/Unit 1")



### Load the data 

ls()

IBM <- read.csv('IBMStock.csv')
GE <- read.csv('GEStock.csv')
CocaCola <- read.csv('CocaColaStock.csv')
ProcterGamble<- read.csv('ProcterGambleStock.csv')
Boeing <- read.csv('BoeingStock.csv')


str(IBM)


######## Convert 'Date' variable into Date in proper format  



IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")



######### Check the summaries/sd of different stocks


summary(IBM)
summary(GE)
summary(CocaCola)
summary(Boeing)
sd(ProcterGamble$StockPrice)



####### Visualizing Stock Dynamics

plot(CocaCola$Date,CocaCola$StockPrice, type ='l',col = 'red')
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = 'blue')
abline(v=as.Date(c("2000-03-01")), lwd=2)

colors()
##### - Visualizing Stock Dynamics 1995-2005
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(IBM$Date[301:432], IBM$StockPrice[301:432], type="l", col="blue", ylim=c(0,210))
lines(GE$Date[301:432], GE$StockPrice[301:432], type="l", col="yellow", ylim=c(0,210))
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], type="l", col="cyan3", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], type="l", col="burlywood3", ylim=c(0,210))

abline(v=as.Date(c("1997-09-01")), lwd=1)
abline(v=as.Date(c("1997-11-01")), lwd=1)


######  - Monthly Trends

tapply(IBM$StockPrice, months(IBM$Date),mean) > mean(IBM$StockPrice)
which.max(tapply(GE$StockPrice, months(GE$Date),mean)) 
