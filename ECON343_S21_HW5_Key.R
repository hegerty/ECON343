############################################
#### ECON 343 Spring 2021
#### HW5 KEY
############################################

## Here is the starting data (I am providing it here, but you should have gotten it yourself)
data<-read.csv("https://raw.githubusercontent.com/hegerty/ECON343/main/ECON343_S21_HW5_Data.csv")

head(data,3)
# The peso was fixed at 12.5 per dollar; later (1993), it was revalued (/1000). This "backdates" the revaluation
tail(data)
dim(data)
## I have data starting in 1969, but I choose the start date rather than delete observations
## Bank of Mexico's data begin in 1990; here I swap in the longer series from column 11
#data[,5]<-data[,11]
d1<-which(data[,1]=="1/1/1979")
tsdata<-ts(data[d1:nrow(data),-1],start=c(1979,1),frequency = 12)
head(tsdata)
tail(tsdata)

# Q1 
# There are 9 series but only one has data in 1979: SF329

# Q2
## These are the five. 
###1 is REER (SR28), 
###4 is MXNUSD (SR1501), 
###5 is MXCPI (SR1503)
# Real Exchange Rate Index
# External Price Index
# World, Currency per U.S. Dollar Index
# Pesos per U.S. Dollar Index
# Mexican Consumer Price Index

# Q3: Compare series
## I plot, divide, and calculate correlations
pair<-tsdata[,c(3,8)]
pair<-na.omit(pair)
pair[,2]/pair[,1]
ts.plot(pair,xlab="",main = "SR150 vs E")
# Separate plots is easier since the scale differs 
plot(pair,xlab="",main = "SR150 vs E")
# Or do a scatter!
plot(pair[,1],pair[,2],ylab ="SR150", xlab="E",main = "SR150 vs E",pch=20)
cor(pair)

# Again, dividing gives a constant
pair<-tsdata[,c(9,2)]
pair<-na.omit(pair)
pair[,1]/pair[,2]
ts.plot(pair,xlab="",main="SR1503 vs CPI")
plot(pair,xlab="",main="SR1503 vs CPI")
# Or do a scatter!
plot(pair[,1],pair[,2],xlab ="SR1503", ylab="CPI",main = "SR150 vs E",pch=20)
cor(pair)

## These are the same except for being multiplied by a constant. This makes sure it equals 100 in a given base year

## Q4
# Create real E beginning in 1979
# Should be around 35--it's easy to tell if you did it wrong
colnames(tsdata)
rer<-tsdata[,3]*tsdata[,1]/tsdata[,2]
head(rer)
tail(rer)

# 12-mo log changes beginning 1980
# Plot nom E, real E, dln side by side
dlnrer<-100*diff(log(rer),12)
dlnmxreer<-100*diff(log(tsdata[,4]),12)
dlnmxnusd<-100*diff(log(tsdata[,3]),12)

par(mfrow=c(2,3))
plot(tsdata[,3],xlab="",ylab="E",main = "Peso-Dollar Nominal Exchange Rate")
plot(rer,xlab="",ylab="rer",main = "Peso-Dollar Real Exchange Rate")
plot(tsdata[,4],xlab="",ylab="reer",main = "Mexico's Real Effective Ex. Rate")
plot(dlnmxnusd,xlab="",ylab="dln(E)",main = "Log Changes, Peso-Dollar Nominal Ex. Rate")
plot(dlnrer,xlab="",ylab="dln(rer)",main = "Log Changes, Peso-Dollar Real Ex. Rate")
plot(dlnmxreer,xlab="",ylab="dln(reer)",main = "Log Changes, Mexico's Real Effective Ex. Rate")

## Lots of similar movements
# Big spike in 1990s
# MXN was fixed to USD early on

par(mfrow=c(1,1))



## Q5 
## Compare rer and REER
rer2<-na.omit(cbind(rer,tsdata[,4]))
colnames(rer2)[2]<-"REER"
head(rer2)
ts.plot(rer2,lwd=c(2,2),col=c("black","black"),lty=c(1,4))
legend("topright",legend=colnames(rer2),lwd=c(2,2),col=c("black","black"),lty=c(1,4),bty="n")
legend("top",legend=paste("cor = ",round(cor(rer2)[2],3)),bty="n")
## Here, "up" is the appreciating dollar (depreciating peso)
## This is true for real too (it uses E to calculate)
## REER moves with RER (high US weight in the basket)

x=rer2[,1]
y=rer2[,2]
plot(x,y,pch=20,xlab="RER",ylab="REER")
abline(lm(y~x))
legend(("topleft"),legend=paste("cor = ", round(cor(x,y),3),sep=""),bty="n")
## Here's a little more about the regression line
summary(lm(y~x))
plot(x,y,pch=20,xlab="RER",ylab="REER",xlim=c(0,65),ylim=c(0,150))
abline(lm(y~x))

# Q6
## full rer moving sd 1981
library(zoo)
rersd<-rollapply(dlnrer,12,sd) 
plot(rersd,lwd=2,xlab="",ylab="SD",main="Volatility of peso-dollar real exchange rate")
## Note late 1980s, mid 1990s in particular

##Here are the summary stats
sdtab<-rbind(mean(rersd),sd(rersd),median(rersd),min(rersd),max(rersd),length(rersd))
rownames(sdtab)<-c("mean","SD","med","min","max","N")
round(sdtab,3)

