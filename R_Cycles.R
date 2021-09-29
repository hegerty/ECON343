#Load quarterly data: USand Mexican Real GDP
#Source: FRED data (BEA and OECD)
#Starts 1993Q1

setwd("F:/")
data<-read.csv("https://github.com/hegerty/ECON343/blob/main/USMXRY.csv",header = TRUE)
head(data)
tail(data)

#make time series
data<-ts(data[,2:3],start=c(1993,1),frequency = 4)
plot(data)

#remove dates, lake natural logs
lndata<-log(data)
colnames(lndata)<-c("USLNY","MXLNY")
head(lndata)
plot(lndata)

#plot US data side by side
par(mfrow=c(1,2))
plot(data[,1],xlab="",ylab="",main="US GDP (levels)")
plot(lndata[,1],xlab="",ylab="",main="US GDP (logs)")
par(mfrow=c(1,1))

#now filter to separate cycle from trend
#Can do Hodrick-Prescott, Baxter-King, Christiano-Fitzgerald
#package 'mFilter'; here, HP with lambda = 1600
#install.packages("mFilter")
library(mFilter)
usf<-hpfilter(lndata[,1])
head(usf)
head(usf$cycle)

#now plot: Just cycle and data w/trend
#Include zero line for cycle; change colors and fonts, etc.
par(mfrow=c(1,2))
plot(usf$cycle,xlab="",ylab="",main="cycle",lwd=3)
abline(h=0,lty=3,col="dark gray",lwd=5)
par(new=FALSE)
plot(usf$trend,xlab="",ylab="",main="trend + cycle",ylim=c(9,10),lwd=3,col="dark grey")
par(new=TRUE)
plot(lndata[,1],xlab="",ylab="",ylim=c(9,10),lwd=2)
par(mfrow=c(1,1))

#Now repeat for Mexico
mxf<-hpfilter(lndata[,2])
head(mxf$cycle)

#Make a cycle dataframe
cycles<-cbind(usf$cycle,mxf$cycle)
plot(cycles)

#Plot together
par(new=FALSE)
plot(cycles[,1],lwd=2,lty=1,col="black",ylim=c(-.1,.1),xlab="",ylab="",main="Business Cycles")
par(new=TRUE)
plot(cycles[,2],lwd=2,lty=4,col="#505050",ylim=c(-.1,.1),xlab="",ylab="")
abline(h=0,lty=2,col="dark grey")
legend("topright",legend=c("US","MX"),lty=c(1,4),lwd=c(2,2),col=c("black","#505050"))

#test for correlations between business cycles
#Will use cross-correlation functions (ccfs)
#first: leads and lags
ll1<-cbind(cycles[,1],lag(cycles[,1],-1))
head(ll1)
#Notice that the variable is "paired" with its past value; lose one observation

#Now add Mexico for this example
ll2<-cbind(ll1,cycles[,2])
head(ll2)

#Any correlation between Mexican GDP and lagged US GDP captured by correlation between Columns 2 and 3
#Mexico NOW associated with US LAST QUARTER
#Mexico "lags" the US
#US "leads" Mexico

#Calculate correlation between MX and lagged US manually
cor(na.omit(ll2[,2:3]))
round(cor(na.omit(ll2[,2:3]))[2,1],3)
#remember this value so you don't get leads and lags backwards!
#now do CCFs using ccf()
#4 leads and lags
#US is x, MX is y

ccf(x=cycles[,1],y=cycles[,2],lag.max = 4)
ccf<-ccf(x=cycles[,1],y=cycles[,2],lag.max = 4)
ccf$acf
#Note it is centered at 5, and the "top" of the list is "X leads Y"
#Also, slight difference in number

#Make a nice x-axis
xaxis<-c(-4:4)
#could also do xaxis<-4*ccf$lag
xaxis
ccftable<-cbind(xaxis,ccf$acf)
colnames(ccftable)<-c("Lag","Correlation")
ccftable

#Plot your own CCF graph
#Include 5% significance vale w/ 100 d.f.
plot(ccftable,pch=20,xlab="lead/lag",ylab="Correlation",ylim=c(0,1),main="Cross-correlations, U.S. and Mexican GDP")
abline(h=0.195,lty=3,col="dark grey")
lines(ccftable)

#Write to a hi-res .jpg
jpeg("USCycles.jpg",height = 3,width=5,units="in",res = 300)
par(mfrow=c(1,2))
par(mar=c(3,3,3,3))
par(cex.axis=.5)
plot(usf$cycle,xlab="",ylab="",main="cycle",lwd=2,ylim=c(-.1,.1))
abline(h=0,lty=2,col="dark gray",lwd=2)
par(new=FALSE)
plot(usf$trend,xlab="",ylab="",main="trend + cycle",ylim=c(9,10),lwd=1.5,col="dark grey",cex=1)
par(new=TRUE)
plot(lndata[,1],xlab="",ylab="",ylim=c(9,10),lwd=2,cex=1,lty=6)
par(mfrow=c(1,1))
dev.off()

jpeg("USMXCycles.jpg",height = 3.5,width=5,units="in",res = 300)
par(new=FALSE)
plot(cycles[,1],lwd=2,lty=1,col="black",ylim=c(-.1,.1),xlab="",ylab="",main="Business Cycles")
par(new=TRUE)
plot(cycles[,2],lwd=2,lty=4,col="#505050",ylim=c(-.1,.1),xlab="",ylab="")
abline(h=0,lty=2,col="dark grey")
legend("topright",legend=c("US","MX"),lty=c(1,4),lwd=c(2,2),col=c("black","#505050"))
dev.off()

jpeg("CCFgraph.jpg",height = 3,width=5,units="in",res = 300)
plot(ccftable,pch=20,xlab="lead/lag",ylab="Correlation",ylim=c(0,1),main="Cross-correlations, U.S. and Mexican GDP")
abline(h=0.195,lty=3,col="dark grey",lwd=2)
lines(ccftable)
dev.off()

remove(ll1,ll2,xaxis)
