####################
#ECON 343 NEIU
#Macroeconomic Data Transformations: Examples
####################
#
##Start with logs and exponets
#Integers 1-15, plot and compare
x<-c(1:15)
expx<-exp(x)
lnx<-log(x)
plot(expx,type="l",xlab="",ylab="",main="exp(x) vs. x (dashed)")
lines(x,lty=2)
#lnx is flatter

plot(x,type="l",xlab="",ylab="",main="x vs. ln(x) (dashed)")
lines(lnx,lty=2)
#lnx is flatter

dev.off()
#Now pull data, transform and plot
data<-read.csv("https://sites.google.com/site/swhegerty/macroeconomic-data-analysis/MacroDataExample.csv",header=TRUE)
head(data)

#First use trade data
data1<-ts(na.omit(data[,2:5]),start=c(1947,1),frequency = 4)
head(data1)
tail(data1)
realx<-100*data1[,1]/data1[,4]
realm<-100*data1[,2]/data1[,4]
ts.plot(realx,realm,lty=c(1,2),xlab="",main="Real Exports and Imports")
legend("topleft",legend=c("realx","realm"),lty=c(1,2))

xshare<-100*data1[,1]/data1[,3]
mshare<-100*data1[,2]/data1[,3]
ts.plot(xshare,mshare,lty=c(1,2),xlab="",main="Export and Import Shares")
legend("topleft",legend=c("x","m"),lty=c(1,2))

xmratio<-data1[,1]/data1[,2]
ts.plot(xmratio,xlab="",ylab="",main="Export/Import Ratio")
abline(h=1,lty=3)

tbshare<-xshare-mshare
open<-xshare+mshare
ts.plot(open,tbshare,lty=c(1,2),xlab="",main="Trade Balance and Openness Measure")
abline(h=0,lty=5,col="dark grey")
legend("topleft",legend=c("tb","open"),lty=c(1,2))

dev.off()

#Now do price data
data2<-ts(data[,c(7,8)],start=c(1993,1),frequency = 12)
head(data2)
tail(data2)
data2[61,]
#Quick transformation to get both to equal 100 (at obs. 61)
t1<-data2[which(data2[,1]==100),2]
data2[,2]<-100*data2[,2]/t1
data2[61,]
ts.plot(data2,lty=c(1,2),xlab="",main="Price Indices")
legend("topleft",legend=c("Education","All Items"),lty=c(1,2))

#Price Ratio
pratio<-data2[,1]/data2[,2]
ts.plot(pratio,main="Price Ratio: Education/All Items",xlab="",ylab="")
abline(h=1,lty=3)

#inflation
cpi<-data2[,2]
inf1<-100*diff(cpi)/lag(cpi)
inf2<-100*diff(log(cpi))
inf<-cbind(inf1,inf2)
cor(inf)

#1- vs. 12-month rates
inf12<-100*(cpi-lag(cpi,-12))/lag(cpi,-12)
head(inf12)            
ts.plot(12*inf1,inf12,lty=c(2,1),col=c("black","dark grey"),lwd=c(1,6),xlab="",main="Monthly vs. Annual Inflation Rates")
legend("topleft",legend=c("1month","12month"),lty=c(1,1),col=c("black","dark grey"),lwd=c(1,6))

