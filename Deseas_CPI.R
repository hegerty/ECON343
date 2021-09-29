##R code for Deseasonalizing a series
#Alternative inflation method (related to PPP script)
#Deseasonalizing, then using monthly changes

#Move the data file to your drive and CHANGE the letter F if you need to
setwd("F:/") #Now I made it a direct upload
data<-read.csv("https://sites.google.com/site/swhegerty/macroeconomic-data-analysis/PPPData.csv")
#Check your data first!
head(data)

#Change name of just column 3
colnames(data)[3]<-"EUCPI"
head(data)

#Remove the date column, leave only EU CPI
data<-data[,3]
head(data)

#make a time series 
tsdata<-ts(data,start=c(1999,1),frequency = 12)
plot(tsdata)

#Now decompose using loess smothing
#note the final product has more than just new time series
tsdata1<-stl(tsdata,s.window = 7)
head(tsdata1$time.series)
plot(tsdata1)

#append deseasonalized series to new
eucpids<-tsdata-tsdata1$time.series[,1]
plot(eucpids)
tsdata<-cbind(tsdata,eucpids)
colnames(tsdata)[1]<-"eucpi"
head(tsdata)

#plot, separately and together
plot(tsdata)
plot(tsdata[,1],xlab="",ylab="",lwd=4,col="dark grey",main="EU CPI")
par(new=TRUE)
plot(tsdata[,2],xlab="",ylab="",lwd=2,col="black",lty=2)
legend("bottomright",lty = c(1,2),lwd=c(4,2),col=c("dark grey","black"),legend = c("Not adjusted","Seasonally adjusted"))

#Now make a month-on-month inflation rate
euinf1<-1200*((tsdata[,2]/lag(tsdata[,2],-1))-1)
#also make a year-on-year inflation rate
euinf12<-100*((tsdata[,2]/lag(tsdata[,2],-12))-1)
#compare the results of the adjusted and unadjusted series
euinf12b<-100*((tsdata[,1]/lag(tsdata[,1],-12))-1)

#Check data and column means
inf<-cbind(euinf1,euinf12,euinf12b)
head(inf)
tail(inf)
colMeans(na.omit(inf))

#Plot both inflation series together
plot(inf[,1],xlab="",ylab="",lwd=2,col="dark grey",main="EU Inflation",ylim=c(-15,10))
par(new=TRUE)
plot(inf[,2],xlab="",ylab="",lwd=1,col="black",lty=1,ylim=c(-15,10))
legend("bottomleft",lty = c(1,1),lwd=c(2,1),col=c("dark grey","black"),legend = c("Monthly (Annualized)","Annual"))

#Make hi-res .jpgs
jpeg("EUCPI.jpg",width=5,height=3,units="in",res=300)
plot(tsdata[,1],xlab="",ylab="",lwd=4,col="dark grey",main="EU CPI",ylim=c(60,110))
par(new=TRUE)
plot(tsdata[,2],xlab="",ylab="",lwd=1,col="black",lty=2,ylim=c(60,110))
legend("bottomright",lty = c(1,2),lwd=c(4,1),col=c("dark grey","black"),legend = c("Not adjusted","Seasonally adjusted"))
dev.off()

jpeg("EUINF.jpg",width=5,height=3,units="in",res=300)
plot(inf[,1],xlab="",ylab="",lwd=2,col="dark grey",main="EU Inflation",ylim=c(-25,10))
par(new=TRUE)
plot(inf[,2],xlab="",ylab="",lwd=1,col="black",lty=1,ylim=c(-25,10))
legend("bottomleft",lty = c(1,1),lwd=c(2,1),col=c("dark grey","black"),legend = c("Monthly (Annualized)","Annual"))
dev.off()