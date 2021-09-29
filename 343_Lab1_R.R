#Change line 2 to YOUR folder location
setwd("G:/")
data<-read.csv("https://github.com/hegerty/ECON343/blob/main/ECON343_Lab1_Data.csv",header = TRUE)
#Some of the info here is just to double-check as you go
head(data)
dim(data)
data<-na.omit(data[,c(2,3)])
colnames(data)<-c("CPI","PWTI")
data$RPWTI<-100*data[,2]/data[,1]

#Now make a time series
data<-ts(data,start=c(1980,1),frequency = 12)
dim(data)
head(data)
plot(data[,3])
INF<-100*(data[,1]/lag(data[,1],-12)-1)
plot(INF)
PCH_PWTI<-100*(data[,2]/lag(data[,2],-12)-1)
PCH_RPWTI<-100*(data[,3]/lag(data[,3],-12)-1)
data<-cbind(data,PCH_PWTI,PCH_RPWTI,INF)
head(data)
tail(data)
dim(data)
cor(data)
cor(na.omit(data[,4:6]))
colnames(data)<-c("CPI","PWTI","RPWTI","PCH_PWTI","PCH_RPWTI","INF")
print(round(cor(na.omit(data[,4:6])),3))
mean(na.omit(data[,4]))
means<-apply(na.omit(data[,3:6]),2,FUN = "mean")
sds<-apply(na.omit(data[,3:6]),2,FUN = "sd")
print(round(means,3))
print(round(sds,3))

data2<-as.data.frame(na.omit(data[,4:6]))
plot(data2[,c(1,3)])

#Re-plotting the real oil price, also showing the average
plot(data[,3],xlab="",ylab="",lwd=2,main="Real Oil Prices (WTI)")
abline(h=mean(data[,3]),col="dark grey",lty=2,lwd=1.5)

