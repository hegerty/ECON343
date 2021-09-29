###################################################
##Macroeconomic Data Analysis
##Econ 343 NEIU
##Japanese Yen Examples
##Source: International Financial Statistics (IMF)
###################################################

#Pull pre-loaded data from website
data<-read.csv("https://raw.githubusercontent.com/hegerty/ECON343/main/JPY_Data.csv",header=TRUE)
#Generate main variables
jpy<-data[,2]
jpy<-ts(jpy,start = c(1970,1),frequency = 12)
dlnjpy<-diff(log(jpy))

#Graph from Book Cover 
#jpeg("jpy2.jpg", units="in", width=5.5, height=5.25, res=600)
plot(jpy,ylab="",xlab="",lwd=3)
par(new=TRUE)
plot(dlnjpy,col="#444444",ylab="",xlab="",main="",axes=FALSE)
axis(4)
#dev.off()

#Fig. 3.3: Comparing percentage changes
#Fig. 3.5: Comparing lag lengths

app1<-1200*(diff(jpy)/lag(jpy,-1))
app12<-100*(diff(jpy,12)/lag(jpy,-12))
series<-na.omit(cbind(app1,1200*dlnjpy,app12))
#jpeg("jpy3.jpg", units="in", width=5.5, height=3, res=600)
ts.plot(series[,c(1,3)],lwd=c(2,4),col=c("dark grey","black"),xlab="",ylab="")
#dev.off()

#jpeg("jpy4.jpg", units="in", width=5.5, height=3, res=600)
ts.plot(series[,c(1,2)],lwd=c(3,2),col=c("black","dark grey"),xlab="",ylab="")
#dev.off()

#Fig. 3.6: Rolling Standard Deviations
#install.packages("zoo")
library(zoo)
jpyvol12<-rollapply(dlnjpy,12,FUN="sd")

plot(100*dlnjpy,lwd=2,col="#999999",ylab="",xlab="",main="",axes=FALSE)
axis(4)
par(new=TRUE)
plot(100*jpyvol12,ylab="",xlab="",lwd=3)

