#############################################
##Macroeconomic Data Analysis
##Econ 343 NEIU
##Seasonal Employment
##Madison, WI monthly employment: Source: BLS
#############################################

#Pull pre-loaded data from website and set as time series
data<-read.csv("https://sites.google.com/site/swhegerty/macroeconomic-data-analysis/EMPL_Data.csv",header=TRUE)
empl<-ts(data[,3],start=c(1990,1),frequency = 12)

#Deasonalize with Census X13
#install.packages("seasonal")
library(seasonal)
s1<-seas(empl)
empl.seas<-s1$data[,3]
empl.series<-cbind(empl.seas,empl)

#plot together
#Include code to print a 600dpi .jpg to file
#jpeg("madempl.jpg",height=3,width=6,units="in",res=600)
#par(mar=c(2,2,2,2))
ts.plot(empl.series,xlab="",ylab="",lwd=c(3,2),col=c("black","#666666"),main="Madison, WI Monthly Employment")
legend("topleft",legend=c("Deseasonalized","Original"),lwd=c(3,2),col=c("black","#666666"),bty="n")
#dev.off()

