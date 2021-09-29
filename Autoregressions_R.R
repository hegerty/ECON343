#Lags, differences, autoregression
#See "Autocorrelation" file for more
#import deseasonalized U.S. real GDP (FRED data); set as time series
setwd("F:/")

data<-read.csv("https://raw.githubusercontent.com/hegerty/ECON343/main/USMXRY.csv",header=TRUE)
head(data)
dim(data)

usry<-ts(data[,2],start=c(1993,1),frequency = 4)
lnusry<-log(usry)
par(mfrow=c(1,2))
plot(usry)
plot(lnusry)
par(mfrow=c(1,1))

#make a lag variable and a difference variable
laglnusry<-lag(lnusry,-1)
dlnusry<-diff(lnusry)
plot(dlnusry)

#also make a lagged difference
lagdlnusry<-lag(dlnusry,-1)

#put all four in a new datafrale
data2<-cbind(lnusry,laglnusry,dlnusry,lagdlnusry)
head(data2)
tail(data2)
#remove extra observation
data2<-data2[-104,]
tail(data2)

#Try regressing lnusry on its lag
reg1<-lm(data2[,1]~data2[,2])
summary(reg1)
#Coefficient close to one
#BUT: Stationarity may be an issue

#Note that simply plotting residuals does not include time!
plot(reg1$residuals)

#Plot a time series of residuals
plot.ts(reg1$residuals)

#Conduct Phillips-Perron stationarity test
PP.test(reg1$residuals)
#Residuals are stationary!

#But series are not
PP.test(data$USY)
PP.test(data2[,1])

#Need to difference the variable
PP.test(dlnusry)

#Try an autoregression on differenced lnusry
reg2<-lm(data2[,3]~data2[,4])
summary(reg2)

#Can also use ar()
ar1<-ar(dlnusry,order.max = 1)
ar1

#Can also do it with arima()
arima1<-arima(dlnusry,order = c(1,0,0))
arima1


#Make a nice table
#Extract the important values
summary(reg2)[4]
#Keep t-stat for this example

#Start with a simple table--separate columns
table1<-summary(reg2)[4]
table1$coefficients[,c(1,3)]
table<-table1$coefficients[,c(1,3)]

#Add labels and round
rownames(table)<-c("Constant","DLNY(-1)")
round(table,3)

#Now concatenate and add parentheses in a new table
t1<-paste(round(table[1,1],3)," (",round(table[1,2],3),")",sep="")
t2<-paste(round(table[2,1],3)," (",round(table[2,2],3),")",sep="")

#Include R-squared
table2<-rbind(t1,t2,round(summary(reg2)$r.squared,3))
table2

#Remove quotes and add names
table2<-noquote(table2)
table2
rownames(table2)<-c("Constant","DLNY(-1)","R-sq.")
colnames(table2)<-"Coeff. (t-stat.)"

table2

#Write to a .csv file
write.csv(table2,"Table2.csv")
