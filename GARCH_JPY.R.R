###########################################
###ECON 343 NEIU
###Macroeconomic Data Analysis
###GARCH Modeling in R 
###Yen per dollar, Monthly, 1974-2019
###########################################

#Import data and check that it lines up and ends correctly
data<-read.csv("https://raw.githubusercontent.com/hegerty/ECON343/main/JPYUSD.csv")
tail(data)

#set "level" of variable and name it E
E<-ts(data[,2],end=c(2019,10),frequency = 12)

#Take log differences and name it e
e=diff(log(E))

#Plot w/zero line
plot(e,ylab="",xlab="")
abline(h=0,col="dark grey",lty=3,lwd=3)

# Some Box-Jenkins; here I am going with an AR(1) process
acf(e)
pacf(e)
ar1<-arima(e,c(1,0,0))
ar1

#Adding an extra AR or MA have insignificant coefficients
ar2<-arima(e,c(2,0,0))
ar2
arma11<-arima(e,c(1,0,1))
arma11

#Install and use 'rugarch' package
#install.packages("rugarch")
library(rugarch)

#specify GARCH(1,1) model
#note: "s" for "standard"
#GARCH order = (1,1)
#mean model = AR(1)
g1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model=list(armaOrder=c(1,0)),distribution.model="std")
#fit model
garch11<-ugarchfit(g1,data = e)
#examine coefficients and model
garch11

#make a GARCH variance series
vole <- ts(garch11@fit$sigma^2,end=c(2019,10),frequency = 12)

#Plot with axes and header
plot(vole,xlab="",ylab="",main="JPYUSD Volatility (GARCH[1,1])")

#Now try an Exponential GARCH (EGARCH) model: This allows for asymmetries between positive and negative shocks
g1e<-ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),mean.model=list(armaOrder=c(1,0)),distribution.model="std")
garch11e<-ugarchfit(g1e,data = e)
coef(garch11e)
garch11e
#AIC, coefficients -> "Standard" GARCH probably preferred

#Make time series and plot
vole2 <- ts(garch11e@fit$sigma^2,end=c(2019,10),frequency = 12)
plot(vole2,xlab="",ylab="",main="JPYUSD Volatility (EGARCH[1,1])")

#How similar are the series?
cor(vole,vole2)

#Plot them both together
ts.plot(vole,vole2,col=c("green","red"),xlab="")
legend("topright",legend=c("Standard","Exponential"),col=c("green","red"),lty=c(1,1))

#Try plotting difference in measures 
voldif<-vole-vole2
plot(voldif,xlab="")

#Plot difference vs. exchange rate returns:
abline(h=0,col="dark grey",lty=2)
plot(voldif,e,pch=20,xlab="GARCH - EGARCH")
abline(h=0,lty=2)
abline(v=0,lty=2)
#No obvious relationship 
