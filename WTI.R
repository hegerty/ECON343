##############################
##Macroeconomic Data Analysis
##Econ 343 NEIU
##Oil-Price Volatility Example
###############################

#Pull pre-loaded data from website
# (You should also be able to locate and download data independently)
data<-read.csv("https://raw.githubusercontent.com/hegerty/ECON343/main/WTI_Data.csv",header=TRUE)

#Generate main variables
wti<-data[,2]
ppi<-data[,3]
#real oil price
rwti<-ts(100*wti/ppi,start=c(1980,1),frequency = 12)
#log changes and percent changes
dlnrwti<-100*diff(log(rwti))
momrwti<-100*diff(rwti)/lag(rwti,-1)
yoyrwti<-100*diff(rwti,12)/lag(rwti,-12)

#Calculate rolling standard deviation
#Here I match Excel's Population std. dev w/ (n/n1)^12
#install.packages("zoo")
library(zoo)
sd12dln<-rollapply(dlnrwti,12,FUN="sd")
sd12dln<-ts(sd12dln,end=c(2016,9),frequency = 12)
sd12dln<-sqrt(11/12)*sd12dln
series<-na.omit(cbind(wti,ppi,rwti,dlnrwti,momrwti,yoyrwti,sd12dln))
print(head(series))

#Plot main variables: E2, E4, E5,E6
ts.plot(cbind(wti,rwti),lty=c(1,3),col=c("black","#333333"),xlab="",ylab="",main = "Nominal and Real Oil Prices")
legend("topleft",legend=c("WTI","RWTI"),lty=c(1,3),col=c("black","#333333"),bty="n")
ts.plot(rwti,xlab="",ylab="",main = "Real WTI Oil Price")
ts.plot(dlnrwti,xlab="",ylab="",main = "Real WTI (monthly log changes)")
ts.plot(sd12dln,xlab="",ylab="",main = "Real WTI Volatility, Rolling St. Dev.")

#Correlation table: E7
#Lower triangle left blank
cortable<-round(cor(series[,4:6]),3)
cortable[lower.tri(cortable)]<-""
print(noquote(cortable))

#Summary statistics: E8
#Note sample std.dev
#rounded to 2 decimal places
series2<-series[,c(1,3,4,7)]
sumtable<-rbind(colMeans(series2),apply(series2,2,sd),apply(series2,2,min),apply(series2,2,max))
rownames(sumtable)<-c("Mean","SD","Min","Max")
print(round(sumtable,2))

#GARCH: Different software = not an exact match with Eviews
#install.packages(rugarch)
library(rugarch)

#Check ACF and PACF: E9
acf(dlnrwti)
pacf(dlnrwti)

#estimate ARMA models
#I don't recreate the tables here
ar1<-arima(dlnrwti,order = c(1,0,0))
print(ar1)
arma11<-arima(dlnrwti,order = c(1,0,1))
print(arma11)

g1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),mean.model=list(armaOrder=c(1,0)),distribution.model="std")
#fit model
garch11<-ugarchfit(g1,data = dlnrwti)
#examine coefficients and model
garch11@model$pars
print(coef(garch11))

#Create GARCH series and plot: E13
vol1 <- ts(garch11@fit$sigma^2,end=c(2016,9),frequency = 12)
ts.plot(vol1,xlab="",yab="",main="Real WTI Volatility, GARCH(1,1)")

          
