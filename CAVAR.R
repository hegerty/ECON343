############################################
##ECON 343 NEIU
##VAR Example:
##Data import and Variable creation
##IRFs, Granger Causality, FEVD
##Special code for IRF plotting, tables
############################################

#Install packages if necessary
#Download data from FRED directly
#Mexico KFA = diff(RES) - CA; M1; U.S. Fed funds rate; US and MX Real GDP 

#install.packages("quantmod")
#install.packages("vars")

library(quantmod)
#CA as share of GDP, but Reserves in dollars
#Need to get MX GDP in dollars
#Get NGDP in pesos and exchange rate
setDefaults(getSymbols,src='FRED')
getSymbols("MEXB6BLTT02STSAQ",src='FRED')
###I now added code to truncate each series to match the video!
MEXB6BLTT02STSAQ=MEXB6BLTT02STSAQ["/2019-10-01"]
CA<-ts(MEXB6BLTT02STSAQ,end=c(2019,4),freq=4)
mean(CA)

getSymbols("MEXGDPNQDSMEI",src='FRED')
MEXGDPNQDSMEI=MEXGDPNQDSMEI["/2019-10-01"]
NGDP<-ts(MEXGDPNQDSMEI,end=c(2019,4),freq=4)

getSymbols("CCUSMA02MXQ618N",src='FRED')
CCUSMA02MXQ618N=CCUSMA02MXQ618N["/2020-01-01"]
MXNUSD<-ts(CCUSMA02MXQ618N,end=c(2020,1),freq=4)
NGDPUSD<-NGDP/MXNUSD

#KFA = diff(Reserves) - CA
getSymbols("MEXB6FARA01CXCUQ",src='FRED')
MEXB6FARA01CXCUQ=MEXB6FARA01CXCUQ["/2019-10-01"]
RES<-ts(MEXB6FARA01CXCUQ,end=c(2019,4),freq=4)
KFA<-(100*diff(RES)/NGDPUSD-CA)
mean(100*diff(RES)/NGDPUSD)
mean(KFA)

getSymbols("MANMM101MXQ189S",src='FRED')
MANMM101MXQ189S=MANMM101MXQ189S["/2018-10-01"]
MXM1<-ts(MANMM101MXQ189S,end=c(2018,4),freq=4)

getSymbols("BOGZ1FL072052006Q",src='FRED')  
BOGZ1FL072052006Q=BOGZ1FL072052006Q["/2019-10-01"]
USR<-ts(BOGZ1FL072052006Q,end=c(2019,4),freq=4)

getSymbols("GDPC1",src='FRED') 
GDPC1=GDPC1["/2020-01-01"]
USY<-ts(GDPC1,end=c(2020,1),freq=4)

getSymbols("NAEXKP01MXQ189S",src='FRED')   
NAEXKP01MXQ189S=NAEXKP01MXQ189S["/2019-10-01"]
MXY<-ts(NAEXKP01MXQ189S,end=c(2019,4),freq=4)

#Take logs of 3 variables
#Combine; notice different lengths
data<-cbind(log(USY),log(USR),log(MXY),log(MXM1),KFA)
names<-c("USY","USR","MXY","MXM1","KFA")
colnames(data)<-names
plot(data)
data<-na.omit(data)
plot(data)
dim(data)
#Here: 51 Quarters, 2006-2018

#Phillips-Perron stationarity test
pptab<-NULL
for(i in 1:ncol(data)){
pp<-PP.test(data[,i])
pptab<-rbind(pptab,pp$p.value)
}
pptab

#redo with differences
data<-cbind(diff(log(USY)),diff(USR),diff(log(MXY)),diff(log(MXM1)),KFA)
data<-na.omit(data)
colnames(data)<-names

pptab<-NULL
for(i in 1:ncol(data)){
  pp<-PP.test(data[,i])
  pptab<-rbind(pptab,pp$p.value)
}
pptab

#plot IRFs: Make VAR (LOWER triangular: Order with most exogenous on left and most endogenous on right)
#Loop to only make response of CA
#Still default settings
var<-data
library(vars)
var1<-VAR(var,type = c("const"),lag.max = 4,ic="SC")
for(i in 1:4){
  irf1<-irf(var1,impulse = colnames(var1$datamat[i]),response = colnames(var1$datamat[5]),n.ahead = 6,ortho = TRUE,ci=0.95,boot=TRUE,runs=100,cumulative = FALSE)
plot(irf1)
}

#Get FEVDs at 1,4,8,12-quarter horizons
fevd1<-fevd(var1,n.ahead = 12)
fevd2<-fevd1$KFA
fevdtab<-round(100*fevd2[c(1,4,8,12),],2)
rownames(fevdtab)<-c(1,4,8,12)
print(fevdtab)
plot(fevd1)

#Granger Causality tests: I make pairwise vars, then a nice table
gctab<-NULL
var<-data
library(vars)
var1<-VAR(var,type = c("const"),lag.max = 4,ic="SC")
for(i in 1:4){
   var2<-VAR(var[,c(i,5)],type = c("const"),lag.max = 4,ic="SC")
  gc<-causality(var2,cause = colnames(var1$datamat[i]))
  gc1<-cbind(as.numeric(gc$Granger$statistic),gc$Granger$p.value)
  gctab<-rbind(gctab,gc1)
}
colnames(gctab)<-c("Statistic","p-val.")
rownames(gctab)<-colnames(var1$datamat[c(1:4)])
gctab<-round(gctab,3)
print(gctab)

#Here I plot my own IRFs!
irfx<-c(0:6)
par(mfrow=c(2,2))
for(i in 1:4){
  irf1<-irf(var1,impulse = colnames(var1$datamat[i]),response = colnames(var1$datamat[5]),n.ahead = 6,ortho = TRUE,ci=0.95,boot=TRUE,runs=100,cumulative = FALSE)
  volirf1<-unlist(irf1[1]$irf[1])
  volirf1l<-unlist(irf1[2]$Lower[1])
  volirf1u<-unlist(irf1[3]$Upper[1])
  
  #par(new=FALSE)
  plot(irfx,volirf1,type="l",lwd=2,ylab="",xlab="",cex.lab=2, ylim=1.2*c(min(volirf1l),max(volirf1u)),main=colnames(var1$datamat[i]))
  par(new=TRUE)
  plot(irfx,volirf1l,type="l",lwd=1,ylab="",xlab="",lty=4,ylim=1.2*c(min(volirf1l),max(volirf1u)))
  par(new=TRUE)
  plot(irfx,volirf1u,type="l",lwd=1,ylab="",xlab="",lty=4,ylim=1.2*c(min(volirf1l),max(volirf1u)))
  abline(h=0,lty=2,lwd=1.5)
  }

