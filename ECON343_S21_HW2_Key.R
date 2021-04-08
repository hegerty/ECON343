############################################
#### ECON 343 Spring 2021
#### HW2 KEY
############################################

##Pull the provided data and check to make sure it looks OK
##Also shows frequency and timespan
data<-read.csv("https://raw.githubusercontent.com/hegerty/ECON343/main/ECON343_S21_HW2_Data.csv")
head(data)
tail(data)

## Q1
## 1955q1-2020q1, quarterly

#Q2 Create new variables: Real and GDP shares
usry<-100*data$US_Y/data$US_CPI
usrc<-100*data$US_C/data$US_CPI
usrg<-100*data$US_G/data$US_CPI
usri<-100*data$US_I/data$US_CPI
cy<-100*data$US_C/data$US_Y
gy<-100*data$US_G/data$US_Y
iy<-100*data$US_I/data$US_Y
## Also log real GDP
lnry<-log(usry)

## Make a set of time series
tsdata<-cbind(usry,usrc,usrg,usri,cy,gy,iy,lnry)
tsdata<-ts(tsdata,start=c(1955,1),freq=4)

## Q4 Plot the time series (add a legend and differentiate the lines)
ts.plot(tsdata[,c(1,2,3)],lwd=c(1,2,3),xlab="")
legend("topleft",legend=colnames(tsdata[,c(1,2,3)]),lwd=c(1,2,3),bty="n")
cor(usry,lnry)
# All are rising, but perhaps Government spending is "flattest."

## Plot the GDP shares
ts.plot(tsdata[,c(5,6,7)],lwd=c(1,2,3),xlab="")
legend("left",legend=colnames(tsdata[,c(5,6,7)]),lwd=c(1,2,3),bty="n")
# Consumption is highest; most are fairly flat. There was a drop in Investment around the 2008-2009 financial crisis.

#Q5 Calculate the correlations
cor(usry,lnry)
## These are the same basically since one is a transformation of the other.
## In fact, Spearman correlation = 1
cor(usry,lnry, method="spearman")

#Q6: Summary statistics
## Here I run a loop to get all versions based on ID Number
lastdigit<-0:9

##Choose the start and endpoints
## Here I already matched the appropriate points
start<-c(1,1,1,1,5,5,5,9,9,9)
end<-c(252,252,256,260,252,256,260,252,256,260)

# Then match ID number with points
for(i in 1:length(start)){
  print(noquote(paste("**************************")))
  stat1<-tsdata[start[i]:end[i],5:7]
 tab<- rbind(apply(stat1,2,mean),apply(stat1,2,sd),apply(stat1,2,median),apply(stat1,2,min),apply(stat1,2,max))     
  rownames(tab)<-c("mean","sd","med","min","max")
  print(noquote(paste("ID# = ",lastdigit[i],sep="")))
  print(round(tab,3))
  print(noquote("The most variable is:"))
  print(which.max(tab[2,]))
  print(noquote(paste("cor(g/y,i/y) = ",round(cor(stat1[,2:3])[2],3),sep="")))
  print(noquote("This correlation is close to zero."))
  }

tabcv<-NULL
## Extra: Calculate the Coefficient of Variation rather than the standard deviation
for(i in 1:length(start)){
  print(noquote(paste("**************************")))
  stat1<-tsdata[start[i]:end[i],5:7]
  tabcv<- rbind(apply(stat1,2,mean),apply(stat1,2,sd))
  tabcv<- rbind(tabcv,tabcv[2,]/tabcv[1,])     
  
  rownames(tabcv)<-c("mean","sd","cv")
  print(noquote(paste("ID# = ",lastdigit[i],sep="")))
  print(round(tabcv,3))
  print(noquote("The most variable (highest CV) is:"))
  print(which.max(tabcv[3,]))
}

