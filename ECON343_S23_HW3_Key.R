############################################
#### ECON 343 Spring 2023
#### HW3 KEY
############################################

## Here is the starting data (I am providing it here, but you should have gotten it yourself)
data<-read.csv("https://raw.githubusercontent.com/hegerty/ECON343/main/ECON343_S23_HW3_Data.csv")


## Check data and omit NAs
head(data)
tail(data)
data<-na.omit(data)
tail(data)
dim(data)
data<-data[1:672,]
tail(data)
colnames(data)<-c("Date","P_ALL","P_FOOD","P_HOUS","P_GAS")
head(data)
##Q2: Make a time series and plot it
datats<-ts(data[,2:5],start=c(1967,1),frequency =12)
plot(datats)

## Better to plot it all as one
ts.plot(datats,lty=c(2,2,3,4),col=rep(c("black","dark grey"),2))
legend("topleft",legend=colnames(datats),lty=c(2,2,3,4),col=rep(c("black","dark grey"),2),bty="n")
## Gasoline is most variable.

##Q3: Calculate all the new variables
colnames(datats)
inf_all_12<-100*diff(datats[,1],12)/lag(datats[,1],-12)
inf_all_12
inf_all<-1200*diff(datats[,1],1)/lag(datats[,1],-1)
dlnpall<-1200*diff(log(datats[,1]))

#Check two alternative monthly inflation rates
head(cbind(inf_all,dlnpall))

#Make some more variables
inf_food<-1200*diff(datats[,2],1)/lag(datats[,2],-1)
inf_hous<-1200*diff(datats[,3],1)/lag(datats[,3],-1)
inf_gas<-1200*diff(datats[,4],1)/lag(datats[,4],-1)
pratio<-datats[,3]/datats[,1]
infdiff<-inf_hous-inf_all

# Combine all the variables into a new dataset
dataset<-cbind(datats,inf_all_12,inf_all,dlnpall,inf_food,inf_hous,inf_gas,infdiff,pratio)
head(dataset)
tail(dataset)

## Q4 Summary statistics table
colnames(dataset)
stat1<-na.omit(dataset[,5:11])
tab<- rbind(apply(stat1,2,mean),apply(stat1,2,sd),apply(stat1,2,min),apply(stat1,2,max),nrow(stat1))     
rownames(tab)<-c("mean","sd","min","max","N")
round(tab,3)
which.max(tab[2,])
#This matches.

# Here it is if you used 100, not 1200, for your monthly inflation rates
# If you rounded, it might be slightly different
# Also, you may have used different lengths--here is the common (shorter) sample.
colnames(dataset)
stat1<-na.omit(dataset[,5:11])
stat1[,2:7]<-stat1[,2:7]/12
tab<- rbind(apply(stat1,2,mean),apply(stat1,2,sd),apply(stat1,2,min),apply(stat1,2,max),nrow(stat1))     
rownames(tab)<-c("mean","sd","min","max","N")
round(tab,3)
which.max(tab[2,])
#This matches.


#Q5
# plot inflation rates: 12-month vs 1-month
xy=dataset[,5:6]
x<-xy[,1]
y<-xy[,2]
plot(y,x,pch=20,xlab=colnames(dataset)[6],ylab=colnames(dataset)[5],main="Inflation Correlations")
round(cor(na.omit(xy))[2],3)
## These are fairly highly correlated.

## Extra: add a regression line!
abline(lm(y~x),lwd=4,lty=3, col="dark grey")
## And get the regression equation
reg1<-lm(y~x)
summary(reg1)

# 1-month % changes vs log changes
xy=dataset[,c(7,6)]
x<-xy[,1]
y<-xy[,2]
plot(y,x,pch=20,xlab=colnames(dataset)[6],ylab=colnames(dataset)[7],main="Inflation Correlations")
round(cor(na.omit(xy))[2],3)
## These are fairly highly correlated.

## Add a regression line!
abline(lm(y~x),lwd=3,lty=3, col="dark grey")
## And get the regression equation
reg1<-lm(y~x)
summary(reg1)

round(cor(na.omit(xy))[2],3)
# The two measures are essentially equivalent.

#Q6: Plot the relative price of housing
plot(dataset[,12],lwd=2,xlab="",ylab="",main="Price ratio")
abline(h=1,lty=6)
# it might be rising?
#The Phillips-Perron test shows that it is nonstationary:
PP.test(pratio)
# You could also split the sample: 1980, 2008 to look for subperiods

#Q7: Plot all prices (inflation) and housing inflation 
q7<-dataset[,c(6,8)]
ts.plot(q7,col=c("red","black"),lwd=c(2,2))
legend("topright",legend=colnames(q7),lwd=c(2,2),q7,col=c("red","black"),bty="n")
# It is hard to tell--that is why the summary statistics help.
tab[c(1,2),c(3,5)]
#Both are basically the same. Housing is a little higher, but this is not significant.
t.test(q7[,1],q7[,2])

#Q8: Make a histogram for the inflation differential
## I made the breaks myself.
q8<-dataset[,11]
hist(q8,breaks = 30, main ="Inflation differential: Housing - All Prices")
## Is this skewed?



