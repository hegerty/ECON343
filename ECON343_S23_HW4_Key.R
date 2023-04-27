############################################
#### ECON 343 Spring 2023
#### HW4 KEY
############################################

## Here is the starting data (I am providing it here, but you should have gotten it yourself)
data<-read.csv("https://raw.githubusercontent.com/hegerty/ECON343/main/ECON343_S23_HW4_Data.csv")



## Q2 - The main objective is from the website

## Check data and omit NAs
head(data)
tail(data)
data<-na.omit(data)
tail(data)
tsdata<-ts(data[,-1],start=c(1976,1),frequency = 12)
head(tsdata)
tail(tsdata)
tsdata<-tsdata[,c(1,3,2,4)]
## Make all the variables first; check the length, etc. as you go
infus12<-100*diff(tsdata[,1],12)/lag(tsdata[,1],-12)
infus<-1200*diff(tsdata[,1])/lag(tsdata[,1],-1)
infmx12<-100*diff(tsdata[,3],12)/lag(tsdata[,3],-12)
infmx<-1200*diff(tsdata[,3])/lag(tsdata[,3],-1)
infdiff<-infmx12-infus12  
realrus<-tsdata[,2]-infus12

# I check the observations because of the lost observations--they need to be at the front (not the end)
head(realrus)
tail(realrus)
length(realrus)
dim(tsdata)

realrmx<-tsdata[,4]-infmx12
nomrdiff<-tsdata[-c(1:12),4]-tsdata[-c(1:12),2]
nomrdiff<-ts(nomrdiff,end=c(2022,12),frequency = 12)
realrdiff<-realrmx-realrus
realrdiff<-ts(realrdiff,end=c(2022,12),frequency = 12)

# One big set of all 13 variables
# I call it ts12 because 12 of the variables are new
ts12<-cbind(tsdata,infus12,infus,infmx12,infmx,infdiff,realrus,realrmx,nomrdiff,realrdiff)
colnames(ts12)[1:4]<-c("pus","rus","pmx","rmx")
#Note the NAs, which are the lost observations
head(ts12)
tail(ts12)
ts12<-na.omit(ts12)
head(ts12)
tail(ts12)
# Q3-1-month loses one month, and 12-month loses 12. The data that start in 1976 wind up starting in 1977.

# I always need to remind myself of the variable names when I choose th column numbers. 
colnames(ts12)

## Here I just copy the same code, but for different pairs
# Q4
par(mfrow=c(1,4))
pair<-ts12[,c(4,2)]
ts.plot(pair,lwd=c(2,2),col=c("black","dark grey"),xlab="")
legend("topright",legend=colnames(pair),lwd=c(2,2),col=c("black","dark grey"),bty="n")

pair<-ts12[,c(5,7)]
ts.plot(pair,lwd=c(2,2),col=c("black","dark grey"),xlab="")
legend("topright",legend=colnames(pair),lwd=c(2,2),col=c("black","dark grey"),bty="n")

pair<-ts12[,c(6,8)]
ts.plot(pair,lwd=c(2,2),col=c("black","dark grey"),xlab="")
legend("topright",legend=colnames(pair),lwd=c(2,2),col=c("black","dark grey"),bty="n")

pair<-ts12[,c(7,8)]
ts.plot(pair,lwd=c(2,2),col=c("black","dark grey"),xlab="")
legend("topright",legend=colnames(pair),lwd=c(2,2),col=c("black","dark grey"),bty="n")

# Mexican inflation is high before the year 2000. The two measures, 1- ans 12-month, do not differ as much.
par(mfrow=c(1,1))

# Q5
pair<-ts12[,c(10,11)]
ts.plot(pair,lwd=c(2,2),col=c("black","dark grey"), main="Real Rates")
legend("topright",legend=colnames(pair),lwd=c(2,2),col=c("black","dark grey"),bty="n")
# Periods of high inflation in Mexico also have negative real interest rates.

# Q6
pair<-ts12[,c(12,13)]
ts.plot(pair,lwd=c(2,2),col=c("black","dark grey"),main="Interest-Rate Differentials")
legend("topright",legend=colnames(pair),lwd=c(2,2),col=c("black","dark grey"),bty="n")
# Mexico has higher nominal rates during the 1990s but lower real rates.


# Q7
pair<-ts12[,c(9,13)]
x=pair[,1]
y=pair[,2]
plot(x,y,pch=20,xlab=colnames(pair)[1],ylab=colnames(pair)[2])
abline(lm(y~x))
legend("topright",legend=paste("cor = ",round(cor(x,y),3),sep=""),bty="n")
 ## The variables are negatively correlated. High inflation lowers the real rate (and the differential)

# Q8
tables<-ts12[,5:13]
tables<-rbind(colMeans(tables),apply(tables,FUN = "sd",2),apply(tables,FUN = "min",2),apply(tables,FUN = "max",2))
rownames(tables)<-c("Mean","SD","Min","max")
round(tables,2)
## Mexico's inflation is higher and more volatile. The real interest-rate differential is negative.

# BONUS
# Split by date: i1 and i2
d1<-which(time(ts12)==2000)
d2<-ts12[,7]
i1<-d2[1:(d1-1)]
i2<-d2[d1:nrow(ts12)]
length(i1)+length(i2)-nrow(ts12)

# Here are each period's statistics. I chose MX inflation since that drives everything.
bonustable<-rbind(c(mean(i1),mean(i2)),c(sd(i1),sd(i2)))
colnames(bonustable)<-c("Pre-2000","Post-2000")
rownames(bonustable)<-c("Mean","SD")
round(bonustable,2)

# inflation is lower and less volatile

# You can do a t-test
t.test(i1,i2)
# The difference is clearly significant

# Or an F test for SDs
var.test(i1, i2, alternative = "two.sided")
# Here too

