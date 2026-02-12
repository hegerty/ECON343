################################################
## ECON 343 S26
## HW2 Key
###############################################

# Get data: I put it on GitHub
data<-read.csv("https://raw.githubusercontent.com/hegerty/ECON343/refs/heads/main/ECON343_HW2_Data2.csv")
head(data)
tail(data) # Quarterly, 1993-2025. I had to cut off the missing data

# Make new variables
RY<-100*(data$GDP/data$GDPDEF)
RC<-100*(data$CONS/data$GDPDEF)
RG<-100*(data$GOV/data$GDPDEF)
RI<-100*((data$GFKF+data$Change_Inv)/data$GDPDEF) # note the formula!!
CY<-100*(data$CONS/data$GDP)
GY<-100*(data$GOV/data$GDP)
IY<-100*((data$GFKF+data$Change_Inv)/data$GDP)
LNRY<-100*(log(RY))

# Make one database
data2<-cbind(data,RY,RC,RG,RI,CY,GY,IY,LNRY)
head(data2)

tsdata<-ts(data2[,-1],start=c(1993,1),freq=4) # make a Time Series object
colnames(tsdata) # see which you need to graph

# Make both plots
# I add color, dashes,width, and a legend
ts.plot(tsdata[,c(7:9)],col=c("black","dark grey","black"),lwd=c(2,2,2),lty=c(1,1,2),xlab="")
legend("right",legend=c("RY","RC","RG"),col=c("black","dark grey","black"),lwd=c(2,2,2),lty=c(1,1,2),bty="n")

ts.plot(tsdata[,c(11:13)],col=c("black","dark grey","black"),lwd=c(2,2,2),lty=c(1,1,2),xlab="")
legend("right",legend=c("RY","GY","IY"),col=c("black","dark grey","black"),lwd=c(2,2,2),lty=c(1,1,2),bty="n")

# all correlations
plot(data2$RY,data2$LNRY, pch=20,xlab="RY",ylab="LNRY") #scatterplot first just because
print(round(cor(data2$RY,data2$LNRY),3))

# This is why you need shares, not dollar values
cor(data2$RG,data2$RI) # too big!!
cor(data2$GY,data2$IY)

# Choose start and end quarter
# This is based on your id

##########
## Put in your last ID digit here
id.digit=3


## This makes it all easy
id1<-expand.grid(c(124,128,129),c(1,5,9))[,c(2,1)]
id2<-id1[id.digit,]
s1<-as.numeric(id2[1])
e1<-as.numeric(id2[2])


#subset what you need!
sample<-tsdata[s1:e1,11:13]
head(sample)

# all functions at once
colnames(sample)
stats.table<-rbind(
colMeans(sample),
apply(sample,2,FUN="sd"),
apply(sample,2,FUN="min"),
apply(sample,2,FUN="median"),
apply(sample,2,FUN="max")
)
stats.table # kinda ugly
rownames(stats.table)<-c("Mean","SD","Min","Med","Max")
round(stats.table,3)

# and do that one correlation
print(round(cor(sample[,2],sample[,3]),3))


#### Do them all at once
for (i in 1:9){
  id2<-id1[i,]
  s1<-as.numeric(id2[1])
  e1<-as.numeric(id2[2])
  
sample<-tsdata[s1:e1,11:13]
stats.table<-rbind(
  colMeans(sample),
  apply(sample,2,FUN="sd"),
  apply(sample,2,FUN="min"),
  apply(sample,2,FUN="median"),
  apply(sample,2,FUN="max")
)
rownames(stats.table)<-c("Mean","SD","Min","Med","Max")
print(paste0("Digit = ",i))
print(round(stats.table,3))
print(round(cor(sample[,2],sample[,3]),3))
}
