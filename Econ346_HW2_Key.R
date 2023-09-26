######################################
# ECON 346 F23 HW2 Key
#####################################

#Open Data; set ID # in Line 6
ID = 8
## Read data from GitHub
data<-read.csv("https://raw.githubusercontent.com/hegerty/ECON346/main/ECON346_GrowthRates.csv",header=TRUE)
head(data)
X<-ID+1

head(data)
#Draw from list and select country
Country1<-c(5,5,5,4,4,3,3,3,2,2)
Country2<-c(4,3,2,3,2,2,5,4,4,5)
clist<-cbind(Country1,Country2)

#test
for(i in 1:length(Country1)){
  print(
cbind(colnames(data)[Country1[i]],colnames(data)[Country2[i]])
)
}

#Subset data; here I keep the old one
#Print data names
data1<-data[,clist[X,]]
head(data1)
print(paste("One variable is"," ",colnames(data1),sep=""))

#Calculate Covariance and correlation
#Print and paste
cov<-cov(data1)
print(paste("The covariance is"," ",round(cov[2,1],3),sep = ""))
print(paste("The Pearson correlation is"," ",round(cor(data1)[2,1],3),sep=""))
print(paste("The Spearman correlation is"," ",round(cor(data1,method = "spearman")[2,1],3),sep=""))

#Make Matrix
###################
##NOTE I never asked you for the product
##Read the formulas
######################################
#Here I put some empty columns in first
#Then I use existing columns to make new ones
dataset<-cbind(data1[,1],mean(data1[,1]),rep(0,nrow(data1)),rep(0,nrow(data1)),data1[,2],mean(data1[,2]),rep(0,nrow(data1)),rep(0,nrow(data1)))
dataset[,3]<-dataset[,1]-dataset[,2]
dataset[,7]<-dataset[,5]-dataset[,6]
dataset[,4]<-dataset[,3]^2
dataset[,8]<-dataset[,7]^2
colnames(dataset)<-c("growth1","mean1","growth1-mean1","(growth1-mean1)^2","growth2","mean2","growth2-mean2","(growth2-mean2)^2")
head(dataset)

#Covariance by hand
#Need product of two columns
cov1<-sum(dataset[,3]*dataset[,7])/(nrow(dataset)-1)
print(paste("This covariance is"," ",round(cov1,3),sep = ""))
print(paste("The other covariance was"," ",round(cov[2,1],3),sep = ""))

#Correlation
#Coveriance divided by the product of two standard deviations
cor1<-cov1/(sd(data1[,1])*sd(data1[,2]))
print(paste("This Pearson Correlation is"," ",round(cor1,3),sep = ""))

print(head(dataset))

pearson<-cor(data[,2:5],method="pearson")
pearson<-round(pearson,3)
pearson[lower.tri(pearson)=="TRUE"]<-""
noquote(pearson)

spearman<-cor(data[,2:5],method="spearman")
spearman<-round(spearman,3)
spearman[lower.tri(spearman)=="TRUE"]<-""
noquote(spearman)

cov<-cov(data[,2:5])
cov<-round(cov,3)
cov[lower.tri(cov)=="TRUE"]<-""
noquote(cov)

