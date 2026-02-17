####################################################
## ECON 343 S26
## Reshaping data
## 343_Emp_Orig_data.xlsx on GitHub
## Chicago MSA Employment (Monthly)
###################################################

data<-read.csv("https://raw.githubusercontent.com/hegerty/ECON343/refs/heads/main/343_Emp_data_2.csv") # pull data
head(data)

# drop column 1 if you need to--take a look and see what you need to do
years<-data[,1]
data<-as.matrix(data[,-1])
head(data)

# Make "long" using a loop
monthlydata<-NULL
for(i in 1:nrow(data)){
  d1<-data[i,]
  monthlydata<-c(monthlydata,d1)
}
monthlydata # your new series!

# 1. make a time series in R
ts1<-ts(monthlydata,start=c(1990,1),frequency = 12)
ts1
plot(ts1)

# 2. Make a regular column with names
rownames<-expand.grid(colnames(data),years)
colnames(rownames)<-c("Month","Year")
newdata<-cbind(rownames,monthlydata)
newdata
