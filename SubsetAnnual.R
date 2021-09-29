###############################################################
##ECON 343 NEIU
##Creating annual averages of monthly data without a package
###############################################################

#Open data and create data series (Real WTI oil prices)
data<-read.csv("https://github.com/hegerty/ECON343/blob/main/WTI_Data.csv",header=TRUE)
head(data)
data$RWTI=100*data$WTI/data$PPI
#Examine data: 12 months per year
head(data,14)
tail(data)
#need to combine obs 1-12, 13-24, etc.
#Gaps of p = 12
#Start with zero, but make i = one, and add one for the start of each subgroup
#find endpoint: Whole number of years
l1<-nrow(data)
l1
l<-l1/12
l
length<-l1%/%12
length

#Write loop to average real WTI within each year
series<-data$RWTI
p=12
new=NULL
for(i in 1:length){
  ydata<-series[((i-1)*p+1):(i*p)]
  yavg<-mean(ydata)
  new=rbind(new,yavg)
}
avgrwti<-new
avgrwti

ts.plot(series)
ts.plot(avgrwti)

#make the years, manually
years<-rep(1979+1:length(avgrwti))
years
#or this way
years1<-data[1:(12*length),1]
years1<-strsplit(years1,"/")
years1
typeof(years1)
years1<-data.frame(Reduce(rbind,years1))
years1
years1<-unique(years1$X3)
years1

#add the years
avgrwti<-noquote(cbind(years1,avgrwti))
avgrwti
#Simple time-series plot
plot(avgrwti,type="l",main="Real WTI Price",xlab="")
