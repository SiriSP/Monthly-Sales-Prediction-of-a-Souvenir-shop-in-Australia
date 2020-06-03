library(fpp)
library(fpp2)
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirts<- ts(souvenir, frequency=12, start=c(1987,1))
plot(souvenirts)

transformsouvenirts <- log(souvenirts)
plot(transformsouvenirts)

transformsouvenirtsdecompose<-decompose(transformsouvenirts)
plot(transformsouvenirtsdecompose)

fit <- stl(transformsouvenirts, s.window=5)
plot(transformsouvenirts, col="gray",
     main="Sales Forecast",
     ylab="Sales", xlab="Time")
lines(fit$time.series[,2],col="red",ylab="T
      rend")

#since it has varying seasonality we can have seasonal subseries plots of the seasonal component
#this helps to visualize the variation in the seasonal component over time 
monthplot(fit$time.series[,"seasonal"], main="", ylab="Seasonal")

 
train<- window(transformsouvenirts,start=1987,end=1991,frequency=12)
test<-window(transformsouvenirts,start=1992)
fit1<-snaive(train,h=12)
autoplot(window(train,start=1987))+autolayer(fit1,series="Snaive",PI=FALSE)+xlab("Time")+ylab("sales")+
ggtitle("Sales forecast using snaive")+guides(colour=guide_legend(title = "Forecast"))
accuracy(fit1,test)


train<- window(transformsouvenirts,start=1987,end=1991,frequency=12)
test<-window(transformsouvenirts,start=1992)
fit2 <- hw(train,seasonal="additive")
autoplot(window(train,start=1987))+autolayer(fit2,series="Holtsseasonal",PI=FALSE)+xlab("Time")+ylab("sales")+
  ggtitle("Sales forecast using Holtsseasonal")+guides(colour=guide_legend(title = "Forecast"))
accuracy(fit2,test)


train<- window(transformsouvenirts,start=1987,end=1991,frequency=12)
test<-window(transformsouvenirts,start=1992)
fit3<-meanf(train,h=12)
autoplot(window(train,start=1987))+autolayer(fit3,series="mean",PI=FALSE)+xlab("Time")+ylab("sales")+
  ggtitle("Sales forecast using mean")+guides(colour=guide_legend(title = "Forecast"))
accuracy(fit3,test)


train<- window(transformsouvenirts,start=1987,end=1991,frequency=12)
test<-window(transformsouvenirts,start=1992)
fit4<-rwf(train,h=12)
autoplot(window(train,start=1987))+autolayer(fit4,series="naive",PI=FALSE)+xlab("Time")+ylab("sales")+
  ggtitle("Sales forecast using naive")+guides(colour=guide_legend(title = "Forecast"))
accuracy(fit4,test)

plot(transformsouvenirts, col="grey",
     main="Sales Forecast",
     xlab="Year", ylab="Sales")
lines(seasadj(fit),col="red",ylab="Seasonally adjusted")

#estimating trend using moving average 

plot(transformsouvenirts, main="Sales Forecast",
     ylab="Sales", xlab="Year")
lines(ma(transformsouvenirts,5),col="blue")


#ARIMA model
arimatrain<- window(transformsouvenirts,start=1987,end=1991,frequency=12)
arimatest<-window(transformsouvenirts,start=1992)
fit<-auto.arima(arimatrain,seasonal = FALSE)
test_data<-forecast(fit,h=12)
accuracy(test_data,arimatest)
autoplot(test_data)
