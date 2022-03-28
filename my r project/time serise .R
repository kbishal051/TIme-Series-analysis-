##Research methodology project 
##A series analysis of Gdp of nepal and forcasting future gdp

#loading library
library(rvest)
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
library(gridExtra)
library(writexl)
#scrapping gdp data from link
#Extracting data from web
link<-"https://www.macrotrends.net/countries/NPL/nepal/gdp-gross-domestic-product"

gdpdata<-read_html(link)%>%html_node('.col-xs-6')%>%html_table()

#tidiying the data 
colnames(gdpdata)<-c("Year","GDPbilliondollor","PerCapita($)",
                     "Growth")
gdpdata<-gdpdata[-1,]
##giving the class for variables 

gdpdata$Year<-as.Date(gdpdata$Year,format ="%Y")
gdpdata$Year<-format(gdpdata$Year,"%Y")
##
gdpdata$GDPbilliondollor<-gsub("\\$","",
                                gdpdata$GDPbilliondollor)
gdpdata$GDPbilliondollor<-gsub("B","",
                                gdpdata$GDPbilliondollor)
gdpdata$GDPbilliondollor<-as.numeric(gdpdata$GDPbilliondollor)
#taking only Yeaer and gdp for time series analyis
gdpdata<-gdpdata[,1:2]
gdpdata<-gdpdata%>%arrange(Year)


###Time series plot of Gdp of nepal 
gdp_tseries<-ts(gdpdata$GDPbilliondollor,
                start = 1960,
                end=2020)

##......................Time series analysis of gdp of nepal.......#
#time series plot

##line plot of actual gdp
ggplot(gdpdata,aes(as.numeric(Year),GDPbilliondollor))+
  geom_line(color="green",size=1)+
  labs(x="Year",y="GDP in Billion dollor",
       title = "GDP of Nepal ",
       subtitle = "from 1960 to 2022")+
  theme_classic()



  
###Discriptive analysis of data 

plot.ts(gdp_tseries)

hist(gdp_tseries,freq = F,
     breaks = 10,main = "histogram of orginal GDP",
     xlab="",col = "green",cex.main=0.8)
lines(density(gdp_tseries),col="blue",lwd=2)


#from line plot and histogram it is clesr that 
#the data is right skewed so log transforming the series 
#befor further analysis 
log_transfromed_series<-log(gdp_tseries)
hist(log_transfromed_series,breaks=10,
     ylim=c(0,0.4),
      freq=F,col="green",
     main="Histogram of Log transformed gdp ",xlab = "",
     cex.main=0.8)
lines(density(log_transfromed_series),col="blue",lwd=2)
#...........checking the data stationary by using dicky fuller test....
adf.test(log_transfromed_series)
#show log trasnfromed series is non stationary 
#diffirencing to make stationary 
#1st order difference 
diff_series<-diff(log_transfromed_series,lag = 1)

plot(diff_series)
#looks preety stationary
#adf test of differenced series

adf.test(diff_series)
###now the series stationary 
#finding the order od AR and MA using auto.arima 
##this code finds best arima model for the log trasnformed gdp data
fit<-auto.arima(gdp_tseries,trace=T,lambda = 0,biasadj = T)

summary(fit)
#ARIMA(0,1,2) with drift is best model 

##attaching the fitted values bay the model to orginal data
gdpdata$fitted<-fit$fitted


#comparision of actual vs fitted
ggplot(gdpdata,aes(as.numeric(Year)))+
  geom_line(aes(y=GDPbilliondollor,color="actual gdp"),size=1)+
  geom_line(aes(y=fitted,color="fitted by model"),size=1)+
   labs(x="year",y="GDP in billion dollor",
       title = "comparision of Actual vs model fitted gdp")+
  theme_classic()

####analysis fo stationarity of residules
adf.test(fit$residuals)
#adf test show residuals are stationary
#visual inspection 
plot.ts(fit$residuals,ylab="Residuals",main="line plot of residuals")
Acf(fit$residuals,main="ACF plot of residual")
Pacf(fit$residuals,main="PACF plot of residuals")
##all the reesidule analysis shows the fitted modeel is appropriate
#forcasting the gdp of nepal to 2030
prediction_raw<-forecast(fit,level = c(95),h=10)
#plotting the forecasted values
autoplot(prediction_raw)+ylab("GDP in Billion dollor")
##saving the forecasted values in table
prediction<-data.frame(seq(2021,2030,by=1),
                       prediction_raw$mean,
                       prediction_raw$upper,
                       prediction_raw$lower)
colnames(prediction)<-c("year","predicted","95%ubound","95%lbound")


