library(readxl)
library(dplyr)
library(prophet)
#install.packages("MLmetrics",repos = "https://ftp.fau.de/cran/")
library(MLmetrics)
library(RODBC)
library(tidyverse)
library(forecast)
library(data.table)
library(padr)
library(xgboost)
library(Matrix)
library(RcppRoll)
library(zoo)
#install.packages("Ckmeans.1d.dp",repos = "https://ftp.fau.de/cran/")
library(Ckmeans.1d.dp)

chan <- odbcConnect("exa_di1")


query <- c("select 
           ORDER_DATE
           , COUNTRY_SHIPPING
           , NMV_BEF_DISCOUNT
           , NMV_BEF_COUPON
           , NMV_BEF_CANCELLATION
           , NMV_BEF_RETURN
           , NMV
           from CPS.ACTUALS
           
           where COUNTRY_SHIPPING ='AT' and ORDER_DATE >= '2015-01-01';")

data.raw.rr.backup <- sqlQuery(chan, query, rows_at_time = attr(chan, "rows_at_time"))

#creating back up data frame
data.raw.rr <- data.raw.rr.backup

#aggregating on daily level
data.raw.rr <- aggregate(cbind(NMV_BEF_DISCOUNT, NMV_BEF_COUPON, NMV_BEF_CANCELLATION, NMV_BEF_RETURN, NMV) ~ ORDER_DATE + COUNTRY_SHIPPING,
                         data = data.raw.rr,
                         FUN = sum)

#creating return, discount and coupon variables
data.raw.rr$DR <- 1 - data.raw.rr$NMV_BEF_COUPON / data.raw.rr$NMV_BEF_DISCOUNT
data.raw.rr$CR <- 1 - data.raw.rr$NMV_BEF_CANCELLATION / data.raw.rr$NMV_BEF_COUPON
data.raw.rr$RR <- 1 - data.raw.rr$NMV / data.raw.rr$NMV_BEF_RETURN

#selecting only the variables I want
data.final.rr <- data.raw.rr %>% 
  select(ORDER_DATE, COUNTRY_SHIPPING, RR, DR, CR)
colnames(data.final.rr)[2] <- "COUNTRY"


#Holidays
#Holidays new year ++++++ La befana day new year

#Holidays new year ++++++ La befana day new year

#Holidays new year ++++++ La befana day new year
#Holidays new year ++++++ La befana day new year

AllsaintsDay <- data.frame(holiday='All saints Day',
                           ds = as.Date(c(
                             '2015-11-01',
                             '2016-11-01',
                             '2017-11-01',
                             '2018-11-01',
                             '2019-11-01'
                           )),
                           lower_window = -4, upper_window = 3)









AscensionDay <-  data_frame(holiday = 'AscensionDay',
                            ds = as.Date(c(
                              '2015-05-14',
                              '2016-05-05',
                              '2017-05-25',
                              '2018-05-10',
                              '2019-05-30'
                            )),
                            lower_window = -4, upper_window = 3)


AssumptionDay <-  data_frame(holiday = 'Assumption Day',
                             ds = as.Date(c('2015-08-15',
                                            '2016-08-15',
                                            '2017-08-15',
                                            '2018-08-15',
                                            '2019-08-15')),
                             lower_window = -4, upper_window = 3)







xmas <- data_frame(holiday = 'Xmas',
                   ds = as.Date(c(
                     '2015-12-25', '2016-12-25', '2017-12-25', '2018-12-25', '2019-12-25')),
                   lower_window = -10, upper_window = 10)



CorpusChriti <-  data_frame(holiday = 'Corpus christi',
                            ds = as.Date(c('2015-06-04',
                                           '2016-05-26',
                                           '2017-06-15',
                                           '2018-05-31',
                                           '2019-06-20')),
                            lower_window = -7, upper_window = 7)






easter <- data_frame(holiday = 'Easter',
                     ds = as.Date(c('2015-04-03', '2016-03-25', '2017-04-14', '2018-03-30', '2019-04-01')),
                     lower_window = -16, upper_window = 10)



Epiphany <- data_frame(holiday = 'Epiphany',
                       ds = as.Date(c(
                         '2015-01-06',
                         '2016-01-06',
                         '2017-01-06',
                         '2018-01-06',
                         '2019-01-06'
                       )),
                       lower_window = -4, upper_window = 3)

Immaculate_Conception_Day <-  data_frame(holiday = 'Immaculate Conception Day',
                                         ds = as.Date(c(
                                           '2015-12-08',
                                           '2016-12-08',
                                           '2017-12-08',
                                           '2018-12-08',
                                           '2019-12-08'
                                         )),
                                         lower_window = -4, upper_window = 3)


Labour_Day <-  data_frame(holiday = 'labour Day',
                          ds = as.Date(c(
                            '2015-05-01',
                            '2016-05-01',
                            '2017-05-01',
                            '2018-05-01',
                            '2019-05-01'
                          )),
                          lower_window = -7, upper_window = 7)

National_Day <-  data_frame(holiday = 'National Day',
                            ds = as.Date(c(
                              '2015-10-26',
                              '2016-10-26',
                              '2017-10-26',
                              '2018-10-26',
                              '2019-10-26'
                            )),
                            lower_window = -7, upper_window = 7)


newyear <-data_frame(holiday='New year',
                     ds= as.Date(c('2015-01-01','2016-01-01','2017-01-01','2018-01-01','2019-01-01'))
                     ,lower_window=-7,upper_window=20)


Whit_Monday <-  data_frame(holiday = 'Whit Monday',
                           ds=as.Date(c('2015-05-25'
                                        ,'2016-05-16'
                                        ,'2017-06-05'
                                        ,'2018-05-21'
                                        ,'2019-06-10'
                           )),lower_window=-7,upper_window=7)
















#commercial events
bf <- data_frame(holiday = 'Black Friday',
                 ds = as.Date(c('2015-11-27',
                                '2016-11-25', '2017-11-24', '2018-11-23', '2019-11-29')),
                 lower_window = -21, upper_window = 16)





#DD <- data_frame(holiday = 'daily deals',
#                 ds = as.Date(c('2018-11-18','2019-11-24')),
#                 lower_window = 0, upper_window = 4)

eoss_countdown <- data_frame(holiday = 'Countdown',
                             ds = as.Date(c(
                               "2015-02-06",
                               "2015-07-21",
                               "2016-02-05",
                               "2016-07-22",
                               "2017-01-20",
                               "2017-07-28",
                               "2018-01-26",
                               "2019-01-27",
                               "2019-07-14"
                               
                             )),
                             lower_window = -14, upper_window = 9)



eoss <- data_frame(holiday = 'eoss',
                   ds = as.Date(c( 
                     '2015-01-01',
                     '2015-02-05',
                     '2015-06-14','2015-07-20'
                   )),
                   lower_window = -20, upper_window = 20)




eoss_Premiumpresale <- data_frame(holiday = 'Premium pre sale',
                                  ds = as.Date(c(
                                    "2016-12-17","2019-06-09"                                    )),
                                  lower_window = -7, upper_window = 7)




Premium_sale <- data_frame(holiday = 'Premium Presale',
                           
                           ds = as.Date(c('2017-12-14',
                                          '2018-06-10')),  
                           lower_window = -10, upper_window = 14)







EOSS_AW_1wave <- data_frame(holiday = 'first wave of AW EoSS',
                            ds = as.Date(c(
                              "2015-12-28",
                              "2016-12-25",
                              "2017-12-24",
                              "2018-12-19",
                              "2019-01-05",
                              "2019-12-26")),
                            lower_window = -10, upper_window = 10)


EOSS_AW_2wave <- data_frame(holiday = 'second wave of AW EoSS',
                            ds = as.Date(c(
                              '2016-01-04',
                              '2017-01-01',
                              "2017-12-31",
                              "2019-01-06"
                              
                            )),
                            lower_window = -14, upper_window = 14)


EOSS_AW_3wave <- data_frame(holiday = 'third wave of AW EoSS',
                            ds = as.Date(c("2016-01-17",
                                           "2016-01-31",
                                           "2017-01-15",
                                           "2018-01-14",
                                           "2019-01-20"
                            )),
                            lower_window = -10, upper_window = 10)





#EOSS_AW_4wave <- data_frame(holiday = 'fourth wave of AW EoSS',
#                            ds = as.Date(c('2015-02-11','2016-02-08','2017-02-15', '2018-02-14','2019-02-12')),
#                            lower_window = -14, upper_window = 14)


EOSS_SS_1wave <- data_frame(holiday = 'first wave of SS EoSS',
                            ds = as.Date(c(
                              '2016-06-10',
                              '2017-06-18',
                              '2018-06-17',
                              '2019-06-16'
                            )),
                            lower_window = -14, upper_window = 14)


EOSS_SS_2wave <- data_frame(holiday = 'second wave of SS EoSS',
                            ds = as.Date(c(
                              "2016-06-19",
                              "2016-07-09",
                              
                              "2017-06-25",
                              "2018-06-24",
                              "2019-06-23")),
                            lower_window = -16, upper_window = 16)




EOSS_SS_3wave <- data_frame(holiday = 'third wave of SS EoSS',
                            ds = as.Date(c(
                              
                              '2016-07-10',
                              '2017-07-02',
                              "2017-07-27",
                              
                              '2018-07-01',
                              "2018-07-26",
                              
                              '2019-06-30'
                              ,"2019-07-13"             
                            )),
                            lower_window = -10, upper_window = 10)

#EOSS_SS_4wave <- data_frame(holiday = 'fourth wave of SS EoSS',
#                           ds = as.Date(c('2018-08-13',
#                                           '2018-08-22',
#                                          '2019-08-12',
#                                         '2019-08-18'
#                                         )),
#                         lower_window = -7, upper_window = 7)

MSS_countdown <- data_frame(holiday = 'MSS countdown',
                            ds = as.Date(c(
                              "2017-10-20",
                              "2018-10-19",
                              "2019-04-07",
                              "2019-10-27"
                            )),
                            lower_window = -7, upper_window = 7)


MSS <- data_frame(holiday = 'MSS',
                  ds = as.Date(c(
                    "2015-04-12",
                    "2015-10-11",
                    "2016-04-09",
                    "2017-10-08",
                    "2018-10-07"
                  )),
                  lower_window = -16, upper_window = 16)




#MSS_flashsale <-  data_frame(holiday = 'MSS flash sale',
#                            ds = as.Date(c('2015-05-13',
#                                            '2015-05-20')),
#                            lower_window = -4, upper_window = 4)

MSSPresale <- data_frame(holiday = 'MSS presale',
                         ds = as.Date(c(
                           "2018-04-01"
                         )),
                         lower_window = -7, upper_window = 7)  


#



MSS_Wave1 <- data_frame(holiday = 'MSS wave 1',
                        ds = as.Date(c(
                          
                          "2016-10-09",
                          "2017-04-21",
                          "2018-04-08",
                          "2019-03-24",
                          "2019-10-13"
                        )),
                        lower_window = -10, upper_window = 10)
MSS_Wave2 <- data_frame(holiday = 'MSS wave 2',
                        ds = as.Date(c(
                          
                          '2016-10-16',
                          '2017-04-28',
                          '2018-04-15',
                          '2019-03-31','2019-10-20'
                        )),
                        lower_window = -10, upper_window = 10)
#eoss_countdown_4d <- data_frame(holiday = '4 day Countdown',
#                                ds = as.Date(c('2018-07-20')),
#                                lower_window = -14, upper_window = 10)






wh_sale <- data_frame(holiday = 'warehouse sale',
                      ds = as.Date(c(
                        
                        "2015-02-22",
                        
                        "2015-08-07",
                        
                        "2016-02-19",
                        "2016-08-06"
                        
                      )),
                      lower_window = -14, upper_window = 14)


flashsales <- data_frame(holiday = 'flash sales',
                         ds = as.Date(c(
                           "2018-03-09",
                           "2018-05-15",
                           "2018-07-27",
                           "2018-10-30",
                           "2018-12-13",
                           "2019-01-27",
                           "2019-05-04",
                           "2019-05-22",
                           "2019-07-14"                    )),
                         lower_window = -16, upper_window = 16)

#wh_sale_3d <- data_frame(holiday = '3 day warehouse sale',
#                         ds = as.Date(c('2017-02-16')),
#                         lower_window = -14, upper_window = 9)


voucher_1d <- data_frame(holiday = 'voucher 1d',
                         ds = as.Date(c(
                           "2019-03-24",
                           "2019-06-16"
                           
                           
                           
                           
                         )),
                         lower_window = -14, upper_window = 14)



voucher_2d <- data_frame(holiday = 'voucher 2d',
                         ds = as.Date(c( 
                           '2018-09-17'
                           
                           
                         )),
                         lower_window = -7, upper_window = 7)








holidays <- bind_rows(AllsaintsDay,AscensionDay,AssumptionDay,xmas,CorpusChriti,easter,Epiphany,Immaculate_Conception_Day,Labour_Day,National_Day,newyear,Whit_Monday,bf,eoss_countdown,eoss,eoss_Premiumpresale,Premium_sale,EOSS_AW_1wave,EOSS_AW_2wave,EOSS_AW_3wave,EOSS_SS_1wave,EOSS_SS_2wave,EOSS_SS_3wave,MSS_countdown,MSS,MSSPresale,MSS_Wave1,MSS_Wave2,wh_sale,flashsales,voucher_1d,voucher_2d)







columns_wanted_prophet <- c("ORDER_DATE", "RR")
df <- data.final.rr[, names(data.final.rr) %in% columns_wanted_prophet]
colnames(df)[1] <- "ds"
colnames(df)[2] <- "y"
df$ds <- as.Date(as.character(df$ds), format="%Y-%m-%d")
today <- Sys.Date()
df <- subset(df, df$ds < today - 45)

#adding regressor
summer_sundays <- function(ds) {
  dates <- as.Date(ds)
  month <- as.numeric(format(dates, '%m'))
  as.numeric((weekdays(dates) == "Sunday") & (month > 5 & month < 9))
}
df$summer_sundays <- summer_sundays(df$ds)

summer_saturdays <- function(ds) {
  dates <- as.Date(ds)
  month <- as.numeric(format(dates, '%m'))
  as.numeric((weekdays(dates) == "Saturday") & (month > 5 & month < 9))
}
df$summer_saturdays <- summer_saturdays(df$ds)
plot(df$y~df$ds)
#fitting prophet
m <- prophet(holidays = holidays, 
             yearly.seasonality = TRUE, 
             weekly.seasonality = TRUE, 
             daily.seasonality = FALSE, 
             seasonality.mode = 'multiplicative',
             seasonality.prior.scale = 100, 
             changepoint.prior.scale = 0.001,
             holidays.prior.scale = 100)
m <- add_regressor(m, 'summer_sundays' )
m <- add_regressor(m, 'summer_saturdays')

df <- df[df$ds >= '2015-12-31',] 
m <- fit.prophet(m, df)


#predicting with prophet
FC_period <- as.Date(Sys.Date() + 75,  format="%Y-%m-%d")
forecast_days <- difftime(FC_period,today-45,  units = c("days"))
future <- make_future_dataframe(m, periods = forecast_days)
future$summer_sundays <- summer_sundays(future$ds)
future$summer_saturdays <- summer_saturdays(future$ds)
tail(future)
forecast <- predict(m, future)

#tail(forecast[c('ds', 'yhat')])

#plotting the predicting values and the seasonalities
dyplot.prophet(m,forecast) %>% dygraphs::dyLegend(show="follow") %>% dygraphs::dyEvent(Sys.Date()-45, "Forecast", labelLoc = "bottom")
plot(m, forecast)
prophet_plot_components(m, forecast)

# #cross validation
# df.cv <- cross_validation(m, horizon = 31, initial = 1094, units = 'days')
#
#MAPE(y_pred = exp(df.cv$yhat), y_true = exp(df.cv$y))

#getting only the FC until EOY
#columns_wanted_prophet <- c("ds", "yhat")
#forecast_values_prophet_rr <- forecast[, names(forecast) %in% columns_wanted_prophet]
#forecast_values_prophet_rr <- subset(forecast_values_prophet_rr, forecast_values_prophet_rr$ds >= today -45 & forecast_values_prophet_rr$ds <= FC_period)
#colnames(forecast_values_prophet_rr)[2] <- "RETURN RATE II"
#forecast_values_prophet_rr$ds <- as.Date(forecast_values_prophet_rr$ds)

#Plus 4 (M+2)
df.cv <- cross_validation(m, horizon =129 , initial = 990,period = 30 ,units = 'days')
m_v <- split(df.cv,as.Date(df.cv$cutoff))
#m_v <-df.cv %>% split(as.Date(df.cv$cutoff))
plus4<- data.frame()
library(lubridate)
for (i in 1:length(levels(as.factor(df.cv$cutoff)))) { 
  m_v1 <- as.data.frame(m_v[i],col.names = NULL)
  m_v1$Month <- month(ymd(m_v1$ds))
  plus4 <- rbind(m_v1[m_v1$Month==i,],plus4)
}
plus4 <- plus4[order(plus4$ds),]
plus4 <- plus4[,c(1,3,6)]
colnames(plus4) <-c("Date","Predicted Value","Forecast Date")
#Checking Sequence
plus4$missingflag <- c(1, diff(as.POSIXct(plus4$Date, format="%Y-%M-%D"))) > 1
if(length(plus4[plus4$missingflag ==TRUE,]$Date) >0 ) { stop("Dates not in sequence ")}







#Plus 3 (M+1)
df.cv <- cross_validation(m, horizon =99 , initial = 1020,period = 30 ,units = 'days')
m_v <- split(df.cv,as.Date(df.cv$cutoff))
#m_v <-df.cv %>% split(as.Date(df.cv$cutoff))
plus3<- data.frame()
library(lubridate)
for (i in 1:length(levels(as.factor(df.cv$cutoff)))) { 
  m_v1 <- as.data.frame(m_v[i],col.names = NULL)
  m_v1$Month <- month(ymd(m_v1$ds))
  plus3 <- rbind(m_v1[m_v1$Month==i,],plus3)
}
plus3 <- plus3[order(plus3$ds),]
plus3 <- plus3[,c(1,3,6)]
colnames(plus3) <-c("Date","Predicted Value","Forecast Date")
#Checking Sequence
plus3$missingflag <- c(1, diff(as.POSIXct(plus3$Date, format="%Y-%M-%D"))) > 1
if(length(plus3[plus3$missingflag ==TRUE,]$Date) >0 ) { stop("Dates not in sequence ")}


#Plus 2 (M)
df.cv <- cross_validation(m, horizon =69 , initial = 1050,period = 30 ,units = 'days')
m_v <- split(df.cv,as.Date(df.cv$cutoff))
#m_v <-df.cv %>% split(as.Date(df.cv$cutoff))
plus2<- data.frame()
for (i in 1:length(levels(as.factor(df.cv$cutoff)))) { 
  m_v1 <- as.data.frame(m_v[i],col.names = NULL)
  m_v1$Month <- month(ymd(m_v1$ds))
  plus2 <- rbind(m_v1[m_v1$Month==i,],plus2)
}
plus2 <- plus2[order(plus2$ds),]
plus2 <- plus2[,c(1,3,6)]
colnames(plus2) <-c("Date","Predicted Value","Forecast Date")
#Checking sequence
plus2$missingflag <- c(1, diff(as.POSIXct(plus2$Date, format="%Y-%M-%D"))) > 1
if(length(plus2[plus2$missingflag ==TRUE,]$Date) >0 ) { stop("Dates not in sequence ")}



#Plus 1 M-1
df.cv <- cross_validation(m, horizon =43 , initial = 1080,period = 30 ,units = 'days')
m_v <- split(df.cv,as.Date(df.cv$cutoff))
plus1<- data.frame()
for (i in 1:length(levels(as.factor(df.cv$cutoff)))) { 
  m_v1 <- as.data.frame(m_v[i],col.names = NULL)
  m_v1$Month <- month(ymd(m_v1$ds))
  plus1 <- rbind(m_v1[m_v1$Month==i,],plus1)
}
plus1 <- plus1[order(plus1$ds),]
plus1 <- plus1[,c(1,3,6)]
colnames(plus1) <-c("Date","Predicted Value","Forecast Date")
plus1$missingflag <- c(1, diff(as.POSIXct(plus1$Date, format="%Y-%M-%D"))) > 1
if(length(plus1[plus1$missingflag ==TRUE,]$Date) >0 ) { stop("Dates not in sequence ")}



#prophet RRII values+Actuals
hty <-  m$history
head(hty$y)
rt <- forecast[,c("ds","yhat")]
lf <- hty$y

add.col<-function(df, new.col) {n.row<-dim(df)[1]
length(new.col)<-n.row
cbind(df, new.col)}

aa <- add.col(rt,lf)
colnames(aa) <- c("Date","Predicted Value","Actual Value")

head(aa,1)


########################################################################################################################
#M-1  calculation RRII 
library(dplyr)
plus1 <- plus1[plus1$Date>=as.Date("2019-01-01"),]
main <-  plus1[,c(1,2)]
head(main,1)
library(lubridate)
main$Month <- month(ymd(main$Date))
y1 <- aggregate(main$`Predicted Value`, by = list(main$Month), mean)
colnames(y1) <-  c("Month","Predicted Value")
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
y1$Monat <- mymonths[y1$Month]
m_1 <- y1


#M forecast
plus2 <- plus2[plus2$Date>=as.Date("2019-01-01"),]
main <-  plus2
head(main,1)
library(lubridate)
main$Month <- month(ymd(main$Date))
y1 <- aggregate(main$`Predicted Value`, by = list(main$Month), mean)

colnames(y1) <-  c("Month","Predicted Value")
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
y1$Monat <- mymonths[y1$Month]
m_2 <- y1





#M+1 Forecast
plus3 <- plus3[plus3$Date>=as.Date("2019-01-01"),]
main <-  plus3
head(main,1)
library(lubridate)
main$Month <- month(ymd(main$Date))
y1 <- aggregate(main$`Predicted Value`, by = list(main$Month), mean)

colnames(y1) <-  c("Month","Predicted value")
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
y1$Monat <- mymonths[y1$Month]
m_3 <- y1


#M+2 Forecast
plus4 <- plus4[plus4$Date>=as.Date("2019-01-01"),]
main <-  plus4
head(main,1)
library(lubridate)
main$Month <- month(ymd(main$Date))
y1 <- aggregate(main$`Predicted Value`, by = list(main$Month), mean)
colnames(y1) <-  c("Month","Predicted value")
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
y1$Monat <- mymonths[y1$Month]
m_4 <- y1


####################################################################


#Main
library(dplyr)
aa <-  aa %>% filter(aa$Date >= '2019-01-01')
main <- aa
library(lubridate)
main$Month <- month(ymd(main$Date))
y1 <- aggregate(main$`Predicted Value`, by = list(main$Month), mean)
colnames(y1) <-  c("Month","Predicted Value")
y2 <-  aggregate(main$`Actual Value`,by =list(main$Month),mean)
colnames(y2) <-  c("Months","Actual Value")
y2 <- y2[,2]
y3 <-  cbind(y1,y2)
colnames(y3) <-  c("Month","Predicted Value","Actual Value")
plot(y3$Month,y3$RRII)
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
y3$Monat <- mymonths[y3$Month]
main <- y3
if(sum(is.na(main$`Actual Value`)) ==5){ main <- main[-length(main[,1]),]}


#Merging Main = m_1 ,m_2,m_3,m_4
#main, m_1- RRII, m_2-RRII,m_3-RRII

#merged m_1
mi <-main
mi[mi$Month==m_1$Month,]$`Predicted Value` <- m_1$`Predicted Value`
#Removing aggregation of actuals
mi1 <- mi[,c(1,4,2)]

mi1 <- add.col(mi1,m_2$`Predicted Value`)
colnames(mi1)[4]  <- "M"
mi1 <-add.col(mi1,m_3$`Predicted value`)
colnames(mi1)[5]  <- "M+1"
mi1 <-  add.col(mi1,m_4$`Predicted value`)
colnames(mi1)[6] <-  "M+2"
#Nov- Jan,Feb,March,April,May


library(xlsx)
write.xlsx(mi1,"ATRRII.xlsx",row.names = FALSE)

###################################################################################################################################################