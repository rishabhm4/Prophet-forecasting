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
           
           where COUNTRY_SHIPPING ='BE' and ORDER_DATE >= '2015-01-01';")

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

Allsaintday <- data_frame(holiday = 'Allsaintday',
                          ds = as.Date(c('2015-11-01',
                                         '2016-11-01',
                                         '2017-11-01',
                                         '2018-11-01',
                                         '2019-11-01')),
                          lower_window = -7, upper_window = 7)

ArmisticeDay <- data_frame(holiday = 'Armistice Day',
                           ds = as.Date(c('2015-11-11',
                                          '2016-11-11',
                                          '2017-11-11',
                                          '2018-11-11',
                                          '2019-11-11')),
                           lower_window = -2, upper_window = 2)


AscensionDay <-  data_frame(holiday = 'AscensionDay',
                            ds = as.Date(c('2015-05-14',
                                           '2016-05-05',
                                           '2017-05-25',
                                           '2018-05-10',
                                           '2019-05-30')),
                            lower_window = -4, upper_window = 3)




AssumptionDay <-data_frame(holiday = 'Assumption Day',
                           ds =as.Date(c('2015-08-15',
                                         '2016-08-15'
                                         ,'2017-08-15',
                                         '2018-08-15'
                                         ,'2019-08-15'
                           ))
                           
                           ,lower_window=-4, upper_window=4)




xmas <- data_frame(holiday = 'Xmas',
                   ds = as.Date(c('2015-12-25', '2016-12-25', '2017-12-25', '2018-12-25', '2019-12-25')),
                   lower_window = -10, upper_window = 10)




easter <- data_frame(holiday = 'Easter',
                     ds = as.Date(c('2015-04-03', '2016-03-25', '2017-04-14', '2018-03-30', '2019-04-19')),
                     lower_window = -16, upper_window = 10)

Independence_day <- data_frame(holiday = 'Independence day',
                               ds = as.Date(c('2015-07-21',
                                              '2016-07-21',
                                              '2017-07-21',
                                              '2018-07-21',
                                              '2019-07-21')),
                               lower_window = -4, upper_window = 4)


labourday <- data_frame(holiday = 'labour day',
                        ds = as.Date(c('2015-05-01','2016-05-01','2017-05-01',
                                       '2018-05-01','2019-05-01')),
                        lower_window = -10, upper_window = 10)



newyear <-data_frame(holiday='New year',
                     ds= as.Date(c('2015-01-01','2016-01-01','2017-01-01','2018-01-01','2019-01-01'))
                     ,lower_window=-7,upper_window=20)

Pentecost_Monday <-  data_frame(holiday = 'Pentecost Monday',
                                ds=as.Date(c('2015-05-25'
                                             ,'2016-05-16'
                                             ,'2017-06-05'
                                             ,'2018-05-21'
                                             ,'2019-06-10'
                                )),lower_window=-7,upper_window=7)








#commercial events
bf <- data_frame(holiday = 'Black Friday',
                 ds = as.Date(c('2015-11-30',
                                '2016-11-25', '2017-11-24', '2018-11-23', '2019-11-29')),
                 lower_window = -21, upper_window = 16)





#DD <- data_frame(holiday = 'daily deals',
#                 ds = as.Date(c('2018-11-18','2019-11-24')),
#                 lower_window = 0, upper_window = 4)

eoss_countdown <- data_frame(holiday = 'Countdown',
                             ds = as.Date(c('2015-01-29',
                                            "2015-07-29",
                                            '2016-01-29',
                                            '2016-07-29',
                                            '2017-01-27',
                                            '2017-07-28',
                                            '2018-01-29',
                                            '2019-01-29',
                                            '2019-07-29'
                             )),
                             lower_window = -14, upper_window = 9)

eoss_Premiumpresale <- data_frame(holiday = 'Premium pre sale',
                                  ds = as.Date(c('2015-06-30',
                                                 '2016-01-01',
                                                 '2017-12-20',
                                                 '2018-01-02',
                                                 '2018-06-15',
                                                 '2018-06-29',
                                                 '2018-12-20',
                                                 "2019-01-02",
                                                 '2019-06-16',
                                                 '2019-06-30',
                                                 '2019-12-18',
                                                 '2019-12-31'
                                  )),
                                  lower_window = -10, upper_window = 10)




#Premium_sale <- data_frame(holiday = 'Premium Presale',

#                           ds = as.Date(c('2018-06-18',
#                                         '2018-12-16',
#                                         '2019-06-17')     ),  
#                           lower_window = -10, upper_window = 14)







EOSS_AW_1wave <- data_frame(holiday = 'first wave of AW EoSS',
                            ds = as.Date(c('2015-01-03',
                                           '2015-01-11',
                                           '2016-01-02',
                                           '2016-01-16',
                                           '2017-01-03',
                                           '2017-01-12',
                                           '2018-01-03',
                                           "2018-01-14",
                                           '2019-01-03'
                            )),
                            lower_window = -10, upper_window = 10)


EOSS_AW_2wave <- data_frame(holiday = 'second wave of AW EoSS',
                            ds = as.Date(c('2015-02-12',
                                           '2016-01-17',
                                           '2016-01-28',
                                           '2017-01-13',
                                           '2017-01-26',
                                           '2018-01-15',
                                           '2018-01-24',
                                           
                                           '2019-01-10')),
                            lower_window = -14, upper_window = 14)


EOSS_AW_3wave <- data_frame(holiday = 'third wave of AW EoSS',
                            ds = as.Date(c("2015-01-19",
                                           "2015-01-28",
                                           "2018-01-25",
                                           "2019-01-17",
                                           "2019-01-28")),
                            lower_window = -10, upper_window = 10)





#EOSS_AW_4wave <- data_frame(holiday = 'fourth wave of AW EoSS',
#                            ds = as.Date(c('2015-02-11','2016-02-08','2017-02-15', '2018-02-14','2019-02-12')),
#                            lower_window = -14, upper_window = 14)


EOSS_SS_1wave <- data_frame(holiday = 'first wave of SS EoSS',
                            ds = as.Date(c('2015-07-01',
                                           '2015-07-11',
                                           
                                           
                                           '2016-07-01',
                                           '2016-07-14',
                                           
                                           
                                           '2017-07-01',
                                           '2018-06-30',
                                           '2018-07-08',
                                           '2019-07-01',
                                           '2019-07-11')),
                            lower_window = -14, upper_window = 14)


EOSS_SS_2wave <- data_frame(holiday = 'second wave of SS EoSS',
                            ds = as.Date(c('2015-07-12',
                                           '2015-07-28',
                                           '2016-07-15',
                                           '2016-07-28',
                                           '2017-07-07',
                                           '2017-07-20',
                                           '2018-07-09',
                                           '2018-07-17',
                                           '2019-07-12',
                                           '2019-07-18')),
                            lower_window = -16, upper_window = 16)



EOSS_SS_3wave <- data_frame(holiday = 'third wave of SS EoSS',
                            ds = as.Date(c('2017-07-21',
                                           '2018-07-18',
                                           '2018-07-31',
                                           '2019-07-19',
                                           
                                           '2019-07-28')),
                            lower_window = -11, upper_window = 11)

#EOSS_SS_4wave <- data_frame(holiday = 'fourth wave of SS EoSS',
#                           ds = as.Date(c('2018-08-13',
#                                           '2018-08-22',
#                                          '2019-08-12',
#                                         '2019-08-18'
#                                         )),
#                         lower_window = -7, upper_window = 7)

MSS_countdown <- data_frame(holiday = 'MSS countdown',
                            ds = as.Date(c( '2015-05-15',
                                            '2016-04-29',
                                            '2016-10-28',
                                            "2017-10-26",
                                            "2019-04-12",
                                            "2019-10-18")),
                            lower_window = -7, upper_window = 7)


MSS <- data_frame(holiday = 'MSS',
                  ds = as.Date(c("2015-05-03",
                                 "2015-05-14",
                                 "2015-10-10",
                                 "2015-10-25",
                                 '2016-04-17',
                                 '2016-04-28',
                                 '2016-10-18',
                                 '2016-10-27',
                                 '2017-04-22',
                                 '2017-05-09',
                                 '2018-10-07',
                                 '2018-10-16',
                                 '2018-11-03')),
                  lower_window = -16, upper_window = 16)




#MSS_flashsale <-  data_frame(holiday = 'MSS flash sale',
#                            ds = as.Date(c('2015-05-13',
#                                            '2015-05-20')),
#                            lower_window = -4, upper_window = 4)

MSSPresale <- data_frame(holiday = 'MSS presale',
                         ds = as.Date(c('2015-05-01',
                                        '2017-10-09',
                                        '2018-04-02')),
                         lower_window = -7, upper_window = 7)  


#



MSS_Wave1 <- data_frame(holiday = 'MSS wave 1',
                        ds = as.Date(c('2017-04-21',
                                       '2017-10-12',
                                       '2018-04-06',
                                       '2018-04-15',
                                       '2019-03-31',
                                       '2019-04-07',
                                       '2019-10-06'
                                       ,'2019-10-12'
                        )),
                        lower_window = -10, upper_window = 10)
MSS_Wave2 <- data_frame(holiday = 'MSS wave 2',
                        ds = as.Date(c('2017-05-03',
                                       '2017-10-16',
                                       '2017-10-25',
                                       '2018-04-16',
                                       '2018-04-22',
                                       '2019-04-08',
                                       '2019-10-13')),
                        lower_window = -10, upper_window = 10)
#eoss_countdown_4d <- data_frame(holiday = '4 day Countdown',
#                                ds = as.Date(c('2018-07-20')),
#                                lower_window = -14, upper_window = 10)






wh_sale <- data_frame(holiday = 'warehouse sale',
                      ds = as.Date(c('2015-02-15',
                                     '2015-08-01',
                                     '2015-08-16',
                                     '2016-02-01',
                                     '2016-02-21',
                                     '2016-08-12',
                                     '2016-08-21',
                                     '2017-02-10',
                                     '2017-08-07',
                                     '2017-12-11',
                                     '2018-02-08',
                                     '2018-02-18',
                                     '2018-08-09',
                                     '2018-08-26',
                                     '2019-02-08',
                                     '2019-02-17',
                                     '2019-03-15',
                                     '2019-08-16'
                      )),
                      lower_window = -14, upper_window = 14)


flashsales <- data_frame(holiday = 'flash sales',
                         ds = as.Date(c(
                           
                           '2015-01-11',
                           '2015-01-26',
                           '2015-02-21',
                           '2015-04-25',
                           '2015-07-10',
                           '2015-10-10',
                           '2015-10-25',
                           '2016-01-07',
                           '2016-04-22',
                           '2016-05-27',
                           '2016-07-08',
                           '2016-08-31',
                           '2016-09-15',
                           '2016-09-25',
                           '2016-10-21',
                           '2016-12-07',
                           '2017-01-20',
                           '2017-03-02',
                           '2017-05-05',
                           '2017-08-14',
                           '2017-08-27',
                           '2017-10-16',
                           '2017-10-22',
                           '2018-01-11',
                           '2018-01-30',
                           '2018-02-21',
                           '2018-03-10',
                           "2018-03-25",
                           "2018-05-14",
                           "2018-12-12",
                           "2019-01-13",
                           '2019-05-17',
                           '2019-07-06',
                           '2019-07-21',
                           '2019-09-01'  )),
                         lower_window = -14, upper_window = 14)

#wh_sale_3d <- data_frame(holiday = '3 day warehouse sale',
#                         ds = as.Date(c('2017-02-16')),
#                         lower_window = -14, upper_window = 9)
voucher_1d <- data_frame(holiday = 'voucher 1d',
                         ds = as.Date(c('2015-12-13',
                                        '2016-08-26',
                                        '2016-12-27',
                                        '2017-04-28',
                                        '2017-06-23',
                                        "2017-07-21",
                                        '2017-11-03',
                                        '2017-12-08',
                                        '2019-07-29','2019-10-18'
                         )),
                         lower_window = -14, upper_window = 14)



voucher_2d <- data_frame(holiday = 'voucher 2d',
                         ds = as.Date(c( '2016-09-23',
                                         '2018-05-30',
                                         '2018-06-01',
                                         '2018-09-28'
                         )),
                         lower_window = -7, upper_window = 7)




holidays <- bind_rows(Allsaintday,ArmisticeDay,AscensionDay,AssumptionDay,xmas,easter,Independence_day,labourday,newyear,Pentecost_Monday,bf,eoss_countdown,eoss_Premiumpresale,EOSS_AW_1wave,EOSS_AW_2wave,EOSS_AW_3wave,EOSS_SS_1wave,EOSS_SS_2wave,EOSS_SS_3wave,MSS_countdown,MSS,MSS_Wave1,MSS_Wave2,wh_sale,flashsales,voucher_1d,voucher_2d)


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
             holidays.prior.scale = 90)
m <- add_regressor(m, 'summer_sundays' ,prior.scale = 0.05)
m <- add_regressor(m, 'summer_saturdays',prior.scale = 0.05)

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

tail(forecast[c('ds', 'yhat')])

#plotting the predicting values and the seasonalities
dyplot.prophet(m,forecast)
plot(m, forecast)
prophet_plot_components(m, forecast)

# #cross validation
# df.cv <- cross_validation(m, horizon = 31, initial = 1094, units = 'days')


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



#Plus 1 Month(M-1)
df.cv <- cross_validation(m, horizon =48 , initial = 1090,period = 30 ,units = 'days')
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
query = c("SELECT FC_FROM_DATE,CDATE,  FC_GROSS_SALES,FC_CANCELLATION_RATE_I
          FROM CPS.CPS_DAILY_FC 
          where COUNTRY_SHIPPING='FI' 
          and FC_FROM_DATE >= '2018-12-01' order by FC_FROM_DATE,CDATE;")

dr<- sqlQuery(chan, query, rows_at_time = attr(chan, "rows_at_time"))

head(dr)




dx <- dr
#Will Give forecasted value from every first monday +2 months
#install.packages("RcppBDT")
library(RcppBDT)
dates <- data.frame(mon=c(1:12),  year= rep(2019,12))
Mond <-  sapply(1:12, function(i)format(getNthDayOfWeek(first, Mon, dates[i,1], dates[i,2])))
library(lubridate)
dx$Month <- month(ymd(dx$FC_FROM_DATE))
i=1
x2<-  data.frame()  
while(i <= 12){
  #Date ext. for Forecast date.. if no date then break
  op <- head(dx[dx$Month==i,]$FC_FROM_DATE,1)
  if(length(op)==0){
    break
  }
  else{  x1 <- dx %>% filter(dx$FC_FROM_DATE==op & dx$CDATE < Mond[i+1] )
  x2 <- rbind(x2,x1)
  i=i+1
  }
}
f_gbc<- x2

dd <- dx %>% filter(dx$FC_FROM_DATE == tail(f_gbc$FC_FROM_DATE,1))
dd <- dd %>% filter(dd$CDATE <= head(dd$CDATE,1) + 90)

f_gbc <- f_gbc %>% filter(f_gbc$FC_FROM_DATE < dd$FC_FROM_DATE)
fgbc <-  rbind(f_gbc,dd)



head(fgbc)
fgbc$FC_GMV_BEF_RET <- mapply('*',1-as.numeric(as.character(fgbc$FC_CANCELLATION_RATE_I)),as.numeric(as.character(fgbc$FC_GROSS_SALES))) 
f_gbc <- fgbc

#M-1 Monthly calculation RRII 
library(dplyr)
fgbc2 <-  filter(f_gbc,f_gbc$CDATE >=as.Date(head(plus1$Date,1)) & f_gbc$CDATE <= as.Date(tail(plus1$Date,1))) 
plus1 <- plus1[plus1$Date>=head(fgbc2$CDATE,1),]
fgbc2 <-  f_gbc %>% filter(f_gbc$CDATE >=as.Date(head(plus1$Date,1)) & f_gbc$CDATE <= as.Date(tail(plus1$Date,1))) 

#plus1$missingflag <- c(1, diff(as.POSIXct(plus1$Date, format="%Y-%M-%D"))) > 1
#if(length(plus1[plus1$missingflag ==TRUE,]$Date) >0 ) { print("Dates not in sequence ")}

length(fgbc2$CDATE)==length(plus1$Date)

#fgbc2$FC_GMV_BEF_RET <- mapply('*',1-as.numeric(as.character(fgbc2$FC_CANCELLATION_RATE_I)),as.numeric(as.character(fgbc2$FC_GROSS_SALES))) 
main <-  data.frame(plus1$Date,plus1$`Predicted Value`,fgbc2$CDATE,fgbc2$FC_GMV_BEF_RET)

head(main,1)
colnames(main) <-  c("Date","PV","Date1","Forecast_GMV_Bef_Return")
main <- main[,-3]
main$revenue_loss <-  mapply('*',as.numeric(as.character(main$PV)),as.numeric(as.character(main$Forecast_GMV_Bef_Return)))
library(lubridate)
main$Month <- month(ymd(main$Date))
y1 <- aggregate(main$revenue_loss, by = list(main$Month), sum)
colnames(y1) <-  c("Month","Revenue Loss")
y2 <-  aggregate(main$Forecast_GMV_Bef_Return,by =list(main$Month),sum)
colnames(y2) <-  c("Months","GMV_Bef_Ret")
y2 <- y2[,2]
y3 <-  cbind(y1,y2)
colnames(y3) <-  c("Month","Revenue Loss","GMV_Bef_Ret")
y3$RRII <- y3$`Revenue Loss`/y3$GMV_Bef_Ret
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
y3$Monat <- mymonths[y3$Month]
m_1 <- y3


#M forecast
#fgbc2 <-  filter(f_gbc,f_gbc$CDATE >=as.Date(head(plus2$Date,1)) & f_gbc$CDATE <= as.Date(tail(plus2$Date,1))) 
plus2 <- plus2[plus2$Date>=head(fgbc2$CDATE,1),]
#fgbc2 <-  f_gbc %>% filter(f_gbc$CDATE >=as.Date(head(plus1$Date,1)) & f_gbc$CDATE <= as.Date(tail(plus1$Date,1))) 
length(fgbc2$CDATE)==length(plus2$Date)
#fgbc2$FC_GMV_BEF_RET <- mapply('*',1-as.numeric(as.character(fgbc2$FC_CANCELLATION_RATE_I)),as.numeric(as.character(fgbc2$FC_GROSS_SALES))) 
main <-  data.frame(plus2$Date,plus2$`Predicted Value`,fgbc2$CDATE,fgbc2$FC_GMV_BEF_RET)
head(main,1)
colnames(main) <-  c("Date","PV","Date1","Forecast_GMV_Bef_Return")
main <- main[,-3]
main$revenue_loss <-  mapply('*',as.numeric(as.character(main$PV)),as.numeric(as.character(main$Forecast_GMV_Bef_Return)))
library(lubridate)
main$Month <- month(ymd(main$Date))
y1 <- aggregate(main$revenue_loss, by = list(main$Month), sum)
colnames(y1) <-  c("Month","Revenue Loss")
y2 <-  aggregate(main$Forecast_GMV_Bef_Return,by =list(main$Month),sum)
colnames(y2) <-  c("Months","GMV_Bef_Ret")
y2 <- y2[,2]
y3 <-  cbind(y1,y2)
colnames(y3) <-  c("Month","Revenue Loss","GMV_Bef_Ret")
y3$RRII <- y3$`Revenue Loss`/y3$GMV_Bef_Ret
plot(y3$Month,y3$RRII)
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
y3$Monat <- mymonths[y3$Month]
m_2 <- y3





#M+1 Forecast
#fgbc2 <- filter(f_gbc,f_gbc$CDATE >=as.Date(head(plus3$Date,1)) & f_gbc$CDATE <= as.Date(tail(plus3$Date,1))) 
plus3 <- plus3[plus3$Date>=head(fgbc2$CDATE,1),]
#fgbc2 <-  f_gbc %>% filter(f_gbc$CDATE >=as.Date(head(plus1$Date,1)) & f_gbc$CDATE <= as.Date(tail(plus1$Date,1))) 
length(fgbc2$CDATE)==length(plus3$Date)
#fgbc2$FC_GMV_BEF_RET <- mapply('*',1-as.numeric(as.character(fgbc2$FC_CANCELLATION_RATE_I)),as.numeric(as.character(fgbc2$FC_GROSS_SALES))) 
main <-  data.frame(plus3$Date,plus3$`Predicted Value`,fgbc2$CDATE,fgbc2$FC_GMV_BEF_RET)
head(main,1)
colnames(main) <-  c("Date","PV","Date1","Forecast_GMV_Bef_Return")
main <- main[,-3]
main$revenue_loss <-  mapply('*',as.numeric(as.character(main$PV)),as.numeric(as.character(main$Forecast_GMV_Bef_Return)))
library(lubridate)
main$Month <- month(ymd(main$Date))
y1 <- aggregate(main$revenue_loss, by = list(main$Month), sum)
colnames(y1) <-  c("Month","Revenue Loss")
y2 <-  aggregate(main$Forecast_GMV_Bef_Return,by =list(main$Month),sum)
colnames(y2) <-  c("Months","GMV_Bef_Ret")
y2 <- y2[,2]
y3 <-  cbind(y1,y2)
colnames(y3) <-  c("Month","Revenue Loss","GMV_Bef_Ret")
y3$RRII <- y3$`Revenue Loss`/y3$GMV_Bef_Ret
#plot(y3$Month,y3$RRII)
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
y3$Monat <- mymonths[y3$Month]
m_3 <- y3


#M+2 Forecast
plus4 <- plus4[plus4$Date>=head(fgbc2$CDATE,1),]
#fgbc2 <-  f_gbc %>% filter(f_gbc$CDATE >=as.Date(head(plus1$Date,1)) & f_gbc$CDATE <= as.Date(tail(plus1$Date,1))) 
length(fgbc2$CDATE)==length(plus4$Date)
#fgbc2$FC_GMV_BEF_RET <- mapply('*',1-as.numeric(as.character(fgbc2$FC_CANCELLATION_RATE_I)),as.numeric(as.character(fgbc2$FC_GROSS_SALES))) 
main <-  data.frame(plus4$Date,plus4$`Predicted Value`,fgbc2$CDATE,fgbc2$FC_GMV_BEF_RET)
head(main,1)
colnames(main) <-  c("Date","PV","Date1","Forecast_GMV_Bef_Return")
main <- main[,-3]
main$revenue_loss <-  mapply('*',as.numeric(as.character(main$PV)),as.numeric(as.character(main$Forecast_GMV_Bef_Return)))
library(lubridate)
main$Month <- month(ymd(main$Date))
y1 <- aggregate(main$revenue_loss, by = list(main$Month), sum)
colnames(y1) <-  c("Month","Revenue Loss")
y2 <-  aggregate(main$Forecast_GMV_Bef_Return,by =list(main$Month),sum)
colnames(y2) <-  c("Months","GMV_Bef_Ret")
y2 <- y2[,2]
y3 <-  cbind(y1,y2)
colnames(y3) <-  c("Month","Revenue Loss","GMV_Bef_Ret")
y3$RRII <- y3$`Revenue Loss`/y3$GMV_Bef_Ret
#plot(y3$Month,y3$RRII)
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
y3$Monat <- mymonths[y3$Month]
m_4 <- y3




#Main
library(dplyr)
aa <-  aa %>% filter(aa$Date >= '2019-01-01' )
f_gbc1 <- f_gbc %>% filter(f_gbc$CDATE >=head(aa$Date,1) & f_gbc$CDATE <= tail(aa$Date,1)) 
aa <-  filter(aa,aa$Date >= head(f_gbc1$CDATE,1) & aa$Date <= tail(f_gbc1$CDATE,1) )


f_gbc1$missingflag <- c(1, diff(as.POSIXct(f_gbc1$CDATE, format="%Y-%M-%D"))) > 1
if(length(f_gbc1[f_gbc1$missingflag ==TRUE,]$CDATE) >0 ) { print("Dates not in sequence ")}


length(f_gbc1$CDATE) ==length(aa$Date)


##calculating Revenue loss

main <-  data.frame(aa$Date,aa$`Predicted Value`,aa$`Actual Value`,f_gbc1$CDATE,f_gbc1$FC_GMV_BEF_RET)
head(main,1)
colnames(main) <-  c("Date","PV","AV","Date1","Forecast_GMV_Bef_Return")
main <- main[,-4]

main$revenue_loss <-  mapply('*',as.numeric(as.character(main$PV)),as.numeric(as.character(main$Forecast_GMV_Bef_Return)))
#Sum revenue loss & gmv bef return by month and divide it for getting RRII
#collate the data with M-1,M,M+1
library(lubridate)
main$Month <- month(ymd(main$Date))
y1 <- aggregate(main$revenue_loss, by = list(main$Month), sum)
colnames(y1) <-  c("Month","Revenue Loss")
y2 <-  aggregate(main$Forecast_GMV_Bef_Return,by =list(main$Month),sum)
colnames(y2) <-  c("Months","GMV_Bef_Ret")
y11 <-  aggregate(main$AV,by =list(main$Month),mean)
colnames(y11) <-  c("Months","Actual Value")


y3 <-  cbind(y1,y2[,2],y11[,2])
colnames(y3) <-  c("Month","Revenue Loss","GMV_Bef_Ret","Actual Value")
y3$RRII <- y3$`Revenue Loss`/y3$GMV_Bef_Ret
plot(y3$Month,y3$RRII)
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
y3$Monat <- mymonths[y3$Month]
main <- y3
if(sum(is.na(main$`Actual Value`)) ==5){ main <- main[-length(main[,1]),]}


write.xlsx(main,"BE1.xlsx",row.names = F)
#Final forecast
mi <-main
mi[mi$Month==m_1$Month,]$RRII <- m_1$RRII
mi1 <- mi[,c(1,6,5)]
mi1 <- add.col(mi1,m_2$RRII)
colnames(mi1)[4]  <- "M"
mi1 <-add.col(mi1,m_3$RRII)
colnames(mi1)[5]  <- "M+1"
mi1 <- add.col(mi1,m_4$RRII)
colnames(mi1)[6] <-  "M+2"
#m_sub1(Dec-Mar),m_sub0(Jan-Apr),m_plus(Feb-May),main(Jan)

#Merging the MSTR Actuals with the forecast
library(mstrio)
conn <- connect_mstr(base_url="https://micro.zalando.net/MicroStrategyLibrary/api", username="rkanojia", password="Username30", project_name = "LIVE_ZALANDO_COMMON",login_mode=16)
rr2  <- get_report(connection = conn, report_id = "1076451B11E9DF9FE6980080EFD59E73")
rr2 <-  rr2[rr2$`Shop Country`=='BE',]

colnames(rr2)
mi1$Year <- "2019"
mi1$'Shop Country' <- "BE"
mi1 <- mi1[,c(7,6,1,3,4,5,2)]

rr21 <- rr2[rr2$Year %in% c("2019"), ] 
rr22 <- rr2[rr2$Year %in% c("2017","2018"), ]

mi11 <-  cbind(mi1,a1$`Return Rate II`)

#################################################################
length(new.col)<-n.row
cbind(df, new.col)}

mi11 <- add.col(mi1[,c(4,5,6)],rr21)
rr22$RRII <- 0
rr22$'RRII+M2' <-0 
rr22$'RRII+M3' <-0
colnames(mi11)
mi12 <- rbind(rr22,mi11)

#########################################################################

library(xlsx)
write.xlsx(mi11,"BERRII.xlsx",row.names = FALSE)

###################################################################################################################################################