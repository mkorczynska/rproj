setwd("C://Users/HP ENVY/Documents/R/Projects/rproj")
setwd("H://R-zaawansowany/lab_R")

install.packages("pracma")
library(pracma)

install.packages("stringr")
library(stringr)

install.packages("quantmod")
library(quantmod)

####Zadanie 1####
market<-read.csv2("market.csv", sep =",", header=T)

#a
amount<-nrow(market)
amount

#b
price<-as.numeric(as.character(market$cena))
volume<-as.numeric(as.character(market$ilosc))
sum_transactions<-sum(price*volume)
sum_transactions

#c
names_table<-table(market[,3])
sort(names_table, decreasing=TRUE)[1:3]

#d
total_table<-aggregate(price ~ nazwa, data = market, sum)
total_df<-as.data.frame(total_table)
total_ordered<-total_df[order(total_df$price),]
tail(total_ordered, 3)
#-------------

####Zadanie 2
#a
crime<-read.delim2(file = 'crim_just_sex.tsv', header=TRUE, na.string=": ")
any(is.na(crime))
sum_nan<-sum(is.na(crime))

#b
first_col<-crime$leg_stat.sex.unit.geo.time

first_col<-str_sub(first_col, -2, -1)
crime_2<-cbind(first_col, crime)
crime_2<-as.data.frame(crime_2)

crime_2$X2017<-as.numeric(as.character(crime_2$X2017))
crime_2$X2016<-as.numeric(as.character(crime_2$X2016))
crime_2$X2015<-as.numeric(as.character(crime_2$X2015))
crime_2$X2014<-as.numeric(as.character(crime_2$X2014))
crime_2$X2013<-as.numeric(as.character(crime_2$X2013))
crime_2$X2012<-as.numeric(as.character(crime_2$X2012))
crime_2$X2011<-as.numeric(as.character(crime_2$X2011))
crime_2$X2010<-as.numeric(as.character(crime_2$X2010))
crime_2$X2009<-as.numeric(as.character(crime_2$X2009))
crime_2$X2008<-as.numeric(as.character(crime_2$X2008))

row_sums<-rowSums(crime_2[,3:12], na.rm = T)
crime_sum<-aggregate(row_sums ~ first_col, data = crime_2, sum)
pl_mean<-crime_sum[33,2]/10
#-------------

####Zadanie 3####
gold<-read.csv2("zloto.csv", sep =";", header=T, dec = ".")
length(gold[,1])

#a
dates_3<-seq(as.Date("2017/02/02"), as.Date("2018/11/20"), length.out = 488)
gold[,1]<-dates_3

#b
head(gold)
tail(gold, 10)

#c
month<-months(gold[,1])
year <- format(gold[,1],format="%y")
aggregate(gold$Zamkniecie~month+year, gold, mean)
#--------------

####Zadanie 4####
wig20<-read.csv2("wig20.csv", sep =";", header=T, dec = ".")

time_series <- ts(wig20$Zamkniecie, start=c(2014, 1), end=c(2018, 12), frequency=12)
show(time_series)
plot(time_series)

moving_average<-movavg(time_series, 6, "s")
moving_avg<-ts(moving_average, start=c(2014, 1), end=c(2018, 12), frequency = 12)
lines(moving_avg, col = "red", type = "l")
grid()
#--------------

####Zadanie 5####
getData<-function(name){
  dane<-read.csv(paste0("https://stooq.pl/q/d/l/?s=", name,
                        "&d1=20171001&d2=20181031&i=d"), head =T, sep=",", dec=".")
}

atm<-getData("ATM")
cdr<-getData("CDR")
cmr<-getData("CMR")
elz<-getData("ELZ")
qmk<-getData("QMK")
sgn<-getData("SGN")

#a
atm_ma<-movavg(atm$Zamkniecie, 12, "s")
cdr_ma<-movavg(cdr$Zamkniecie, 12, "s")

plot(atm_ma, type = "l")
plot(cdr_ma, type = "l")

#b
cor(atm$Zamkniecie, cdr$Zamkniecie)
cor(atm_ma, cdr_ma)
#--------------

####Zadanie 6####
getSymbols(c("^GSPC","FB","^IXIC","AAPL","CSCO"), from="2016-01-02", to="2018-01-02") 

#a
aapl_ma<-movavg(AAPL$AAPL.Close, 12, "s")
csco_ma<-movavg(CSCO$CSCO.Close, 12, "s")

plot(aapl_ma, type = "l")
plot(csco_ma, type = "l")

#b
cor(AAPL$AAPL.Close, CSCO$CSCO.Close)
cor(aapl_ma, csco_ma)
#-------------