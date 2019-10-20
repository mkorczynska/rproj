setwd("C://Users/HP ENVY/Documents/R/Projects/rproj")
setwd("H://R-zaawansowany/lab_R")

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
#crime<-read.table(file = 'crim_just_sex.tsv', sep = '\t', header = TRUE)
crime<-read.csv2(file = "crim_just_sex_1_Data.csv", sep = ',', header = TRUE)
crime$Value<-as.character(crime$Value)
any(is.na(crime))

#b
geo_value<-aggregate(Value ~ GEO, data = crime, mean) #co zrobic z na?

#c
time_value<-aggregate(Value ~ TIME, data = crime, mean) #co zrobic z na?
hist()

#d
par(mfcol=c(2,1))
hist()
boxplot()
par(mfcol=c(1,1))

#e

#-------------

####Zadanie 3####
gold<-read.csv2("zloto.csv", sep =";", header=T, dec = ".")
length(gold[,1])
dates_3<-seq(as.Date("2017/02/02"), as.Date("2018/11/20"), length.out = 488)
gold[,1]<-dates_3

head(gold)
tail(gold, 10)

month<-months(gold[,1])
year <- format(gold[,1],format="%y")
aggregate(gold$Zamkniecie~month+year, gold, mean)
#--------------

####Zadanie 4
wig20<-read.csv2("wig20.csv", sep =";", header=T, dec = ".")

time_series <- ts(wig20$Zamkniecie, start=c(2014, 1), end=c(2018, 12), frequency=12)
show(time_series)
plot(time_series, xlim=c(0,60))

moving_average<-movavg(time_series, 6, "s")
lines(moving_average, col = "red", type = "l")

install.packages("pracma")
library(pracma)
plot(moving_average, type = "l")
moving_average<-movavg(time_series, 6, "s")
lines(moving_average, col = "red", type = "l")
grid()
lines(time_series, col = "green", type = "l")
#--------------

####Zadanie 5
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

#b
cor(atm$Zamkniecie, cdr$Zamkniecie)
#--------------

####Zadanie 6
install.packages("quantmod")
library(quantmod)

getSymbols(c("^GSPC","FB","^IXIC","AAPL","CSCO"), from="2016-01-02", to="2018-01-02") 

dates_aapl<-index(AAPL)
months_aapl<-months(index(AAPL))
year_aapl <- format(index(AAPL),format="%y")
means_aapl<-aggregate(AAPL.Close~months_aapl+year_aapl, AAPL, mean)

dates_csco<-index(CSCO)
months_csco<-months(index(CSCO))
year_csco <- format(index(CSCO),format="%y")
means_csco<-aggregate(CSCO.Close~months_csco+year_csco, CSCO, mean)

sorted_means_aapl<-means_aapl[order(means_aapl[,2], factor(means_aapl$months_aapl, month.name) ),]

cor(AAPL, CSCO)






