setwd("C://Users/HP ENVY/Documents/R/Projects/rproj")
setwd("H://R-zaawansowany/lab_R")

####Zadanie 1
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
zloto<-read.csv2("zloto.csv", sep =";", header=T, dec = ".")
length(zloto[,1])
dates_3<-seq(as.Date("2017/02/02"), as.Date("2018/11/20"), length.out = 488)
zloto[,1]<-dates_3

head(zloto)
tail(zloto, 10)

month<-months(zloto[,1])
year <- format(zloto[,1],format="%y")
aggregate(zloto$Zamkniecie~month+year, zloto, mean)
#--------------

####Zadanie 4
wig20<-read.csv2("wig20.csv", sep =";", header=T, dec = ".")
dates_4<-seq(as.Date("2014/01/31"), as.Date("2018/10/31"), by="month")
wig20[,1]<-dates_4
wig<-cbind(wig20$Data, wig20$Zamkniecie)
series<-ts(wig20)
plot(series)
#--------------

####Zadanie 5
getData<-function(name){
  dane<-read.csv(paste0("https://stooq.pl/q/d/l/?s=", name,
                        "&d1=20171001&d2=20181031&i=d"), head =T, sep=",", dec=".")
}
#a
#b
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






