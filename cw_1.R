setwd("C://Users/HP ENVY/Documents/R/Projects/rproj")

####Zadanie 1
dane<-read.csv2("market.csv", sep =",", header=T)
quantity<-nrow(dane)

sum(dane[,5])

cena<-as.numeric(dane$cena)
ilosc<-as.numeric(dane$cena)
suma<-sum(cena*ilosc)

tab<-table(dane[,3])
tab_df<-as.data.frame(tab)
sort(tab, decreasing=TRUE)[1:3]

#d
#-------------

####Zadanie 2
crime_data<-read.table(file = 'crim_just_sex.tsv', sep = '\t', header = TRUE)
any(is.na(crime_data))

#b
#c
#d
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
wig20$Data<-dates_4
wig<-cbind(wig20$Data, wig20$Zamkniecie)
series<-ts(wig20)
plot(series)
#--------------

####Zadanie 5
#a
#b
#--------------

####Zadanie 6
install.packages("quantmod")
library(quantmod)

getSymbols(c("^GSPC","FB","^IXIC","AAPL","CSCO"), from="2016-01-02", to="2018-01-02") 
month<-months(zloto[,1])
year <- format(zloto[,1],format="%y")
aggregate(zloto$Zamkniecie~month+year, zloto, mean)

cor(AAPL, CSCO)






