####Zadanie 1
dane<-read.csv2("C://Users/HP ENVY/Documents/R/Projects/rproj/market.csv", sep =",", header=T)
show(dane)
ilosc<-nrow(dane)
suma<-sum(dane[,5])

tab<-table(dane[,3])
tab_df<-as.data.frame(tab)
sort(tab, decreasing=TRUE)[1:3]

#agg = aggregate(dane, by = list(dane$nazwa, dane$cena), FUN=mean)
tab<-table(dane[,3])
tab_df<-as.data.frame(tab)
#-------------

####Zadanie 2
crime_data<-read.table(file = 'H://R-zaawansowany/lab_R/crim_just_sex.tsv', sep = '\t', header = TRUE)
crime_data[crime_data==":"]<-NA
crime_data[which(crime_data[,2]==";")]<-""
#-------------

####Zadanie 3
zloto<-read.csv2("H://R-zaawansowany/lab_R/zloto.csv", sep =";", header=T)
daty<-seq(as.Date("2017/02/02"), as.Date("2018/11/20"), by="day")
zloto[,1]<-daty
head(zloto)
tail(zloto, 10)
#--------------

####Zadanie 4
wig20<-read.csv2("H://R-zaawansowany/lab_R/wig20.csv", sep =";", header=T)
wig20<-cbind(wig20$Data, wig20$Zamkniecie)
szereg<-ts(wig20)
plot(szereg)
#--------------

####Zadanie 5

#--------------

####Zadanie 6
install.packages("quantmod")
library(quantmod)

getSymbols(c("^GSPC","FB","^IXIC","AAPL","CSCO"), from="2016-01-02", to="2018-01-02") 








