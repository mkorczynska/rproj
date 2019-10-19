install.packages("nycflights13")
install.packages("dplyr")
install.packages("VIM")
install.packages("mice")

library(nycflights13)
library(dplyr)
library(VIM)
library(mice)

data("flights")
typeof(flights)
class(flights)
str(flights)
dim(flights)
sum(is.na(flights))
loty_wigilia<-flights[flights$month == 12 & flights$day ==24,]
dim(loty_wigilia)
sum(is.na(loty_wigilia))
head(loty_wigilia[,c("dep_time", "air_time", "origin")])

length(unique(flights$origin))
unique(flights$origin)
length(unique(flights$dest))

loty_wigilia_jfk<-flights[flights$day==24 & flights$month==12 & flights$origin=="JFK",]
loty_wigilia_ewr<-flights[flights$day==24 & flights$month==12 & flights$origin=="EWR",]
loty_wigilia_lga<-flights[flights$day==24 & flights$month==12 & flights$origin=="LGA",]
dim(loty_wigilia_ewr)
dim(loty_wigilia_jfk)

loty_wigilia_jfk_distance<-loty_wigilia_jfk[order(loty_wigilia_jfk$distance),]
loty_wigilia_ewr_distance<-arrange(loty_wigilia_ewr, distance, desc(air_time))

loty_wigilia_ewr_km<-cbind(loty_wigilia_ewr_distance[,13:16],km_distance=loty_wigilia_ewr_distance$distance*1.609)
colnames(loty_wigilia_ewr_km)
head(loty_wigilia_ewr_km)

plot(arr_time~dep_time, data=loty_wigilia_ewr_distance)
marginplot(loty_wigilia_ewr_distance[,c(4,7)], pch=19)

ranking<-loty_wigilia_ewr_km %>%
  group_by(dest) %>%
  mutate(rank=min_rank(desc(air_time)))

unlist(tapply(loty_wigilia_ewr_km$air_time, loty_wigilia_ewr_km$dest, rank))
loty_wigilia_ewr_km_ranking<-cbind(loty_wigilia_ewr_km, ranking[,6])
head(loty_wigilia_ewr_km_ranking)

sort(table(loty_wigilia_ewr_km$dest), na.last = T)

head(airquality, 5)
sum(is.na(airquality))
which(is.na(airquality))
which(complete.cases(airquality))

apply(airquality[,1:2], 2, mean, na.rm=T)
apply(airquality[complete.cases(airquality),1:2],2, mean)
apply(na.omit(airquality[,1:2]),2, mean)
summary(airquality[,1:2])

table(airquality[,1], useNA = "ifany")

model_cc<-lm(Solar.R~Ozone, data=airquality)
summary(model_cc)
mean(model_cc$residuals)

model_omit<-lm(Solar.R~Ozone, data=airquality, na.action=na.omit)
summary(model_omit)
mean(model_omit$residuals)

model_exclude<-lm(Solar.R~Ozone, data=airquality, na.action=na.exclude)
summary(model_exclude)
mean(model_exclude$residuals)

data("airquality")
airquality_cc<-na.omit(airquality)
airq_m<-airquality[1:2]

marginplot(airquality[,c(1,2)])

Ozone_imp<-is.na(airquality[,1])
Solar.R_imp<-is.na(airquality[,2])
airq<-cbind(Ozone_imp=Ozone_imp, Solar.R_imp=Solar.R_imp, Ozone=airq_m$Ozone, SOlar.R=airq_m$Solar.R)

