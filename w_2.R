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

#########################################################################################################
#obiektowosc
install.packages("sloop")
install.packages("car")
library(sloop)
library(car)

a<-c('a','b','a','b','c','a',1,1,21)
class(a)
attributes(a)
summary(a)
a<-as.factor(a)
class(a)
attributes(a)
summary(a)
summary(unclass(a))

methods(summary)
x<-rpois(5,4)
ef<-ecdf(x)
summary(ef)
#zdefiniowane klasy mozna modyfikowac

s3_dispatch(summary(ef))

s<-list(first=c("abc"), second=1995, third=c(123,321))
summary(s)
class(s)<-append(class(s), 'enum') #dodanie nowej nazwy klasy
summary.enum<-function(x){
  print("this is enum")
  print(paste0(x$first,', Year:', x$second))
  print(x$third)
}
summary(s)

s3_dispatch(summary(s))
attr(s,'mode')
attr(s,'class')
methods(summary)

ftype(summary)
ftype(sum)
body(summary)
body(sum)
methods(sum)

GetFirst<-function(x){
  print("wywolanie funkcji generycznej getfirst")
  UseMethod("GetFirst", x)
}

GetFirst.enum<-function(x){
  print("wywolanie funkcji getfirst dla obiektu enum")
  return(x$first)
}

GetFirst(s)

body(GetFirst)
body(GetFirst.enum)
methods(class = "enum")

body(plot.lm) #metoda jest w innym srodowisku, dlatego nie mamy do niej dostepu
s3_dispatch(mean(x))
s3_dispatch(sum(x))

x<-matrix(1:10, nrow = 2)

s3_dispatch(mean(x))
s3_dispatch(sum(x))

lm1<-lm(speed~., data=cars)
s3_dispatch(plot(lm1))

#tworzenie klasy

daneB<-t(read.csv(file="http://galaxy.uci.agh.edu.pl/~bbasiura/eurostat2016.csv", row.names = 1, sep=";"))
d<-daneB[,c(1,5,8,20,21)]
head(d)
mojaAnaliza<-function(x, metoda){
  stopifnot(is.numeric(x))
  stopifnot(is.matrix(x))
  metoda<-match.arg(metoda, c("spearman","kendall"))
  me<-list(x=x, KP=cor(x), KR=cor(x, method=metoda))
  class(me)<-append(class(me), 'mojaAnaliza')
  return(me)
}

environment(mojaAnaliza)
ma<-mojaAnaliza(d, "spearman")

ma

class(ma)
class(d)
dim(ma)
dim(ma$KP)

d<-daneB[,c(2,4,20,21)]

install.packages("corrplot")
library(corrplot)
corrplot(ma$KP, method = "circle", col = c("blue", "red"))
  
res1 <- cor.mtest(ma$KP, conf.level = .95)
corrplot(ma$KP, p.mat = res1$p, method = "color",
         insig = "label_sig", pch.col = "white")


plot.mojaAnaliza<-function(x){
  dl<-dim(x$KP)[1]
  d1<-rep(seq(1, dl), dl)
  d2<-rep(seq(1, dl), each=dl)
  
  col <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))(200)
  
  odl.cor<-x$KP[upper.tri(x$KP)]
  sig<-sign(x$KP[upper.tri(x$KP)])
  assign.color <- function(dat = odl.cor, color = col) {
    newcorr <- (dat + 1)/2
    newcorr[newcorr <= 0] <- 0
    newcorr[newcorr >= 1] <- 1 - 1e-16
    color[floor(newcorr * length(color)) + 1]
  }
  
  col.fill <- assign.color()
  
  get_color_from_value <- function(value) {
    color = if (value > 0) "#ff0000" else "#0000ff"
    colorRampPalette(c("#ffffff", color))(256)[round(abs(value) * 255) + 1]
  }
  
  symbols(d1, d2, circles = rep(0.05, dl*dl), inches = FALSE, bg=sapply(x$KP, get_color_from_value))
}

plot(ma)
s3_dispatch(plot(ma))
methods(plot)
methods(class="mojaAnaliza")

#jak napisac metode print do tej klasy

#metody akcesorowe
# -------------------------------
# --- metody akcesorowe w klasie ---
# --- wybór danych               ---
d1 <- daneB[,c(2,20,21)]
dim(d1)
class(d1)
is.matrix(d)

# --- set ---

setDane<-function(elOb,value){
  print("Wywołanie funkcji generycznej")
  UseMethod("setDane",elOb)
  print("Brak działania")
}

setDane(ma,d1)

setDane.default<-function(elOb,value){
  print("Wywołanie funkcji domyslnej - nie znam metody")
  return(elOb)
}

setDane(ma,d1)

setDane.mojaAnaliza<-function(elOb,value){
  print("Ustaw wartość dla klasy")
  elOb$x<-value
  elOb$KP<-cor(value)
  elOb$KR<-cor(value,method = "spearman")
  return(elOb)
}

ma<-setDane(ma,d1)
ma
methods(class='mojaAnaliza')

# --- get ---

getDane<-function(elOb,i){
  print("Wywolanie funkcji generycznej")
  UseMethod("getDane", elOb)
  print("Brak działania")
}
getDane(mA,1)
getDane.default<-function(elOb,i){
  print("Funkcja domyslna")
  return(NULL)
}
getDane(mA,1)

getDane.mojaAnaliza<-function(elOb,i){
  print("Dla Klasy pobierz wartość")
  return(elOb[[i]])
}
getDane(mA,2)

mA

# --- wszystko razem --- wraz ze środowiskiem

mojaAnaliza<-function(x, metoda){
  stopifnot(is.numeric(x))
  stopifnot(is.matrix(x))
  metoda <- match.arg(metoda, c("spearman", "kendall"))
  
  thisEnv <- environment()
  x = x
  KP = cor(x)
  KR = cor(x, method = metoda)
  me <- list(
    thisEnv=thisEnv,
    getEnv=function(){
      return(get("thisEnv",thisEnv) ) },
    getDane=function(){
      return(get("x", thisEnv) )
    },
    getMetoda=function(){
      return(get('metoda',thisEnv))
    },
    setDane = function(value){
      return(assign('x',value,thisEnv))
    },
    setKP = function(){
      return(assign('KP',cor(x),thisEnv))
    },
    setKR=function(){
      return(assign("KR",cor(x, method = metoda),thisEnv))
    })
  assign('this',me, envir=thisEnv)
  class(me)<-append(class(me),'mojaAnaliza')
  return(me)
}

daneB<-t(read.csv(file="http://galaxy.uci.agh.edu.pl/~bbasiura/eurostat2016.csv",
                  row.names=1,sep=";"))
d<-daneB[,c(1,5,8,20,21)]
mA<-mojaAnaliza(d,"spearman")
get('x',mA$getEnv())
get('KP',mA$getEnv())
get('KR',mA$getEnv())

d<-daneB[,c(2,4,20:28)]
mA2<-mojaAnaliza(d,"kendal")
get('x',mA$getEnv())
get('KP',mA2$getEnv())
get('KR',mA2$getEnv())

# ------------------------------
# ---    kopiowanie obiektu  ---

mA2<-mA
get('KP',mA$getEnv())
get('KP',mA2$getEnv())

d<-daneB[,c(2,20,21)]
mA2$setDane(d)
mA2$getDane()
mA$getDane()

mA2$setKP()

get('KP',mA2$getEnv())
get('KP',mA$getEnv())

#  --- funkcja generyczna ---
makeCopy<-function(elOb){
  print('Wywołanie funkcji generycznej makeCopy')
  UseMethod("makeCopy",elOb)
  print("Brak działania")
}

makeCopy.default<-function(elOb){
  print('Wywołanie funkcji domyslnej - nie znam OBIEKTU')
  return(elOb)
}

makeCopy.mojaAnaliza<-function(elOb){
  print('Dla klasy mojaAnaliza towrzę kopię')
  newObiect<-mojaAnaliza(
    x = elOb$getDane(),
    metoda= elOb$getMetoda()
  )
}

mA3<-makeCopy(mA)
mA3$getDane()
d<-daneB[,c(2,20,21)]
mA3$setDane(d)
mA3$getDane()
mA$getDane()

mA3$setKP()

get('KP',mA3$getEnv())
get('KP',mA$getEnv())









