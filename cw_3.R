#zadanie 1
los<-sample(c(1:4000),100,replace=TRUE)
minmaxK<-function(x,K)
{
  sortedx<-sort(x)
  kmin<-head(sortedx,K)
  kmax<-tail(sortedx,K)
  lista<-list(kmin,kmax)
  return(lista)
}
minmaxK(los,5)


#zadanie 2
lDsknl<-function(x){
  vect<-vector(length = x)
  a=1
  for(i in 1:(x-1)){
    if(x%%i==0){
      vect[a]=i
      a=a+1
    }
    else{
      a=a
    }
  }
  if(sum(vect)==x){
    result<-x
  }
  else{
    result=NULL
  }
  
  return(result)
}
lDsknl(27)

zakr<-10000
vect_d<-vector(length = zakr)
j=1
system.time(for(i in 2:zakr){
  if(!is.null(lDsknl(i))){
    vect_d[j]=i
    j=j+1
  }
  else{
    j=j
  }
})


#zadanie 3
vect_2<-c(1,2,3,4)

myNorm<-function(x){
  vect_un<-vector(length = length(x))
  for(i in 1:length(x)){
    result=(x[i]-mean(x))/(max(x)-min(x))
    vect_un[i]=result
  }
  
  return(vect_un)
}
myNorm(vect_2)


#zadanie 4
myCorr<-function(x, y){
  if(length(x)==length(y)){
    list( pearson<-cor(x, y, method = "pearson"),
          kendall<-cor(x, y, method = "kendall"),
          spearman<-cor(x, y, method = "spearman")
          )
  }
  else{
    print("blad")
  }
  
}

x<-runif(100, min = 0, max = 5)
e<-rnorm(100)

y<-x+e

corrs<-myCorr(x, y)

#zadanie 5
myStats<-function(x, p){
  if(p==0 || p==1){
    if(p==0){
      sr<-mean(x)
      odch_std<-sd(x)
      wynik<-c(sr, odch_std)
    }
    else{
      med<-median(x)
      med_odch_bez<-mad(x)
      wynik<-c(med, med_odch_bez)
    }
  }
  else{
    print("bledna wartosc")
  }
  
  return(wynik)
}

myStats(los, 0)


#zadanie 6
install.packages("rootSolve")
library(rootSolve)
myFun=function(x){
  10*sin(1.5*x)*cos(0.5*x^3)+(1/2)*sqrt(abs(x))
}

#a
ur67<-uniroot(myFun, c(6, 7))
ur12<-uniroot(myFun, c(1, 2))
ur55<-uniroot(myFun, c(-5, 5))

#b
urs33<-uniroot.all(myFun, c(-3,3))

urs44<-uniroot.all(myFun, c(-4,4))
plot(myFun, from = -4, to=4, type = "l", ylab = "f(x)", main="10*sin(1.5*x)*cos(0.5*x^3)+(1/2)*sqrt(abs(x))", cex.main=0.75)
points(urs44, y=replicate(21, 0), col="red", pch=19)

#zadanie 7
myLin<-function(x){
  c(F1 = 2*x[1] + x[2] + 2,
    F2 = x[1] + 2 + x[2] - 2 + x[3] - 1,
    F3 = 2 * x[1] + x[2] - x[3] + 3 )
}

solution_Lin<- multiroot(myLin, c(0, 0, 0))

#zadanie 8
myNonLin<-function(x){
  c(F1 = 2*x[1] + x[2]^2 -2*x[3] - 2,
    F2 = x[1]^2 + 2*x[2] - 2*x[3] - 3,
    F3 = 2*x[1] + x[2] - x[3] - 3 )
}

solution_nonLin<- multiroot(myNonLin, c(0, 0, 0), useFortran = FALSE)

#zadanie 9
install.packages("httr")
install.packages("XML")
library(httr)
library(XML)
myDane<-function(url){
  r <- GET(url)
  doc <- readHTMLTable(
    doc=content(r, "text"))
}

tab<-myDane("https://pl.wikipedia.org/wiki/Najwi%C4%99ksze_przedsi%C4%99biorstwa_%C5%9Bwiata")
tab<-as.data.frame(tab)
tab<-tab[,-c(1:2)]
col_names<-c("miejsce", "nazwa", "branża", "przychód", "rok", "kapitalizacja", "zatrudnienie", "symbol", "siedziba", "prezes")
colnames(tab)<-col_names
tab<-tab[-c(1),]

ch<-as.character(tab[,4])
ch<-gsub(" ","",ch)
ch<-gsub("\\[\\d+\\]",'',ch)
ch<-gsub("\\,",'.',ch)
col_4<-as.numeric(ch)

ch<-as.character(tab[,6])
ch<-gsub(" ","",ch)
ch<-gsub("\\[\\d+\\]",'',ch)
ch<-gsub("\\,",'.',ch)
col_6<-as.numeric(ch)

ch<-as.character(tab[,7])
ch<-gsub(" ","",ch)
ch<-gsub("[$[:punct:]]\\d",'',ch)
ch<-gsub("[[:punct:]]",'',ch)
col_7<-as.numeric(ch)

sum(is.na(col_7))
cols<-cbind(col_4, col_6, col_7)

cols_omit_na<-na.omit(cols)

cols_omit_na[,1]<-myNorm(cols_omit_na[,1])
cols_omit_na[,2]<-myNorm(cols_omit_na[,2])
cols_omit_na[,3]<-myNorm(cols_omit_na[,3])
