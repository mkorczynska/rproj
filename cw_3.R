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
vect_un<-vector(length = length(vect_2))
myNorm<-function(x){
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
  10*sin(1.5*x)*cos(0.5*x)+(1/2)*sqrt(abs(x))
}

#a
ur67<-uniroot(myFun, c(6, 7))
ur12<-uniroot(myFun, c(1, 2))
ur55<-uniroot(myFun, c(-5, 5))

#b
urs33<-uniroot.all(myFun, c(-3,3))

plot(myFun, from = -5, to=10, type = "l", ylab = "f(x)", main="10*sin(1.5*x)*cos(0.5*x)+(1/2)*sqrt(abs(x))", cex.main=0.75)

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
col_names<-tab[1,]
