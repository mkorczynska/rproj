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
myFun<-function(x){
  y=10*sin(1.5*x)*cos(0.5*x)+(1/2)*sqrt(abs(x))
  return(y)
}

myFun(1)
