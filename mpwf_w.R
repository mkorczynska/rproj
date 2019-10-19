setwd("C://Users/HP ENVY/Documents/R/Projects/rproj")
install.packages("stats4")
install.packages("dplyr")

library(dplyr)

wig20<-read.csv2("wig20_d.csv", sep =";", header=T, dec = ".")

rates <- wig20 %>% select(Data, Zamkniecie) %>% mutate(Stopa = log(Zamkniecie/lag(Zamkniecie, 1)))
rates <- rates[-c(1),]

ksi_lag <- function (p, ksi){
  ksi_lag = t(p)%*%ksi
}

ksi <- function (ksi_lag, d){
  ksi = ksi_lag%*%d / t(1)*(ksi_lag%*%d)
}ksi

dens <- function (x, m, s){
  return(1/sqrt(2*pi*s*s)*exp(-(x-m)^2/w*s))
}

lh <- function(m1, s1, m2, s2, p1, p2) {
  x <- rates$Stopa
  ksi = matrix()
  
  #ksi t|t
  ksi1 = c(1,0)
  p = matrix(data = c(p1, 1-p1, 1-p2, p2), nrow = 2, byrow = T)
  d = c(dnorm(x[1], m1, s1), dnorm(x[1], m2, s2))
  
  ksi_result <- vector()
  
  #for(i = 1)
}

?rnorm
