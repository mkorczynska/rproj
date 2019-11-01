setwd("C://Users/HP ENVY/Documents/R/Projects/rproj")
install.packages("stats4")
install.packages("dplyr")

library(dplyr)
library(stats4)

wig20<-read.csv2("wig.csv", sep =";", header=T, dec = ".")

rates <- wig20 %>% select(Data, Zamkniecie) %>% mutate(Stopa = log(Zamkniecie/lag(Zamkniecie, 1)))
rates$Data <- as.Date(rates$Data)
rates <- rates[-c(1),]
rates <- rates[1758:n,]
n <- dim(rates)[1]

sd(x)
mean(x)

ksi_lag_f <- function (p, ksi){
  ksi_lag = t(p)%*%ksi
  return(ksi_lag)
}

ksi_f <- function (ksi_lag, d){
  bottom <- one%*%(ksi_lag*d)
  top <- ksi_lag*d
  
  ksi = c(top[1]/bottom, top[2]/bottom) 
  return(ksi)
}

#dens <- function (x, m, s){
#  return(1/sqrt(2*pi*s*s)*exp(-(x-m)^2/w*s))
#}
x <- rates$Stopa
one <- c(1,1)

#różne rozkłady https://www.stat.umn.edu/geyer/old/5101/rlook.html
dens <- function(x, dist, ...){
  args <- list(...)
  dens <- NULL
  switch(dist,
         normal = {dens <- dnorm(x, args$m, args$s)},
         student = {dens <- dt(x, args$df)},
         chisquare = {dens <- dchisq(x, args$df)}
         )
  return(dens)
}

lh <- function(params, return_ksi = F) {
  
  m1 = params[1]
  s1 = params[2]
  m2 = params[3]
  s2 = params[4]
  p1 = params[5]
  p2 = params[6]
  
  #ksi t|t
  ksi = c(1,0)
  ksi_lag <- c(-1, -1)
  ksi_all <- ksi
  ksi_lag_all <- ksi_lag
  
  p = matrix(data = c(p1, 1-p1, 1-p2, p2), nrow = 2, byrow = T)
  d = c(dens(x[1], dist = "normal", m = m1, s = s1), 
        dens(x[1], dist = "normal", m = m2, s = s2))
  
  bottom <- vector()
  
  for(i in 2:n) {
    ksi_lag <- ksi_lag_f(p, ksi)
    ksi_lag_all <- cbind(ksi_lag_all, ksi_lag)
    ksi <- ksi_f(ksi_lag, d)
    ksi_all <- cbind(ksi_all, ksi)
    bottom <- c(bottom, one%*%(ksi_lag*d))
    d = c(dens(x[i], dist = "normal", m = m1, s = s1), 
          dens(x[i], dist = "normal", m = m2, s = s2))
  }
  
  sum_logs <- sum(log(bottom))
  
  if(return_ksi){
    return(list(ksi_all, ksi_lag_all))
  } else {return(-sum_logs)}

}

start = list(m1=mean(x), s1=sd(x), m2=0, s2=sd(x), p1 = 0.9, p2=0.95)

par <- optim(start, lh, method = "Nelder-Mead")

#rysowanie wykresów
ksi_matrices <- lh(par$par, return_ksi = T)

ksi_all <- ksi_matrices[[1]]
ksi_lag_all <- (ksi_matrices[[2]])[,-1]

install.packages("ggplot2")
library(ggplot2)

plot_data <- data.frame(time = as.Date(rates$Data), first = ksi_all[1,], second = ksi_all[2,])
#plot_data <- data.frame(time = 100:500, first = ksi_lag_all[1,100:500], second = ksi_lag_all[2,100:500])

par(mfrow = c(2,1))
plot(plot_data$time, plot_data$first, type = "l")
plot(as.Date(rates$Data), rates$Zamkniecie, type = "l")

ggplot(plot_data, aes(time)) +
  geom_line(aes(y = second))

ggplot(rates, aes(as.Date(rates$Data))) +
  geom_line(aes(y = Zamkniecie))

ksi_smooth_all <- ksi_all[,1]
p = matrix(data = c(par$par[5], 1-par$par[5], 1-par$par[6], par$par[6]), nrow = 2, byrow = T)

for(t in (n-1):1){
  ksi_smooth <- ksi_all[,t]*(p%*%(ksi_lag_all[,n-1]/ksi_lag_all[,t]))
  ksi_smooth_all <- cbind(ksi_smooth_all, ksi_smooth)
}

ksi_smooth_all <- rbind(rev(ksi_smooth_all[1,]), rev(ksi_smooth_all[2,]))


plot_data_2 <- data.frame(time = as.Date(rates$Data), first = ksi_smooth_all[1,], second = ksi_smooth_all[2,])
par(mfrow = c(3,1))
plot(plot_data$time, plot_data$first, type = "l")
plot(as.Date(rates$Data), rates$Zamkniecie, type = "l")
plot(plot_data_2$time, plot_data_2$first, type = "l")

#zewnętrznie zdefiniowana macierz p
#inne rozkłady
