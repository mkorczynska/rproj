setwd("C://Users/HP ENVY/Documents/R/Projects/rproj")
install.packages("stats4")
install.packages("dplyr")

library(dplyr)
library(stats4)

wig20<-read.csv2("wig20_d.csv", sep =";", header=T, dec = ".")

#obliczenie stop
rates <- wig20 %>% select(Data, Zamkniecie) %>% mutate(Stopa = log(Zamkniecie/lag(Zamkniecie, 1)))
rates <- rates[-c(1),]
n <- dim(rates)[1]

#odchylenie, srednia
sd(x)
mean(x)

#ksi opoznione
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

one <- c(1,1)
ksi_all <- c(-1,-1)
ksi_lag_all <- c(-1,-1)

lh <- function(params) {
  x <- rates$Stopa
  
  m1 = params[1]
  s1 = params[2]
  m2 = params[3]
  s2 = params[4]
  p1 = params[5]
  p2 = params[6]
  
  #ksi t|t
  ksi = c(1,0)
  ksi_lag <- c(-1, -1)
  
  p = matrix(data = c(p1, 1-p1, 1-p2, p2), nrow = 2, byrow = T)
  d = c(dnorm(x[1], m1, s1), dnorm(x[1], m2, s2))
  
  bottom <- vector()
  
  for(i in 2:n) {
    ksi_lag <- ksi_lag_f(p, ksi)
    ksi_lag_all <- cbind(ksi_lag_all, ksi_lag)
    ksi <- ksi_f(ksi_lag, d)
    ksi_all <- cbind(ksi_all, ksi)
    bottom <- c(bottom, one%*%(ksi_lag*d))
    d <- c(dnorm(x[i], m1, s1), dnorm(x[i], m2, s2))
  }
  
  sum_logs <- sum(log(bottom))
  return(-sum_logs)
  
}

start = list(m1=mean(x), s1=sd(x), m2=0, s2=sd(x), p1 = 0.9, p2=0.95)

par <- optim(start, lh, method = "Nelder-Mead")

#rysowanie wykresów
#zewnętrznie zdefiniowana macierz p
#inne rozkłady

##################################################################
lik <- function(theta, lnoil, lnng){
  alpha1 <- theta[1]
  alpha2 <- theta[2]
  alpha3 <- theta[3]
  alpha4 <- theta[4]
  alpha5 <- theta[5]
  alpha6 <- theta[6]
  p11 <- 1 / (1 + exp(-theta[7]))
  p22 <- 1 / (1 + exp(-theta[8]))
  dist.1 <- 0
  dist.1 <- (1/(alpha5*sqrt(2*pi)))*exp((-(lnng-alpha1-alpha3*lnoil)^2)/(2*alpha5^2))
  dist.2 <- 0
  dist.2 <- (1/(alpha6*sqrt(2*pi)))*exp((-(lnng-alpha2-alpha4*lnoil)^2)/(2*alpha6^2))
  dist <- cbind(dist.1, dist.2)
  o.v <- c(1,1)
  P <- matrix(c(p11, 1-p11, 1- p22, p22), nrow=2, ncol=2)
  xi.a <- rep(0,2*length(lnoil))
  xi.a <- matrix(xi.a, nrow=(length(lnoil)),ncol=2)
  xi.b <- rep(0,2*length(lnoil))
  xi.b <- matrix(xi.a, nrow=(length(lnoil)),ncol=2)
  model.lik <- rep(0, length(lnoil))
                   xi.a[1,] <- (c(p11,p22)*dist[1,])/(o.v%*%(c(p11,p22)*dist[1,]))
                   ## Here is the Hamilton filter
                   for (i in 1:(length(lnoil)-1)){
                     xi.b[i+1,] <- P%*%xi.a[i,]
                     xi.a[i+1,] <- (xi.b[i+1,]*dist[i+1,])/(o.v%*%(xi.b[i+1,]*dist[i+1,]))
                     model.lik[i+1] <- o.v%*%(xi.b[i+1,]*dist[i+1,])
                     }
                   logl <- sum(log(model.lik[2:length(model.lik)]))
                   return(-logl)
}
lnng<-rates$Stopa
theta.start <- c(-.05, .01, .2, .4, .1, .2, .5, .5)
max.lik.optim <- optim(theta.start, lik, lnoil=lnoil, lnng=lnng, hessian=T)
OI <- solve(max.lik.optim$hessian)
se <- sqrt(diag(OI))
t <- max.lik.optim$par/se
pval <- 2*(1-pt(abs(t), nrow(data)-2))

alpha.hat <- c(max.lik.optim$par[1], max.lik.optim$par[2], max.lik.optim$par[3], max.lik.optim$par[4],max.lik.optim$par[5], max.lik.optim$par[6])
p.0.hat <- c(( 1 / (1 + exp(-max.lik.optim$par[7]))), ( 1 / (1 + exp(-max.lik.optim$par[8]))))
## [...more code similar to above...]
for (i in 1:(length(lnoil)-1)){
  xi.b.hat[i+1,] <- P.hat%*%xi.a.hat[i,]
  xi.a.hat[i+1,] <- (xi.b.hat[i+1,]*dist.hat[i+1,])/(o.v%*%(xi.b.hat[i+1,]*dist.hat[i+1,]))
  model.lik.hat[i+1] <- o.v%*%(xi.b.hat[i+1,]*dist.hat[i+1,])
  }
plot(xi.a.hat[,2], type='l', xlab='Month', ylab='Probability',main='Filtered Probability of Being in Regime 2')

######################################################
install.packages("kableExtra")
library(knitr)
library(kableExtra)
library(dplyr)
theta_v <- data.frame(t(c(2.00,-2.00,1.00,2.00,0.95,0.85)))
names(theta_v) <- c("$\\mu_1$","$\\mu_2$","$\\sigma_1$","$\\sigma_2$","$p_{11}$","$p_{22}$")
kable(theta_v, "html", booktabs = F,escape = F) %>% 
  kable_styling(position = "center")

p11 <- theta_v[1,5] 
p22 <- theta_v[1,6]
P <- matrix(c(p11,1-p22,1-p11,p22),2,2)
P[1,]

set.seed(13)
T_end <- 10^2

s0 <- 1 
st <- function(i) sample(1:2,1,prob = P[i,])

s <- st(s0)
for(t in 2:T_end) {
  s <- c(s,st(s[t-1]))
}
plot(s, pch = 20,cex = 0.5)

P_stat <- Reduce(function(M1,M2) M1%*%M2 ,lapply(1:100,function(x) P ))
P_stat[1,]

mean(s==1)

set.seed(11)
x1 <- rnorm(T_end,theta_v[1,1],theta_v[1,3])
set.seed(17)
x2 <- rnorm(T_end,theta_v[1,2],theta_v[1,4])

t_index <- 1:T_end
x <- rep(0,T_end)
x[s==1] <- x1[s==1]
x[s==2] <- x2[s==2]
plot(x~t_index, pch = 20)
points(x[s == 2]~t_index[s==2],col = 2)

HMM_Lik <- function(theta) {
  mu <- theta[1:2] # means
  sig <- theta[3:4] # volatilities
  p <- t(matrix(c(theta[5],1-theta[6],1-theta[5],theta[6]),2,2)) # transition matrix
  xi <- c(0.5,0.5) # prior about the filter
  
  Xi <- numeric()
  L <- numeric()
  for(t in 1:T_end) {
    phi1 <- dnorm((x[t]-mu[1])/sig[1])/sig[1]
    phi2 <- dnorm((x[t]-mu[2])/sig[2])/sig[2]   
    phi <- c(phi1,phi2)
    xi_next <- t(p)%*%xi
    l_t <- t(phi)%*%xi_next
    L <- c(L,l_t)
    # update the filter for next period
    xi <- c((1/l_t))*(phi*xi_next) # inference prob
    Xi <- rbind(Xi,t(xi))
  }
  LL <- sum(log(L))
  list(Xi = Xi,LL = LL)
}

theta_known <- unlist(theta_v[1,])
Filter <- HMM_Lik(theta_known)$Xi
Filter <- cbind(t_index,Filter)
colnames(Filter) <- c("$t$","$\\xi_{t \\mid t, 1}$","$\\xi_{t \\mid t, 2}$")
rownames(Filter) <- NULL
kable(round(head(Filter),3), "html", booktabs = F,escape = F) %>% 
  kable_styling(position = "center")

all(round(apply(Filter[,-1],1,sum),9) == 1)

plot(Filter[,3]~t_index, type = "l", ylab = expression(xi[2]))
points(Filter[s==2,3]~t_index[s==2],pch = 20, col = 2)

m1 <- m2 <- mean(x,na.rm = T)
s1 <- s2 <- sd(x,na.rm = T)
p1 <- p2 <- 0.5
theta0 <- c(m1,m2,s1,s2,p1,p2)

# set the constraints
ui <- matrix(0,4,6)
ui[,5] <- c(1,0,-1,0)
ui[,6] <- c(0,1,0,-1)

# stack in matrix and vector
A <- t(matrix(ui,ncol= 4))
A <- ui
B <- c(0.01,0.01,-0.99,-0.99)

all(A%*%theta0 >= B)

L.lik <- function(theta) -HMM_Lik(theta)$LL
opt <- constrOptim(theta0,L.lik, NULL,ui = A, ci = B )
opt

plot(opt$par ~ theta_known,pch = 20,cex=2,ylab="MLE",xlab = "True")
abline(a=0,b=1,lty=2)
