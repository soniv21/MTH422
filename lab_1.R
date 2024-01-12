## 1 ##

dat1 <- rbinom(n = 50, size = 1, prob = 0.6)
dat2 <- rbinom(n = 10, size = 1, prob = 0.6)
dat3 <- rpois(n = 50, lambda = 1.5)
dat4 <- rnorm( n= 50, mean = 0, sd = 2)
dat5 <- runif(n = 50, min = -1, max = 1)
dat6 <- rbeta(n = 50, shape1 = 2, shape = 1)
dat7 <- rgamma(n = 50, shape = 2, rate = 0.5)
dat8 <- 1/rgamma(n = 50, shape = 2, rate = 0.5)
 
## 2 ##
X <- runif(50)
Y <- rep(NA, 50)
Y[X<0.2]  <- 1
Y[X>0.2 & X<0.6] = 2
Y[X>0.6 & X<0.9] = 3
Y[X>0.9] = 4
Y
sum(Y==1)
sum(Y==2) 
sum(Y==3) 
sum(Y==4) 


## 3 ##
X <- rbinom(n = 50,size = 1, prob = 0.6)
Y[X==0] <- rgamma(n = sum(X==0) , shape = 2, rate = 0.5)
Y[x==1] <- 1/rgamma(n = sum(X==1), shape = 2, rate = 0.5)

## 4 ##
Y <- rbeta(n = 50, shape = 2, rate = 4)
X <- rbinom(n = 10, size = 1, prob = Y)

## 5 ##
library(mvtnorm)
data <- rmvnorm(n = 50, mean = c(2,2), sd = matrix(c(1,0.5,0.5,1),2,  2)
data  

### 6 ###
x <- runif(500)
y <- runif(500)
data <- rbind(x,y)
plot(x,y, pch = 16)

### 7 ###
cnt <- 0
n = 5000
z = NULL
while(cnt<n){
  x <- runif(2, -1, 1)
  if(sum(x^2)  <= 1){
    z <- rbind(z,x)
    cnt = cnt +1
  }
}
z
head(z)

# another solution
radius <- runif(5000,0,1)
theta <- 2*pi*runif(5000,0,1)
x <- sqrt(radius)*cos(theta)
y <- sqrt(radius)*sin(theta)
plot(x,y)

### 8 ###
integral <- integrate(function(x) {exp(-x^4)}, lower = -Inf, upper = Inf)$value
c <- 1/integral
c

### 9 ###
integral1 <- integrate(function(x) {exp(-x^4)}, lower = -Inf, upper = Inf)$value
integral2 <- integrate(function(y) {exp(-y^3)} , lower = 0, upper = Inf)$value
C <- 1/(integral1 * integral2)
C
  
### 10 ###
c <- 1/integral2(function(x,y){-x^4 + x^2*y^2 -y^4},xmin=-50,xmax=50,ymiin=-50,ymax=50)$Q
c