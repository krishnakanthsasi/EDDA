---
title: "Assignment 1"
author: "Tommy Maaiveld, ..."
date: "14 February 2019"
---
  
load(file="assign1.RData")

# ----- Exercise 1 -----
# x1
hist(x1,prob=TRUE,xlim=c(8,12),main='Data in x1')
p <- seq(min(x1),max(x1),length=20)
lines(p,dnorm(p,mean(x1),sd(x1)))
qqnorm(x1,main='QQplot of x1')

random_x1 <- rnorm(20,mean(x1),sd(x1))
hist(random_x1,prob=TRUE,xlim=c(8,12),main='Random samples from a normal distribution',xlab='Random Samples')
lines(p,dnorm(p,mean(x1),sd(x1)))
qqnorm(random_x1)

# x2
hist(x2,prob=TRUE,main='Data in x2')
qqnorm(x2,main='QQplot of x2')

random_x2 <- runif(100,min(x2),max(x2))
hist(random_x2,prob=TRUE,main='Random samples from a uniform distribution')
abline(y=mean(random_x2))
qqnorm(random_x2)

# x3
hist(x3,prob=TRUE, main="Data in x3", xlim=c(-4,4),breaks=16)
q <- seq(min(x3),max(x3),length=100)
lines(q,dnorm(q,mean(x3),sd(x3)))
qqnorm(x3)

random_x3 <- rnorm(100,mean(x3),sd(x3))
hist(random_x3,prob=TRUE,main='Random samples from a normal distribution',xlim=c(-4,4),breaks=16)
lines(q,dnorm(q,mean(x3),sd(x3)))
qqnorm(random_x3)

# x4
hist(x4,prob=TRUE,main='Data in x4',xlim=c(0,8),breaks=8)
qqnorm(x4,main='QQplot of x4')

random_x4 <- rnorm(length(x4),mean(x4),sd(x4))
hist(random_x4,prob=TRUE,main='Random samples from a normal distribution',xlim=c(0,8),breaks=8)
qqnorm(random_x4)

# x5
hist(x5,prob=TRUE,main='Data in x5')
qqnorm(x5,main='QQplot of x5')

#random_x5 <- (rnorm(length(x5),mean(sqrt(x5)),sd(sqrt(x5))))^2
random_x5 <- rchisq(length(x5), df=1, ncp = 0)
hist(random_x5,prob=TRUE,main='Random samples from a chi-squared distribution', xlab='Random samples')
qqnorm(random_x5)

# ----- Exercise 2 -----
# 2.1 parameters
mu <- 180
nu <- mu
m <- 30
n <- m
sd <- 10

# 2.2 parameters
mu <- 180
nu <- mu
m <- 30
n <- 30
sd <- 1

# 2.3 parameters
mu <- 180
nu <- 175
m <- 30
n <- m
sd <- 6

# test script Exercise 2
B=1000
p=numeric(B)
for (b in 1:B) {x=rnorm(m,mu,sd)
                y=rnorm(n,nu,sd)
                p[b]=t.test(x,y,var.equal=TRUE)[[3]]}
power5=mean(p<0.05)
power10=mean(p<0.10)

# diagnostics
power5
power10
hist(p, main="Distribution of p-values",prob=TRUE)


# ----- Exercise 3 -----
# 3.1
mu <- 180
m <- 30
n <- m
sd <- 5

#3.2
mu <- 180
m <- 100
n <- m
sd <- 5

#3.3 
mu <- 180
m <- 30
n <- m
sd <- 100

# test script Ex. 3
nu <- seq(175,185,by=0.1)
q <- numeric(length(nu))

for (value in nu) {
  B=1000
  p=numeric(B)
  for (b in 1:B) {x=rnorm(m,mu,sd)
                  y=rnorm(n,value,sd)
                  p[b]=t.test(x,y,var.equal=TRUE)[[3]]}
  power5=mean(p<0.05)
  power10=mean(p<0.10)
  q[value*10-1751] <- power5
}

# diagnostics
plot(nu, q, type='l', col='red', ylab='power',main='T-test power for various values of nu')
lines(nu, q, col='green')
lines(nu, q, col='blue')
plot(nu, q, ylab="power", main="T-test power for 3")
qqnorm(q)
mean(q)