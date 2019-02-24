x=3
x
x==3; x==4

y=c(x,6)
y

1:10
seq(0,10,length=40)
rep(2,5)
rep(1:2,5)
y=c(1.4,1.6,7.5,3.1,9.5,1.0,3.8,2)
length(y)

y[1]
y[c(1,4,5)]
y[-1]
y<4
y[y<4]

boxplot(y)
help(boxplot)
z=c(2.7,4.3,9.5,1.4,5.5,7.2); boxplot(y,z)
xaxis=seq(0,10,length=30)
xaxis
sin(xaxis)

plot(xaxis,sin(xaxis))
plot(xaxis,sin(xaxis),type="l",xlab="x-axis",ylab="y-axis")

xaxis=seq(0,10,length=300)
plot(xaxis,sin(xaxis),type="l")


2*3-7
2^3
y^3

4*(3:9)

mean(y)
var(y)
sum((y-mean(y))^2)/(length(y)-1)
sort(y)
sample(y)
sample(y)
median(y)

x=rnorm(100)
hist(x,prob=TRUE)
x=rbinom(1,30,0.5)

for (i in 1:10) print(i)
m=numeric(500)
for (i in 1:500) m[i]=mean(rexp(25))
hist(m)
hist(m,prob=TRUE)
u=seq(min(m),max(m),length=100)
lines(u,dnorm(u,mean(m),sqrt(var(m))))

hist(rexp(500))

data=read.table(file="LINREG1.R",header=TRUE)
data
dim(data)
data$run
data[,1]
data[1:5,1]

labels=1:10
labels
sum(labels)
labels=as.factor(labels)
labels
sum(labels)

qnorm(0.95); qnorm(0.975)

x=rnorm(30)
par(mfrow=c(1,2))
hist(x)
qqnorm(x)

hist(10*x+3)
qqnorm(10*x+3)

x=rnorm(10)
hist(x)
qqnorm(x)

x=runif(100)
hist(x)
qqnorm(x)

x=rchisq(30,5)
hist(x)
qqnorm(x)


m=30
n=30
mu=180
nu=175
sd=5
x=rnorm(m,mu,sd)
y=rnorm(n,nu,sd)
t.test(x,y,var.equal=TRUE)
t.test(x,y,var.equal=TRUE)[[3]]

B=1000
p=numeric(B)
for (b in 1:B) {x=rnorm(m,mu,sd)
                y=rnorm(n,nu,sd)
                p[b]=t.test(x,y,var.equal=TRUE)[[3]]}
power=mean(p<0.05) 
 

