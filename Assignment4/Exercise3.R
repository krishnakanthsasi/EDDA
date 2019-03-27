# 3.1
hist(rpois(10,32))
hist(rpois(100,32))

# Varying n only changes the amount of samples,
# so setting it high is preferable when examining
# the shape of the distribution in the histogram.

par(mfrow=c(1,2))

hist(rpois(10000,.1))
hist(rpois(10000,.5))
hist(rpois(10000,2))
hist(rpois(10000,5))
hist(rpois(10000,10))
hist(rpois(10000,50))
hist(rpois(10000,100))
hist(rpois(10000,1000))

# For larger values of $lambda$, the distribution is similar
# to a normal distribution with the mean and variance both equal
# to lambda.


# 3.2

# In order for the distribution of a randomly distributed variable
# $Y$ to be in a location-scale family as a given random variable $X$, 
# Y must have the same distribution as $a + bX$ for some parameters
# $a$ and $b$ (in other words, <math>Y \stackrel{d}{=} a + b X</math>, 
# where <math>Y \stackrel{d} means 'equal in distribution'. 

# In the case of the Poisson distribution, the distribution
# is both scaled by parameter $\lambda$, since the mean and variance
# are both equal to $\lambda$. Thus, it can be 
# said that, given a variable $Y$ and a variable $X$ that follow 
# a Poisson distribution, <math>Y \stackrel{d}{=} \lambda X</math>, 
# which satisfies the above condition for location-scale families.

# However, for very small values of lambda ($lambda$ $<$ $1$), 
# where the distribution looks less similar to a normal distribution, 
# it may prove difficult to produce Poisson distributions with larger
# $\lambda$ values, as a scaling transformation may not be able to
# fit a normal distribution.

#3.3

africa = read.table("africa.txt",header=TRUE)
africaglm=glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size+numelec+numregim,
              family=poisson,data=africa)

plot(africa)

summary(africaglm)
confint(africaglm)
coef(africaglm)

# Assumption checks:
plot(fitted(africaglm),residuals(africaglm))
plot(log(fitted(africaglm)),residuals(africaglm))
plot(fitted(africaglm),residuals(africaglm,type="response"))

# Performing visual checks on the residuals of the model shows some odd relationships between the
# relationships and the fitted values, as the variance of the residuals doesn't seem to increase 
# for higher fitted values. This is expected under a Poisson distribution, as higher fitted values
# correspond to higher variances as lambda is modeled differently for each observation. The first plot
# also shows some collinearity between variables such as `popn` and `pollib`.

#3.4

summary(glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size+numelec+numregim,
            family=poisson,data=africa))
# `numelec` has the highest p-value, and is removed.
summary(glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size+numregim,
            family=poisson,data=africa))
# `numregim` is removed next.
summary(glm(miltcoup~oligarchy+pollib+parties+pctvote+popn+size,
            family=poisson,data=africa))
# removing `size`
summary(glm(miltcoup~oligarchy+pollib+parties+pctvote+popn,
            family=poisson,data=africa))
# removing `popn`
summary(glm(miltcoup~oligarchy+pollib+parties+pctvote,
            family=poisson,data=africa))
# removing `pctvote`
summary(glm(miltcoup~oligarchy+pollib+parties,
            family=poisson,data=africa))

The remaining parameters appear significant, as their p-value is lower than 0.05.
plot(africa[,1:4])
