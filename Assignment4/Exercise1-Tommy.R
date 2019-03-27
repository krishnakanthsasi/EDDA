#1.1
fruitflies = read.table(file="data/fruitflies.txt", header=TRUE)
fruitflies = cbind(fruitflies, log(fruitflies[,2]))
names(fruitflies)[4] = "loglongevity"

#1.2
attach(fruitflies)
plot(loglongevity~thorax,pch=as.character(activity))

# The plot shows a linear correlation between thoractic length (`thorax`) and log longevity. It seems to indicate 
# that flies with the `activity` factor set to `high` live shorter than those with `low`, which in turn score lower
# than those with `isolated`, assuming equal thoractic length between specimens. The perceived implication is that
# fruitflies that are increasingly sexually active tend to have increasingly shorter lifespans.

#1.3 
fruitfliesaov=lm(loglongevity~activity, data=fruitflies)
anova(fruitfliesaov)

# According to this analysis, `activity` seems likely to have an effect on `loglongevity`,
# since the p-value $< 0.05$ (`r anova(fruitfliesaov)$"Pr(>F)"`)

#1.4

summary(fruitfliesaov)
attach(fruitfliesaov)

# interpretation!!!

# the estimates for each level of the factor `activity` are `r fruitfliesaov$coef[1]`
# for fruitflies of level `high`, `r fruitfliesaov$coef[1]+fruitfliesaov$coef[2]`
# for fruitflies of level `isolated`, and `r fruifliesaov$coef[1]+fruitfliesaov$coef[3]`
# for fruitflies of level `low`.