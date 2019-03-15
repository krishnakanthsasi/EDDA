library(lme4)
# check variable prints, generating new results may cause problems (bootstrap)

cow <- read.table("C:/Users/Tommy/Desktop/EDDA/EDDA - Assignment 3/cow.txt")

#4.1
cow$id <- factor(cow$id)
cowlm <- lm(milk~treatment+per+id,data=cow)
anova(cowlm)
print("The p-value for treatment is 0.75. Therefore, this model seems to 
       indicate that the feed treatment does not affect the volume of 
       milk produced.")

#4.2
summary(cowlm)
print("treatmentB   -0.2444     1.0895  -0.224 0.828096")

# The milk yield of treatment B is estimated 0.51 lower
# than that of treatment A.

#4.3
cowlmer <- lmer(milk~treatment+order+per+(1|id), data=cow, REML=FALSE)
summary(cowlmer)

cowlmer1 <- lmer(milk~order+per+(1|id), data=cow, REML=FALSE)
anova(cowlmer1,cowlmer)

print("By performing an ANOVA test between a linear model fitted including
      the treatment factor to one not including the treatment factor, a p-value
      of 0.45 is obtained. There is still no reason to reject the null
      hypothesis, but the result is different from that in 4.1. The results 
      under'Fixed effects' are identical to those obtained in 4.2.")

#4.4
attach(cow)
t.test(milk[treatment=="A"],milk[treatment=="B"],paired=TRUE)

# Similar test:
aovcow <- lm(milk~treatment+id,data=cow)
anova(aovcow)
summary(aovcow)

print("Performing a paired t-test yields an equivalent result to a repeated
      measures experiment where excheangability is assumed. Its result is
      incompatible with that of 4.1, since that test does not assume there
      are no time effects, learning effects or dissimilar subjects affecting
      results. In this experiment, these assumptions do not seem safe, meaning
      a crossover design is more appealing. This paired t-test does not produce
      a valid test for difference in milk production.")