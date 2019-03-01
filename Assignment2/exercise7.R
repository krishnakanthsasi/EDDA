# ----- Exercise 7 -----
# --- Question 1 ---
dogs <- read.table("C:/Users/Tommy/Desktop/dogs.txt",stringsAsFactors = FALSE)
boxplot(as.numeric(dogs[2:11,1]), as.numeric(dogs[2:11,2]), as.numeric(dogs[2:11,3]),
        ylab = "concentration (ng/mm)",
        main = "concentrations of plasma epinephrine",
        names = c("isofluorane", "halothane", "cyclopropane"))

par(mfrow=c(1,3))
qqnorm(as.numeric(dogs[2:11,1]), main = dogs[1,1])
qqnorm(as.numeric(dogs[2:11,2]), main = dogs[1,2])
qqnorm(as.numeric(dogs[2:11,3]), main = dogs[1,3])

# No (might not be). Particularly the plot for isofluorine looks skewed. 

# --- Question 2 ---
dogsframe <- data.frame(concentration=as.numeric(as.matrix(dogs[2:11,])), 
                        substance=factor(c(rep(dogs[1,1],10),rep(dogs[1,2],10),rep(dogs[1,3],10))))
dogsaov=lm(concentration~substance,data=dogsframe)
anova(dogsaov)
summary(dogsaov)

# The p-value is low (0.011), so the null hypothesis would be rejected.
# Estimates:            < remove
# isofluorane: 0.469
# halothane: 0.434
# cyclopropane: 0.853

# --- Question 3 ---
kruskal.test(dogsframe$concentration,dogsframe$substance)

# The p-value is 0.5948, which is larger than 0.05. The null hypothesis would not be rejected.
# The difference in results could indicate that the assumptions for a parametric one-way ANOVA
# test are not met. The populations tested here may be nonnormal, as seen in the Q-Q plots in 
# Question 1, and the sample size is small (n=10). 