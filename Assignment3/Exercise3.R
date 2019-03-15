#study info
# Single experimental unit :
# Number of experimental units :
# Explanatory factors (levels) : batch, position, starter  
# Response variable : acidity

study_data1 = read.table(file = "C:\\Users\\krish\\Documents\\EDDA\\Assignment3\\data\\cream.txt", header = TRUE)

# 3 way Anova
study_data1$batch = as.factor(study_data1$batch)
study_data1$position = as.factor(study_data1$position)
study_data1$starter = as.factor(study_data1$starter)
study_data1_aov = lm(acidity~starter+batch+position, data=study_data1)
# results of aov
anova(study_data1_aov)
print(summary(study_data1_aov))

#Diagnostics
qqnorm(residuals(study_data1_aov), main = "Normal Q-Q plot of the residuals of the AOV test")

# Effects of starter 2 and 4 seem statistically significant here. 

# mean comparisons
library(multcomp)
data_aov = lm(acidity~starter+batch+position, data = study_data1)
mean_comparison = glht(data_aov, linfct =mcp(starter="Tukey"))
print(summary(mean_comparison))

# As can be seen from the results, starter 4 seems to give significant different from other starters
# whereas none of the other starters seems to have a significant pairwise difference in their means
# This seems to show that only starter 4 has a significant main effect on the lactation of the yogurt.

# Showing the table of confidence interval for mean diff of main effects of starter with 95% confidence
print(confint(mean_comparison))

# From the result, we can see that 4-1, 4-2, 4-3,  and 4-5 do not contain zero, and henceforth
# has main effect on the process with 95% confidence.