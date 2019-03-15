#Study info
# a single experimental unit = 1 slice of bread
# number of experimental units = 18 
# explanatory factors(factor levels) : Temperature(3) , Humidity(2)

bread = as.character(c(1:18))
temperatures = c("warm", "intermediate", "cold")
humidity = c("dry", "wet")

# Randomization of the bread loafs

design = cbind(data.frame("slice" = bread), data.frame("temperatures" = sample(rep(temperatures, each=6))), data.frame("humidity" = sample(rep(humidity, each=9))))
print(design)

# Analysis of the given data
study_data = read.table(file = "C:\\Users\\krish\\Documents\\EDDA\\Assignment3\\data\\bread.txt", header = TRUE)
# box plots of time with factors (2)
boxplot(hours~environment, data = study_data, xlab = "Temperature", ylab = "Hours to decay", main = "Box plot of hours vs temperature")
boxplot(hours~humidity, data = study_data, xlab = "Humidity", ylab = "Hours to decay", main = "Box plot of hours vs humidity")
# interaction plot of time with factors (2)
interaction.plot(study_data$environment, study_data$humidity, study_data$hours, trace.label = "Humidity", xlab = "Temperature", ylab = "Mean of hours to decay ", main = "Interaction between hours and temperature")
interaction.plot(study_data$humidity, study_data$environment, study_data$hours, trace.label = "Temperature", xlab = "Humidity", ylab = "Mean of hours to decay ", main = "Interaction between hours and humidity")
#analysis of variance
study_data$environment = as.factor(study_data$environment)
study_data$humidity = as.factor((study_data$humidity))
study_data_aov = lm(hours~humidity*environment, data=study_data)
# results of aov
print(anova(study_data_aov))
# As can be seen P value of interaction component is < 0.05. So the interaction is significant.
# Which means that the relationship between environment and decay hours depends on the humidity.

# Finding which of the two factor has more effect on the decay hours
# Estimation of the main effects might not be relevant because of significant interaction between the factors
# You should be able to ignore the interaction effects for an additive model generation 
# and estimation, which isn't possible in this case. Because of the interaction, one of the factors might be
# augumenting the effects of another, thereby skewing the results.


#Diagnostics
qqnorm(residuals(study_data_aov), main = "Normal Q-Q plot of the residuals of the AOV test")
#Normality is very likely. Two of the outliers seem too extreme though, removing them might give better results.
plot(fitted(study_data_aov),residuals(study_data_aov))
# The residual spread seems homogenous except for two fitted points around 230-240. Removing the these outliers 
# would produce better results. 

# There are two outliers.


