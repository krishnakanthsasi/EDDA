peruvians = read.table("./data/peruvians.txt", header=TRUE)
par(mfrow=c(3,3))

plot(age~migration, peruvians)
plot(weight~migration, peruvians)
plot(length~migration, peruvians)
plot(wrist~migration, peruvians)
plot(systolic~migration, peruvians)
plot(diastolic~migration, peruvians)
# I cannot make out much about the correlation from the scatterplots
par(mfrow=c(1,1))
# Checking normality for migration sample
qqnorm(peruvians$migration,main="Q-Q Plot migration")
#Normality is not evident for migration sample, hence we use Spearman's correlation test to check for dependence between the variables

print(cor.test(peruvians$age, peruvians$migration, method = "spearman"))
# Moderate correlation observed

print(cor.test(peruvians$weight, peruvians$migration, method = "spearman"))
# Moderate correlation observed

print(cor.test(peruvians$length, peruvians$migration, method = "spearman"))
# Insignificant correlation

print(cor.test(peruvians$wrist, peruvians$migration, method = "spearman"))
# Weak correlation observed

print(cor.test(peruvians$systolic, peruvians$migration, method = "spearman"))
# Weak but inverse correlation observed

print(cor.test(peruvians$diastolic, peruvians$migration, method = "spearman"))
# Insignificant correlation observed
