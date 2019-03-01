peruvians = read.table("C:/Users/krish/Documents/EDDA/Assignment2/data/peruvians.txt", header=TRUE)
par(mfrow=c(3,3))
plot(age~migration)
plot(weight~migration)
plot(length~migration)
plot(wrist~migration)
plot(systolic~migration)
plot(diastolic~migration)
# I cannot make out much about the correlation from the scatterplots
par(mfrow=c(1,1))
# Checking normality for migration sample
qqnorm(migration,main="Q-Q Plot migration")
#Normality is not evident for migration sample, hence we use Spearman's correlation test to check for dependence between the variables

print(cor.test(age, migration, method = "spearman"))
# Moderate correlation observed

print(cor.test(weight, migration, method = "spearman"))
# Moderate correlation observed

print(cor.test(length, migration, method = "spearman"))
# Insignificant correlation

print(cor.test(wrist, migration, method = "spearman"))
# Weak correlation observed

print(cor.test(systolic, migration, method = "spearman"))
# Weak but inverse correlation observed

print(cor.test(diastolic, migration, method = "spearman"))
# Insignificant correlation observed
