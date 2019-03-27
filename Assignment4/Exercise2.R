psiData = read.table("./data/psi.txt", header = TRUE)

psiDataNonFactor = data.frame(psiData)

par(mfrow=c(1,1))
plot(passed ~ gpa, data = psiData)

psiData$passed = ifelse(test=psiData$passed == 1, yes="Pass", no="Fail")
psiData$passed = as.factor(psiData$passed)

psiData$psi = ifelse(test=psiData$psi == 1, yes="Yes", no="No")
psiData$psi = as.factor(psiData$psi)

psiDataCopy = data.frame(psiData)

## 75% of the sample size
smpSize = floor(0.9 * nrow(psiData))

## set the seed to make your partition reproducible
set.seed(123456)
train_ind = sample(seq_len(nrow(psiData)), size = smpSize)

train = psiData[train_ind, ]
test = psiData[-train_ind, ]

str(psiData)

par(mfrow=c(1,2))
hist(psiData$gpa, freq = FALSE)
qqnorm(psiData$gpa)

summary(psiData)

xtabs(~passed + psi, data = psiData)

logRegModel = glm(passed ~ psi + gpa, data = train, family = "binomial")
summ = summary(logRegModel)
summ$coefficients[3, 1]

train
test

predicted = predict(logRegModel, test, type="response")  # predicted scores

predicted

newdat1 = data.frame(gpa=seq(0, 4, len=300))
newdat2 = data.frame(gpa=seq(0, 4, len=300))

newdat1$psi = 1
newdat1$psi = ifelse(test=newdat1$psi == 1, yes="Yes", no="No")
newdat1$psi = as.factor(newdat1$psi)
newdat2$psi = 0
newdat2$psi = ifelse(test=newdat2$psi == 1, yes="Yes", no="No")
newdat2$psi = as.factor(newdat2$psi)

newdat1$passed = predict(logRegModel, newdata=newdat1, type="response")
newdat2$passed = predict(logRegModel, newdata=newdat2, type="response")

par(mfrow=c(1,1))
plot(passed ~ gpa, data = psiDataNonFactor, xlim = c(1.5 ,4))
lines(passed ~ gpa, data = newdat1, col="green4", lwd=2)
lines(passed ~ gpa, data = newdat2, col="red4", lwd=2)
legend(x = "left", legend=c("PSI = YES", "PSI = NO"),
       col=c("green", "red"), lty=1:1, cex=0.8)

logRegModel = glm(passed ~ psi + gpa, data = psiData, family = "binomial")
testSec4 = read.table("./data/psi-section4.txt", header = TRUE)
testSec4$passed = ifelse(test=testSec4$passed == 1, yes="Pass", no="Fail")
testSec4$passed = as.factor(testSec4$passed)

testSec4$psi = ifelse(test=testSec4$psi == 1, yes="Yes", no="No")
testSec4$psi = as.factor(testSec4$psi)

testSec4

predicted = predict(logRegModel, testSec4, type = "response")
predicted

#seciton 5

round(exp(logRegModel$coefficients), 3)
