psiData = read.table("./data/psi.txt", header = TRUE)

psiData$passed = ifelse(test=psiData$passed == 0, yes="Pass", no="Fail")
psiData$passed = as.factor(psiData$passed)

psiData$psi = ifelse(test=psiData$psi == 0, yes="Yes", no="No")
psiData$psi = as.factor(psiData$psi)

## 75% of the sample size
smpSize = floor(0.8 * nrow(psiData))

## set the seed to make your partition reproducible
set.seed(12345)
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
summ$coefficients[1, 1]

train
test

predicted = predict(logRegModel, test, type="response")  # predicted scores

predicted



