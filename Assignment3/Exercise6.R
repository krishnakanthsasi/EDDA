airPollution = read.table("./data/airpollution.txt", header = TRUE)

# 1st part
pairs(~ wind + temperature + humidity + insolation + oxidant, data = airPollution)

# 2nd part
model1 = lm(oxidant ~ wind, data = airPollution)
model2 = lm(oxidant ~ temperature, data = airPollution)
model3 = lm(oxidant ~ humidity, data = airPollution)
model4 = lm(oxidant ~ insolation, data = airPollution)

par(mfrow=c(2,2))
plot(oxidant ~ wind, data = airPollution); abline(model1)
plot(oxidant ~ temperature, data = airPollution); abline(model2)
plot(oxidant ~ humidity, data = airPollution); abline(model3)
plot(oxidant ~ insolation, data = airPollution); abline(model1)

sum1 = summary(model1)
sum2 = summary(model2)
sum3 = summary(model3)
sum4 = summary(model4)

round(sum1$r.squared, 3) # We choose this one because of the R2 score
round(sum2$r.squared, 3)
round(sum3$r.squared, 3)
round(sum4$r.squared, 3)

# We chose 1st one
stepUpModel = lm(oxidant ~ wind + temperature, data = airPollution)
stepUpSum = summary(stepUpModel)
round(stepUpSum$r.squared, 3)
stepUpModel = lm(oxidant ~ wind + humidity, data = airPollution)
stepUpSum = summary(stepUpModel)
round(stepUpSum$r.squared, 3)
stepUpModel = lm(oxidant ~ wind + insolation, data = airPollution)
stepUpSum = summary(stepUpModel)
round(stepUpSum$r.squared, 3)

# We somewhat choose first one
stepUpModel = lm(oxidant ~ wind + temperature + humidity, data = airPollution)
stepUpSum = summary(stepUpModel)
round(stepUpSum$r.squared, 3)
stepUpModel = lm(oxidant ~ wind + temperature + insolation, data = airPollution)
stepUpSum = summary(stepUpModel)
round(stepUpSum$r.squared, 3)

lm(oxidant ~ wind + temperature + humidity, data = airPollution)

# No need for this
stepUpModel = lm(oxidant ~ wind + temperature + humidity + insolation, data = airPollution)
stepUpSum = summary(stepUpModel)
round(stepUpSum$r.squared, 3)

# Part 3

# All combined
multiModel = lm(oxidant ~ wind + temperature + humidity + insolation, data = airPollution)
multiSum = summary(multiModel)
round(multiSum$coefficients, 6)

# 1st Round
multiModel = lm(oxidant ~ wind + temperature + humidity, data = airPollution)
multiSum = summary(multiModel)
round(multiSum$coefficients, 6)

#2nd Round
multiModel = lm(oxidant ~ wind + temperature, data = airPollution)
multiSum = summary(multiModel)
multiSum

# Part 4
estimate = function(wind, temp) {
  val = multiModel$coefficients[1] + 
    multiModel$coefficients[2] * wind + 
    multiModel$coefficients[3] * temp
  return(val)
}

estimate(airPollution$wind, airPollution$temperature)

# Part 5
par(mfrow=c(1,2))
qqnorm(residuals(multiModel))
plot(fitted(multiModel), residuals(multiModel))

