
expensesCrime = read.table("./data/expensescrime.txt", header = TRUE)
pairs(expensesCrime[-1])

expensesCrime[-1] = log(expensesCrime[-1])
pairs(expensesCrime[-1])
# expend: in 1000$
# bad: number of people under criminal supervision
# crime: crime rate per 100000
# lawyer: num of lawyers in the state
# employ: num of people employed in the state
# pop: population of the state in 1000

# From the pair-wise scatter plots, expenditure can be explained linearly.

# We check for influncing points
model = lm(expend ~ bad + crime + lawyers + employ + pop, data = expensesCrime[-1])
rows = round(cooks.distance(model), 3) > 1
round(cooks.distance(model), 3)
expensesCrime[rows,]
plot(1:51, cooks.distance(model))

newData = expensesCrime[!rows, ]

model = lm(expend ~ bad + crime + lawyers + employ + pop, data = newData)
summary(model)
model = lm(expend ~ crime + lawyers + employ + pop, data = newData)
summary(model)
model = lm(expend ~ crime + lawyers + employ, data = newData)
summary(model)
model = lm(expend ~ crime + employ, data = newData)
summary(model)

model

par(mfrow=c(1,2))
qqnorm(residuals(model))
plot(fitted(model), residuals(model))

