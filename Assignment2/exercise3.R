klmData = scan("./data/klm.txt")

par(mfrow=c(1,3))
# This doen't look like it is from normal distribution?
hist(klmData, freq=FALSE)
qqnorm(klmData)
boxplot(klmData)

# H_0 median duration is <= 31 days
testMedian = 31

klmMedian = median(klmData)

# We expect median to divide the data set into two equal parts
# so that when a random sample is chosen, the probability of it
# being smaller or greater than the median should be equal to
# tossing a coin

sumOut = sum(klmData <= testMedian) # Get values smaller than the test value

binom.test(sumOut, length(klmData), p=0.5, alternative = "less")

#Seconds part
lateDays = numeric(sum(klmData > 72))
lateDays = klmData[which(klmData > 72)]
lateDays # Days greater than max delivery days of 72

binom.test(length(lateDays), length(klmData), p=0.1, alternative = "greater")


