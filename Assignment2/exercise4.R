clouds = read.table("./data/clouds.txt", header = TRUE)
sqrtClouds = sqrt(clouds)
sqrtSqrtClouds = sqrt(sqrtClouds)

par(mfrow=c(1,2))
hist(clouds$seeded - clouds$unseeded, main = "Histogram of Differences")
qqnorm(clouds$seeded - clouds$unseeded, main = "Normal Q-Q Plot Differences")

# T - Test

# Differences of the data samples does not seem to be normal
# However, histogram of these differences is seem to be?
# This is probably not paired since QQ Normal Plot suggests that
# the distribution is not normal
t.test(clouds$seeded, clouds$unseeded)

# Mann - Whitney Test

wilcox.test(clouds$seeded, clouds$unseeded)

# Kolmogorov - Smirnov Test

ks.test(clouds$seeded, clouds$unseeded)

# Section 2

par(mfrow=c(1,2))
hist(sqrtClouds$seeded - sqrtClouds$unseeded, main = "Histogram of Sqrt Differences")
qqnorm(sqrtClouds$seeded - sqrtClouds$unseeded, main = "Normal Q-Q Sqrt Differences")

# T - Test

t.test(sqrtClouds$seeded, sqrtClouds$unseeded)

# Mann - Whitney Test

wilcox.test(sqrtClouds$seeded, sqrtClouds$unseeded)

# Kolmogorov - Smirnov Test

ks.test(sqrtClouds$seeded, sqrtClouds$unseeded)

# Section 3

par(mfrow=c(1,2))
hist(sqrtSqrtClouds$seeded - sqrtSqrtClouds$unseeded, main = "Histogram of SqrtSqrt Differences")
qqnorm(sqrtSqrtClouds$seeded - sqrtSqrtClouds$unseeded, main = "Normal Q-Q SqrtSqrt Differences")

# T - Test

t.test(sqrtSqrtClouds$seeded, sqrtSqrtClouds$unseeded)

# Mann - Whitney Test

wilcox.test(sqrtSqrtClouds$seeded, sqrtSqrtClouds$unseeded)

# Kolmogorov - Smirnov Test

ks.test(sqrtSqrtClouds$seeded, sqrtSqrtClouds$unseeded)
