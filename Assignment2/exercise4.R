clouds = read.table("./data/clouds.txt", header = TRUE)
sqrtClouds = sqrt(clouds)
sqrtSqrtClouds = sqrt(sqrtClouds)

par(mfrow=c(1,2))
hist(clouds$seeded - clouds$unseeded, main = "Histogram of Cloud Differences")
qqnorm(clouds$seeded - clouds$unseeded, main = "Normal Q-Q Plot Clouds Differences")

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
