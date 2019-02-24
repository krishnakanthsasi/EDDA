data = read.table("./data/telephone.txt", header = TRUE)

par(mfrow=c(1,2)) # Two graphs side by side

B = 1000 # Iterations times

# Bounds for rate of exponential distribution
rateLower = 0.01
rateUpper = 0.1
rateIncrease = 0.01

n = length(data$Bills)

# Setup for loop
tStar = numeric(B)
t = median(data$Bills)

hist(data$Bills, probability = TRUE)

# Try for all rates
for(rate in seq(from=rateLower, to=rateUpper, by=rateIncrease)) {
  for(iter in seq(from=0, to=B, by=1)) {
    # Get surrogate X*s from exponential distribution
    # with same size as the original data set
    sample = rexp(n, rate)
    
    # Store T* values for future comparison
    tStar[iter] = median(sample)
  }
  
  # Calculate p-value according to the slides of week-2
  pl = sum(tStar<t) / B
  pr = sum(tStar>t) / B
  p = 2*min(pl, pr)
  
  if (p > 0.05) {
    hist(tStar, probability = TRUE)
    print(sprintf("Rate: %.2f P-Value: %.2f", rate, p))
    print("H0 is not rejected.")
    break
  }
}

# Try to plot it with same graph style in week-2/30th slide
# TODO: Figure out how to plot properly
par(mfrow=c(1,1))
hist(tStar, probability=TRUE, ylim=c(0, 0.25), main="Histogram of T* & True Density Curve of T")

