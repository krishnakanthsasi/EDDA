light = read.table("./data/light.txt") # Newcomb's measurements made in 1882 on three days
light1879 = read.table("./data/light1879.txt", fill = TRUE) # Michelson's measurements in 1879
light1882 = read.table("./data/light1882.txt", fill = TRUE) # Michelson's measurements in 1882

# TODO: Should we use something other than the mean? 
# Because I think there are some outliers which might mess up the mean
confidence = function(data) {
  B = 1000
  Tstar = numeric(B)
  
  for (i in 1:B) {
    Xstar = sample(data, replace=TRUE)
    Tstar[i] = median(Xstar)
  }
  
  Tstar25 = quantile(Tstar, 0.025, na.rm = TRUE)
  Tstar975 = quantile(Tstar, 0.975, na.rm = TRUE)
  
  T1 = median(data)
  sum(Tstar < Tstar25)
  c(2 * T1 - Tstar975, 2 * T1 - Tstar25)
}

lightMicro = (light / 1000) + 24.8 # Microseconds to travel 7442 kilometers
light = 7442 / (lightMicro * 10^(-3)) # TODO: This should be 10^-6 but something is not right

par(mfrow=c(1,2), oma = c(0, 0, 3, 0)) # Two graphs side by side
hist(light$V1, freq=FALSE, main = "Histogram", xlab="Speed of Light (km/sec)")
boxplot(light$V1)

mtext("Newcomb's Measurements in 1882", outer = TRUE, cex = 1.3)

light1879Stacked = stack(light1879 + 299000)
light1879Stacked = light1879Stacked[complete.cases(light1879Stacked), ]
hist(light1879Stacked$values, freq=FALSE, 
     main = "Histogram", xlab="Speed of Light (km/sec)")
boxplot(light1879Stacked$values)
mtext("Michelson's Measurements in 1879", outer = TRUE, cex = 1.3)

light1882Stacked = stack(light1882 + 299000)
light1882Stacked = light1882Stacked[complete.cases(light1882Stacked), ]
hist(light1882Stacked$values, freq=FALSE
     , main = "Histogram", xlab="Speed of Light (km/sec)")
boxplot(light1882Stacked$values)
mtext("Michelson's Measurements in 1882", outer = TRUE, cex = 1.3)

c1 = confidence(light$V1)
c2 = confidence(light1879Stacked$values)
c3 = confidence(light1882Stacked$values)

