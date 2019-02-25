light = read.table("./data/light.txt") # Newcomb's measurements made in 1882 on three days
light1879 = read.table("./data/light1879.txt", fill = TRUE) # Michelson's measurements in 1879
light1882 = read.table("./data/light1882.txt", fill = TRUE) # Michelson's measurements in 1882

lightMicro = (light / 1000) + 24.8 # Microseconds to travel 7442 kilometers
light = 7442 / (lightMicro * 10^(-3)) # TODO: This should be 10^-6 but something is not right

par(mfrow=c(1,2), oma = c(0, 0, 3, 0)) # Two graphs side by side
hist(light$V1, freq=FALSE, main = "Histogram", xlab="Speed of Light (km/sec)")
boxplot(light$V1)
mtext("Newcomb's Measurements in 1882", outer = TRUE, cex = 1.3)

light1879Stacked = stack(light1879 + 299000)
hist(light1879Stacked$values, freq=FALSE, 
     main = "Histogram", xlab="Speed of Light (km/sec)")
boxplot(light1879Stacked$values)
mtext("Michelson's Measurements in 1879", outer = TRUE, cex = 1.3)

light1882Stacked = stack(light1882 + 299000)
hist(light1882Stacked$values, freq=FALSE
     , main = "Histogram", xlab="Speed of Light (km/sec)")
boxplot(light1882Stacked$values)
mtext("Michelson's Measurements in 1882", outer = TRUE, cex = 1.3)
