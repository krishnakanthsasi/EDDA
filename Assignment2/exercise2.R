light = read.table("./data/light.txt") # Newcomb's measurements made in 1882 on three days
light1879 = read.table("./data/light1879.txt", fill = TRUE) # Michelson's measurements in 1879
light1882 = read.table("./data/light1882.txt", fill = TRUE) # Michelson's measurements in 1882

par(mfrow=c(2,3)) # Two graphs side by side
hist(light$V1, freq=FALSE, main = "Histogram of Newcomb's Measurements")
light1879Stacked = stack(light1879)
hist(light1879Stacked$values, freq=FALSE, main = "Histogram of Michelson's Measurements in 1879")

light1882Stacked = stack(light1882)
hist(light1882Stacked$values, freq=FALSE, main = "Histogram of Michelson's Measurements in 1882")

boxplot(light$V1)
boxplot(light1879Stacked$values)
boxplot(light1882Stacked$values)