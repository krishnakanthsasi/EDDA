psiData = read.table("./data/psi.txt", header = TRUE)

hist(psiData$gpa, freq = FALSE)
summary(psiData)

xtabs(~passed + psi, data = psiData)
