nausea.table <- read.table("C:/Users/Tommy/Desktop/EDDA/EDDA - Assignment 3/nauseatable.txt")

#5.1
nausea <- c(rep(1,nausea.table[1,2]), rep(0,nausea.table[1,1]),
            rep(1,nausea.table[2,2]), rep(0,nausea.table[2,1]),
            rep(1,nausea.table[3,2]), rep(0,nausea.table[3,1]))

medicin <- factor(rep(1:3, c((nausea.table[1,1]+nausea.table[1,2]),
                             (nausea.table[2,1]+nausea.table[2,2]),
                             (nausea.table[3,1]+nausea.table[3,2]))), 
                  labels=c("chlor","pent100","pent150"))

nausea.frame <- data.frame(nausea,medicin)

#5.2
xtabs(~medicin+nausea)

# Same layout as original nausea.table

#5.3
attach(nausea.frame)
perm.ind.test(nausea.frame, type = "raw", R = B) 

B <- 1000
tstar <- numeric(B)

for(i in 1:B)
{
  medicinstar <- sample(medicin) 
  tstar[i] <- chisq.test(xtabs(~medicinstar+nausea))[[1]]
}

myt <- chisq.test(xtabs(~medicin+nausea))[[1]]

hist(tstar)
abline(v=myt, col='red')

pr <- sum(tstar>myt)/B
pr

print("The test statistic obtained for the labeling in the experiment is higher
      than 95% of the test statistics for the permuted labels. The p-value is
      lower than \alpha (0.039), which could warrant a rejection of the null
      hypothesis. This indicates the medicines do not work equally well against
      nausea.")

#5.4
chisq.test(xtabs(~medicin+nausea))[[3]]

print("The p-value is almost equal (0.036) to that of the permutation test
      in 5.3 (0.039). Both tests detect a relationship between the variables
      'type of drug' and 'incidence of nausea', making independence unlikely.")