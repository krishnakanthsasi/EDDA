df <- read.table("C:/Users/Tommy/Desktop/run.txt")
df$cat <- c(rep(1,12), rep(2,12))

# ----- Exercise 1 -----
# --- Question 1 ---
summary(df)
plot(before~after, pch=cat, col=cat, data=df); abline(0,1)
boxplot(df[1:12,1],df[1:12,2], df[13:24,1], df[13:24,2], 
        col=c('red','red','blue','blue'), cex.axis = 0.75, 
        names=c("lemonade before","lemonade after","energy before","energy after"),
        ylab = "sprinting time")
boxplot(df[1:12,1]-df[1:12,2],df[13:24,1]-df[13:24,2],
        names = c("lemonade", "energy"), ylab = "differences per sample pair (before/after)")

# --- Question 2 ---
t.test(df[1:12,1],df[1:12,2],paired=TRUE)
t.test(df[13:24,1], df[13:24,2], paired=TRUE)

# --- Question 3 ---
df$differences <- df$before - df$after
t.test(df[1:12,5],df[13:24,5])



# --- Question 4 ---
# Since the participants were asked to run two stretches within a relatively small timespan,
# the first measurement may be affecting the second (learning effect). there could be additional factors affecting performance on the second run such as fatigue or 
# muscle activation (i.e. 'getting warmed up'). 

# --- Question 5 ---
# The samples are drawn from independent populations and one measurement does not affect the other.

# --- Question 6 ---
residuals <- df[1:12,5] - df[13:24,5]

qqnorm(residuals, main='Q-Q plot of residuals')
