#simulation for 1000 random exponentials
set.seed(1000)
rnumbers <- rexp(1000, 0.2)

#simulation for 1000 averages of 40 random exponentials
set.seed(4000)
rmeans = NULL
for(i in 1:1000) {
        rmeans <- c(rmeans, mean(rexp(40, 0.2)))
}

#comparing means of simulations
par(mfrow = c(1, 2), mar = c(4, 4, 3, 0))
hist(rnumbers, col = "green", xlab = "Number", freq = FALSE,
     main = "1,000 Random Exponential Numbers")
abline(v = mean(rnumbers), col = "red", lwd = 4)
text(6, 400, labels = "Red Line is Mean", pos = 4, cex = 2, col = "red")
x.num <- seq(0, 50, length.out=100)
y.num <- dnorm(x.num, mean(rnumbers), sd(rnumbers))
lines(x.num, y.num, col = "blue", lwd = 2, lty = 2)

hist(rmeans, col = "yellow", xlab = "Number", freq = FALSE,
     main = "1,000 Means of 40 Random Exponential Numbers")
abline(v = mean(rmeans), col = "red", lwd = 4)
text(5.5, 200, labels = "Red Line is Mean", pos = 4, cex = 2, col = "red")
x.mean <- seq(2, 8, length.out=10)
y.mean <- dnorm(x.mean, mean(rmeans), sd(rmeans))
lines(x.mean, y.mean, col = "blue", lwd = 2, lty = 2)

means <- data.frame("Dataset" = c("1,000 Numbers", "1,000 Means"), 
                    "Value" = c(mean(rnumbers), mean(rmeans)))
print(means)

#comparing variances






