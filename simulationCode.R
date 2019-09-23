#simulation for 1000 averages of 40 random exponentials
set.seed(4000)
rmeans = NULL
for(i in 1:1000) {
        rmeans <- c(rmeans, mean(rexp(40, 0.2)))
}

#comparing means of simulations
par(mar = c(4,4,2,1))
hist(rmeans, col = "yellow", xlab = "Number", freq = FALSE,
     main = "1,000 Means of 40 Random Exponential Numbers")
abline(v = mean(rmeans), col = "red", lwd = 2)
abline(v = 1/0.2, col = "green", lwd = 2, lty = 2)
x.mean <- seq(2, 8, length.out=100)
y.mean <- dnorm(x.mean, mean(rmeans), sd(rmeans))
lines(x.mean, y.mean, col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("Simulation Mean", "Theoretical Mean", 
                              "Density Curve"), 
       col = c("red", "green", "blue"), lwd = c(1, 2, 2), lty = c(1, 2, 2))

means <- data.frame("Statistic" = c("Theoretical Mean", "Simulation Mean"), 
                    "Mean" = c(1/0.2, mean(rmeans)))
print(means)

#comparing variances
hist(rmeans, col = "yellow", xlab = "Number", freq = FALSE, xlim = c(0, 10),
     main = "1,000 Means of 40 Random Exponential Numbers")
x.mean <- seq(2, 8, length.out=100)
y.mean <- dnorm(x.mean, mean(rmeans), sd(rmeans))
lines(x.mean, y.mean, col = "blue", lwd = 2, lty = 2)
abline(v = mean(rmeans) - sd(rmeans), col = "red", lwd = 2, lty = 2)
abline(v = mean(rmeans) + sd(rmeans), col = "red", lwd = 2, lty = 2)
abline(v = 1/0.2 - 1/0.2, col = "green", lwd = 2, lty = 2)
abline(v = 1/0.2 + 1/0.2, col = "green", lwd = 2, lty = 2)
legend("topright", legend = c("Simulation SD", "Theoretical SD", 
                              "Density Curve"), 
       col = c("red", "green", "blue"), lwd = c(1, 2, 2), lty = c(1, 2, 2))

sds <- data.frame("Statistic" = c("Theoretical Mean", "Simulation Mean"), 
                  "Standard Deviation" = c(1/0.2, sd(rmeans)))
print(sds)

#prove normal distribution
sd1low <- mean(rmeans) - sd(rmeans)
sd2low <- mean(rmeans) - 2*sd(rmeans)
sd3low <- mean(rmeans) - 3*sd(rmeans)
sd1high <- mean(rmeans) + sd(rmeans)
sd2high <- mean(rmeans) + 2*sd(rmeans)
sd3high <- mean(rmeans) + 3*sd(rmeans)
sd1percent <- length(which(rmeans <= sd1high & rmeans >=sd1low))/length(rmeans)
sd2percent <- length(which(rmeans <= sd2high & rmeans >=sd2low))/length(rmeans)
sd3percent <- length(which(rmeans <= sd3high & rmeans >=sd3low))/length(rmeans)
dist <- data.frame("Standard_Deviations" = c(1, 2, 3), 
                   "Normal_Distribution" = c(.68, .95, .99),
                   "Simulation_Percents" = c(sd1percent, sd2percent, sd3percent))
print(dist)





