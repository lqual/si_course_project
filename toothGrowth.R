#exploratory data analysis
library(datasets)
summary(ToothGrowth)
ToothGrowth$supp
ToothGrowth$len
ToothGrowth$dose
dim(ToothGrowth)
ToothGrowth
plot(ToothGrowth$supp,ToothGrowth$len)
plot(ToothGrowth$dose,ToothGrowth$len)

#summary
stats <- data.frame("Statistic" = c("Mean", "Median", "Min", "Max", "SD"), 
                   "Tooth_Length" = c(mean(ToothGrowth$len), 
                                      median(ToothGrowth$len), 
                                      min(ToothGrowth$len),
                                      max(ToothGrowth$len),
                                      sd(ToothGrowth$len)))
print(stats)

par(mfrow = c(1, 2), mar = c(4, 4, 3, 0))
plot(ToothGrowth$supp,ToothGrowth$len, col = "purple", xlab = "Supplement Type", 
     ylab = "Tooth Length", main = "Tooth Length by Supplement Type")
plot(ToothGrowth$dose,ToothGrowth$len, 
     col = rgb(red = 1, green = 1, blue = 0, alpha = 0.5), xlab = "Dosage", 
     ylab = "Tooth Length", main = "Tooth Length by Dosage Type", pch = 19,
     cex = 2)

#delivery method effects
par(mar = c(4,4,2,1))
library(dplyr)
oj <- ToothGrowth %>% select(len, supp) %>% filter(supp == "OJ") %>% pull(len)
vc <- ToothGrowth %>% select(len, supp) %>% filter(supp == "VC") %>% pull(len)
plot(density(oj), col = "orange", lwd = 4, xlab = "Tooth Length", 
     main = "Density of Tooth Length Impact by Supplement Type")
lines(density(vc), col = "blue", lwd = 4)
legend("topright", legend = c("OJ", "VC"), 
       col = c("orange", "blue"), lwd = 4)

print(t.test(oj, vc, paired = FALSE, var.equal = TRUE, data = ToothGrowth))

#dose effects
d5 <- ToothGrowth %>% select(len, dose) %>% filter(dose == 0.5) %>% pull(len)
d1 <- ToothGrowth %>% select(len, dose) %>% filter(dose == 1) %>% pull(len)
d2 <- ToothGrowth %>% select(len, dose) %>% filter(dose == 2) %>% pull(len)
plot(density(d5), col = "red", lwd = 4, xlab = "Tooth Length", ylim = c(0, .12),
     main = "Density of Tooth Length Impact by Dosage Type", xlim = c(0, 40))
lines(density(d1), col = "blue", lwd = 4)
lines(density(d2), col = "green", lwd = 4)
legend("topright", legend = c("0.5", "1.0", "2.0"), 
       col = c("red", "blue", "green"), lwd = 4)

print(t.test(d5, d1, paired = FALSE, var.equal = TRUE, data = ToothGrowth))
print(t.test(d5, d2, paired = FALSE, var.equal = TRUE, data = ToothGrowth))
print(t.test(d1, d2, paired = FALSE, var.equal = TRUE, data = ToothGrowth))

