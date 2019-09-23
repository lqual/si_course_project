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
par(mfrow = c(1, 1), mar = c(4,4,2,1))
library(dplyr)
oj <- ToothGrowth %>% select(len, supp) %>% filter(supp == "OJ") %>% pull(len)
vc <- ToothGrowth %>% select(len, supp) %>% filter(supp == "VC") %>% pull(len)
plot(density(oj), col = "orange", lwd = 4, xlab = "Tooth Length", 
     main = "Density of Tooth Length Impact by Supplement Type")
lines(density(vc), col = "blue", lwd = 4)
legend("topright", legend = c("OJ", "VC"), 
       col = c("orange", "blue"), lwd = 4)

ptype <- t.test(oj, vc, paired = FALSE, var.equal = TRUE, 
                data = ToothGrowth)$p.value
ctype <- t.test(oj, vc, paired = FALSE, var.equal = TRUE, 
                data = ToothGrowth)$conf
typestats <- data.frame("Statistic" = c("P-Value", "Conf.Level"), 
                    "Low_Value" = c(ptype, ctype[1]), 
                    "High_Value" = c(NA, ctype[2]))
print(typestats)

#dose effects
d5 <- ToothGrowth %>% select(len, dose) %>% filter(dose == 0.5) %>% pull(len)
d1 <- ToothGrowth %>% select(len, dose) %>% filter(dose == 1) %>% pull(len)
d2 <- ToothGrowth %>% select(len, dose) %>% filter(dose == 2) %>% pull(len)
plot(density(d5), col = "red", lwd = 4, xlab = "Tooth Length", ylim = c(0, .12),
     main = "Density of Tooth Length Impact by Dosage Rate", xlim = c(0, 40))
lines(density(d1), col = "blue", lwd = 4)
lines(density(d2), col = "green", lwd = 4)
legend("topright", legend = c("0.5", "1.0", "2.0"), 
       col = c("red", "blue", "green"), lwd = 4)

prate1 <- t.test(d5, d1, paired = FALSE, var.equal = TRUE, 
                data = ToothGrowth)$p.value
crate1 <- t.test(d5, d1, paired = FALSE, var.equal = TRUE, 
                data = ToothGrowth)$conf
prate2 <- t.test(d5, d2, paired = FALSE, var.equal = TRUE, 
                 data = ToothGrowth)$p.value
crate2 <- t.test(d5, d2, paired = FALSE, var.equal = TRUE, 
                 data = ToothGrowth)$conf
prate3 <- t.test(d1, d2, paired = FALSE, var.equal = TRUE, 
                 data = ToothGrowth)$p.value
crate3 <- t.test(d1, d2, paired = FALSE, var.equal = TRUE, 
                 data = ToothGrowth)$conf

ratestats <- data.frame("Statistic" = c("P-Value (.5-1)", "Conf.Level (.5-1)", 
                                        "P-Value (.5-2)", "Conf.Level (.5-2)",
                                        "P-Value (1-2)", "Conf.Level (1-2)"), 
                        "Low_Value" = c(prate1, crate1[1], prate2, crate2[1], 
                                        prate3, crate3[1]), 
                        "High_Value" = c(NA, crate1[2], NA, crate2[2], 
                                         NA, crate3[2]))
print(ratestats)
