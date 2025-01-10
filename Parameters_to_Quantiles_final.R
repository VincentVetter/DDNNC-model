library(ExtDist)

setwd("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code")

# Importing predicted parameters
preds <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/preds.csv", header = FALSE)

# Empty matrix for saving results
percentiles <- data.frame(matrix(NA, nrow = 9168, ncol = 1000))

probs <- seq(1/101, 100/101, by=1/101)

for (day in 0:381) {
  for (hour in 0:23) {
      param1 <- unlist(preds[day*24+hour+1, 1:4])
      param2 <- unlist(preds[day*24+hour+1, 5:8])
      param3 <- unlist(preds[day*24+hour+1, 9:12])
      param4 <- unlist(preds[day*24+hour+1, 13:16])
      param5 <- unlist(preds[day*24+hour+1, 17:20])
      param6 <- unlist(preds[day*24+hour+1, 21:24])
      param7 <- unlist(preds[day*24+hour+1, 25:28])
      param8 <- unlist(preds[day*24+hour+1, 29:32])
      param9 <- unlist(preds[day*24+hour+1, 33:36])
      param10 <- unlist(preds[day*24+hour+1, 37:40])
      

      values1 <- qJohnsonSU(probs, gamma = param1[2], delta = param1[4], xi = param1[1], lambda = param1[3])
      values2 <- qJohnsonSU(probs, gamma = param2[2], delta = param2[4], xi = param2[1], lambda = param2[3])
      values3 <- qJohnsonSU(probs, gamma = param3[2], delta = param3[4], xi = param3[1], lambda = param3[3])
      values4 <- qJohnsonSU(probs, gamma = param4[2], delta = param4[4], xi = param4[1], lambda = param4[3])
      values5 <- qJohnsonSU(probs, gamma = param5[2], delta = param5[4], xi = param5[1], lambda = param5[3])
      values6 <- qJohnsonSU(probs, gamma = param6[2], delta = param6[4], xi = param6[1], lambda = param6[3])
      values7 <- qJohnsonSU(probs, gamma = param7[2], delta = param7[4], xi = param7[1], lambda = param7[3])
      values8 <- qJohnsonSU(probs, gamma = param8[2], delta = param8[4], xi = param8[1], lambda = param8[3])
      values9 <- qJohnsonSU(probs, gamma = param9[2], delta = param9[4], xi = param9[1], lambda = param9[3])
      values10 <- qJohnsonSU(probs, gamma = param10[2], delta = param10[4], xi = param10[1], lambda = param10[3])
      
      percentiles[day*24+hour+1,] = t(c(values1, values2, values3, values4, values5, 
                                        values6, values7, values8, values9, values10))
  }
}

write.csv(percentiles, "percentiles.csv", dec = ".")
