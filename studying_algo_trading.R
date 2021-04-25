library("quantmod")
library("tasi")
tsi <- getIndexRecords(fromDate = "2010-01-01",toDate = "2021-03-08")
dim(tsi)[1]/240
tsi.close <- tsi$close

# Set Random Seed

# Create Time Index
t <- 1:(length(tsi.close)-1)
# Tradable Capital Vector
Vt <- c(rep(10000, length(t)))
# Benchmark Return Series
Rb <- rep(NA, length(t))
for(i in 2:length(t)) { Rb[i] <- (tsi.close[i] / tsi.close[i - 1]) - 1 }

# Benchmark Equity Curve
Eb <- rep(NA, length(t))
Eb[1] <- Vt[1]
for(i in 2:length(t)) { Eb[i] <- Eb[i-1] * (1 + Rb[i]) }




# Randomly Simulated Return Series 1
Rt <- rep(NA, length(t))
for(i in 2:length(t)){
  Rt[i] <- Rb[i] + rnorm(n = 1,
                         mean = 0.24/length(t),
                         sd = 2.5 * sd(Rb, na.rm = TRUE))
}
# Randomly Simulated Return Series 2
Rt2 <- rep(NA, length(t))
for(i in 2:length(t)){
  Rt2[i] <- Rb[i] + rnorm(n = 1,
                          mean = 0.02/length(t),
                          sd = .75 * sd(Rb, na.rm = TRUE))
}
# Randomly Simulated Equity Curve 1
Et <- rep(NA, length(t))
Et <- Vt[1]
for(i in 2:length(t)) { Et[i] <- Et[i-1] * (1 + Rt[i]) }
# Randomly Simulated Equity Curve 2
Et2 <- rep(NA, length(t))
Et2 <- Vt[1]
for(i in 2:length(t)) { Et2[i] <- Et2[i-1] * (1 + Rt2[i]) }
# Plot of Et1 against the SPY Portfolio
plot(y = Et, x = t, type = "l", col = 1,
     xlab = "Time",
     ylab= "Equity ($)",
     main = "Figure 1–3: Randomly Generated Equity Curves")

abline(h = 10000)
lines(y = Et2, x = t, col = 2)
lines(y = Eb, x = t, col = 8)
legend(x = "topleft", col = c(1,2,8), lwd = 2, legend = c("Curve 1",
                                                          "Curve 2",
                                                          "SPY"))

# Use na.rm = TRUE to ignore NAs at position 1 in return series
SR <- mean(Rt, na.rm = TRUE) / sd(Rt, na.rm = TRUE)
SR2 <- mean(Rt2, na.rm = TRUE) / sd(Rt2, na.rm = TRUE)
SRb <- mean(Rb, na.rm = TRUE) / sd(Rb, na.rm = TRUE)

plot(y = Et, x = t, type = "l", col = 1,
     xlab = "",
     ylab= "Equity ($)",
     main = "Figure 1-4: Sharpe Ratios")
grid()
abline(h = 10000)
lines(y = Et2, x = t, col = 2)
lines(y = Eb, x = t, col = 8)
legend(x = "topleft", col = c(1,2,8), lwd = 2,
       legend = c(paste0("SR = ", round(SR, 3)),
                  paste0("SR = ", round(SR2, 3)),
                  paste0("SR = ", round(SRb, 3))))


MD <- function(curve, n = 1){
  time <- length(curve)
  v <- rep(NA, (time * (time - 1)) / 2)
  k <- 1
  for(i in 1:(length(curve)-1)){
    for(j in (i+1):length(curve)){
      v[k] <- curve[i] - curve[j]
      k <- k + 1
    }
  }
  m <- rep(NA, length(n))
  for(i in 1:n){
    m[i] <- max(v)
    v[which.max(v)] <- -Inf
  }
  return(m)
}


NPMD <- (Et[length(Et)] - Vt[1]) / MD(Et)
Burke <- (Et[length(Et)] - Vt[1]) /
  sqrt((1/length(Et)) * sum(MD(Et, n = round(length(Et) / 20))^2))

min_cached_date <- min(as.Date(com_2m$transactionDate))
max_cached_date <- max(as.Date(com_2m$transactionDate))

start_date <- "2021-02-02" 
end_date <- "2021-02-09"

com_2m[which(as.Date(com_2m$transactionDate) >= start_date & as.Date(com_2m$transactionDate) <= end_date),]

df1<-getCompanyRecords(startDate = "2018-01-01", endDate = "2021-03-08", companySymbol = 8270)
  system.file("company_symbols", package = "tasi")
  