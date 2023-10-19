library(rstanarm)

# Part a)
22.5 - 16

# Part b)
4^2 * 2^5

# Part c)
n_iters <- 200
Y0_obs <- c(16, 20, 20, 10, 14)
Y1_obs <- c(14, 31)


Y0 <- c(NA, 16, 20, 20, 10, 14, NA)
Y1 <- c(14, rep(NA, 5), 31)
ATEs <- rep(NA, n_iters)

for (i in 1:n_iters){
  Y0[1] <- sample(Y0_obs, 1)
  Y0[7] <- sample(Y0_obs, 1)
  for (j in 2:6){
    Y1[j] <- sample(Y1_obs, 1)
  }
  ATEs[i] <- mean(Y1) - mean(Y0)
}

sd(ATEs)
