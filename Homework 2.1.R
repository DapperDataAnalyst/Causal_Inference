# Set seed for reproducibility (and to get the right answer!):

RNGkind(sample.kind = 'Rounding')

set.seed(2021)

N <- 1000 # number of individuals

tau <- 7 # homogenous treatment effect

# Generate errors and pre-treatment covariates:

e0 <- rnorm(n = N, mean = 0, sd = 1)

e1 <- rnorm(n = N, mean = 0, sd = 1)

X <- rnorm(n = N, mean = 65, sd = 9)

# Compute potential outcomes based on linear relationship:

Y0 <- 10 + 0.9 * X + 0 + e0

Y1 <- 10 + 0.9 * X + tau + e1

# Create omniscient dataframe:

#omni_data <- data.frame(X, Y0, Y1)
omni_data <- read.table(file = 'https://raw.githubusercontent.com/DapperDataAnalyst/Causal_Inference/main/quiz_omni.txt', header = T, sep = "\t", fileEncoding = "UTF-8")

set.seed(2021)

Z <- sample(c(1,0), size = N, replace = TRUE) # Generate random treatment assignments

Yobs <- Z * Y1 + (1 - Z) * Y0 # Observed potential outcomes Y(Z)

# Create observed dataframe:

#obs_data <- data.frame(X, Z, Yobs)
obs_data <- read.table(file = 'https://raw.githubusercontent.com/DapperDataAnalyst/Causal_Inference/main/quiz_obs.txt', header = T, sep = "\t", fileEncoding = "UTF-8")


#========================================================

dm_res <- rep(NA,10000)  # Difference in means results
ols_res <- rep(NA,10000)  # OLS results

q <- 0.5
for (i in 1:10000) {
  # Create new assignment vector, pick observations
  Z_prime <- rbinom(N,1,q)
  Y_obs_prime <- Z_prime * Y1 + (1 - Z_prime) * Y0
  
  # Collect dm values and save to results vector
  Y_dm <- mean(Y_obs_prime[Z==1]) - mean(Y_obs_prime[Z==0])
  dm_res[i] <- Y_dm
  
  
}
