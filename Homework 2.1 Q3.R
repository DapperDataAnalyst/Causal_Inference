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

Y0 <- 10 + 0 * X + 0 + e0

Y1 <- 10 + 0 * X + tau + e1

# Create omniscient dataframe:

omni_data <- data.frame(X, Y0, Y1)
#omni_data <- read.table(file = 'https://raw.githubusercontent.com/DapperDataAnalyst/Causal_Inference/main/quiz_omni.txt', header = T, sep = "\t", fileEncoding = "UTF-8")

set.seed(2021)

Z <- sample(c(1,0), size = N, replace = TRUE) # Generate random treatment assignments

Yobs <- Z * Y1 + (1 - Z) * Y0 # Observed potential outcomes Y(Z)

# Create observed dataframe:

obs_data <- data.frame(X, Z, Yobs)
#obs_data <- read.table(file = 'https://raw.githubusercontent.com/DapperDataAnalyst/Causal_Inference/main/quiz_obs.txt', header = T, sep = "\t", fileEncoding = "UTF-8")


# Create randomization distributions========================================

dm_res <- rep(NA,10000)  # Difference in means results
ols_res <- rep(NA,10000)  # OLS results

q <- 0.5
for (i in 1:10000) {
  # Create new assignment vector, pick observations
  Z_prime <- rbinom(N,1,q)
  Y_obs_prime <- Z_prime * omni_data$Y1 + (1 - Z_prime) * omni_data$Y0
  obs_data_prime <- data.frame(X, Z_prime, Y_obs_prime)
  
  # Collect dm values and save to results vector
  dm_res[i] <- mean(Y_obs_prime[Z_prime==1]) - mean(Y_obs_prime[Z_prime==0])
  
  ols_res[i] <- coef(lm(Y_obs_prime ~ X + Z_prime, data = obs_data_prime))['Z_prime']
}

# Perform Monte Carlo========================================================
library(ggplot2)

monte_carlo <- function(estimates){
  # Number of replicates for the Monte Carlo simulation
  num_replicates <- 1000
  
  # Initialize an empty vector to store the simulated ATEs
  simulated_ates <- numeric(num_replicates)
  
  # Monte Carlo simulation
  for (i in 1:num_replicates) {
    # Randomly shuffle treatment assignments (simulate randomization)
    shuffled_treatment <- sample(x=estimates,size=1000)
    
    # Calculate the ATE for the permuted treatment assignments
    simulated_ate <- mean(shuffled_treatment)
    
    # Store the simulated ATE in the vector
    simulated_ates[i] <- simulated_ate
  }
  # Create a data frame with the simulated ATEs
  df <- data.frame(Simulated_ATE = simulated_ates)
  return(df)
}

dm_mc <- monte_carlo(dm_res)
ols_mc <- monte_carlo(ols_res)

x_col <- c(1:1000,1:1000)

combined_data <- rbind(dm_mc,ols_mc)
combined_data <- cbind(x_col, combined_data)
combined_data$labels[1:1000] <- 'blue'  # This is DM
combined_data$labels[1001:2000] <- 'red'  # This is OLS

# Create a scatterplot using ggplot2
ggplot(combined_data, aes(x = x_col, y = Simulated_ATE)) +
  geom_point(color=combined_data$labels) +
  labs(
    title = "Scatterplot of Simulated ATEs",
    x = "X",
    y = "Simulated ATE"
  )

mean(dm_mc$Simulated_ATE)
var(dm_mc)
mean(ols_mc$Simulated_ATE)
var(ols_mc)



