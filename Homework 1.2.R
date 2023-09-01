library(tidyverse)
library(tidyr)

data <- read.table(file = 'quiz_omni.txt', header = T, sep = "\t", fileEncoding = "UTF-8")

# Question 6:
true_SATE <- mean(data$Y1 - data$Y0)
true_SATE

# Reshape the data from wide to long format
data_long <- gather(data, key = "Outcome", value = "Value", Y0, Y1)

# Create a line plot with both Y0 and Y1 on X
ggplot(data_long, aes(x = X, y = Value, color = Outcome)) +
  geom_line() +
  labs(title = "Line Plot of Y0 and Y1 on X",
       x = "X",
       y = "Value") +
  scale_color_manual(values = c("Y0" = "blue", "Y1" = "red"))
# 
# # Set seed for reproducibility (and to get the right answer!):
# 
# RNGkind(sample.kind="Rounding")
# 
# set.seed(2021)
# 
# N <- 1000 # number of individuals
# 
# tau <- 7 # homogenous treatment effect
# 
# # Generate errors and pre-treatment covariates:
# 
# e0 <- rnorm(n = N, mean = 0, sd = 1)
# 
# e1 <- rnorm(n = N, mean = 0, sd = 1)
# 
# X <- rnorm(n = N, mean = 65, sd = 9)
# 
# # Compute potential outcomes based on linear relationship:
# 
# Y0 <- 10 + 0.9 * X + 0 + e0
# 
# Y1 <- 10 + 0.9 * X + tau + e1
# 
# # Create dataframe:
# 
# quiz_omni <- data.frame(X, Y0, Y1)
# 
# # Set seed for reproducibility
# 
# set.seed(2021)
# 
# Z <- sample(c(1,0), size = N, replace = TRUE) # Generate random treatment assignments
# 
# Yobs <- Z * Y1 + (1 - Z) * Y0 # Observed potential outcomes Y(Z)
# 
# # Create dataframe:
# 
# quiz_obs <- data.frame(X, Z, Yobs)

quiz_obs <- read.table(file = 'quiz_obs.txt', header = T, sep = "\t", fileEncoding = "UTF-8")

est_SATE <- mean(quiz_obs$Yobs[which(quiz_obs$Z == 1)]) - mean(quiz_obs$Yobs[which(quiz_obs$Z == 0)])
est_SATE

(true_SATE - est_SATE) / sqrt(var(quiz_obs$Yobs))
