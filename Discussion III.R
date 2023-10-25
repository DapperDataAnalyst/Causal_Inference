library(rstanarm)
library(dplyr)
cows <- read.table(file = 'https://raw.githubusercontent.com/DapperDataAnalyst/Causal_Inference/main/Cows.txt', header = T, sep = "\t", fileEncoding = "UTF-8")

# Part 1)
model1 <- stan_glm(milk*fat ~ level, data = cows, refresh=0)
summary(model1)

# Part 2)
model2 <- stan_glm(milk*fat ~ level+lactation+age+initial.weight, data = cows, refresh=0)
summary(model2)

# Part 3)
# Convert 'level' to a factor with four levels
cows$level <- as.factor(cows$level)

# Fit the stan_glm model with 'level' as a categorical predictor
model2 <- stan_glm(milk*fat ~ level + lactation + age + initial.weight, data = cows, refresh = 0)

# Get posterior samples of the coefficients
posterior_samples <- as.matrix(model2)

# Get the point estimates for the coefficient of 'level' for each level
point_estimates <- data.frame(
  Level = c(0.1, 0.2, 0.3),
  Coefficient = apply(posterior_samples[, grepl("level", colnames(posterior_samples))], 2, mean),
  StnDev = apply(posterior_samples[, grepl("level", colnames(posterior_samples))], 2, sd)
)

point_estimates <- point_estimates %>% mutate(lower = Coefficient - 2*StnDev) %>%
  mutate(upper = Coefficient + 2*StnDev)

# Load the ggplot2 package
library(ggplot2)

# Create a sample dataset
data <- data.frame(
  Category = point_estimates$Level,
  PointEstimate = point_estimates$Coefficient,
  LowerCI = point_estimates$lower,
  UpperCI = point_estimates$upper
)

# Create the multiplot
plot <- ggplot(data, aes(x = PointEstimate, y = Category)) +
  geom_linerange(aes(xmin = LowerCI, xmax = UpperCI), size = 1) +
  geom_point(size = 5) +  # Add point estimates
  labs(
    title = "Point Estimates and Confidence Intervals of Treatment Effect for Levels 1-3",
    x = "Treatment Effect",
    y = "Level"
  ) +
  theme_bw()

# Display the plot
print(plot)



cows <- read.table(file = 'https://raw.githubusercontent.com/DapperDataAnalyst/Causal_Inference/main/Cows.txt', header = T, sep = "\t", fileEncoding = "UTF-8")
model2 <- stan_glm(milk*fat ~ level + lactation + age + initial.weight, data = cows, refresh = 0)
treat_effect <- coef(model2)[2]
standev <- se(model2)[2]
LowerCI <- treat_effect - 2*standev
UpperCI <- treat_effect + 2*standev
data <- data.frame(
  PointEstimate = treat_effect,
  LowerCI = LowerCI,
  UpperCI = UpperCI
)
# Create the single plot
plot <- ggplot(data, aes(x = treat_effect)) +
  geom_linerange(aes(y = (LowerCI + UpperCI) / 2, xmin = LowerCI, xmax = UpperCI), size = 1) +
  geom_point(aes(y = (LowerCI + UpperCI) / 2), size = 5) +
  labs(
    title = "Point Estimate and Confidence Interval of Treatment Effect",
    x = "Treatment Effect"
  ) +
  theme_bw() +
  labs(y = NULL) +  # Remove y-axis title
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())  # Remove y-axis labels and ticks

# Display the horizontal plot
print(plot)

