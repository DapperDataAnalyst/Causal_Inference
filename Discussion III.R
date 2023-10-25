library(rstanarm)
cows <- read.table(file = 'https://raw.githubusercontent.com/DapperDataAnalyst/Causal_Inference/main/Cows.txt', header = T, sep = "\t", fileEncoding = "UTF-8")

# Part 1)
model1 <- stan_glm(milk*fat ~ level, data = cows, refresh=0)
summary(model1)

# Part 2)
model2 <- stan_glm(milk*fat ~ level+lactation+age+initial.weight, data = cows, refresh=0)
summary(model2)

# Part 3)

# Fit the stan_glm model with 'level' as a categorical predictor
model3 <- stan_glm(milk*fat ~ factor(level) + lactation + age + initial.weight, data = cows, refresh = 0)
summary(model3)

# Load the ggplot2 package
library(ggplot2)

# Create a sample dataset
data_multi <- data.frame(
  Category = c('Level 1','Level 2', 'Level 3'),
  PointEstimate = c(coef(model3)[2],coef(model3)[3],coef(model3)[4]),
  LowerCI = c(coef(model3)[2]+2*se(model3)[2],coef(model3)[3]+2*se(model3)[3],coef(model3)[4]+2*se(model3)[4]),
  UpperCI = c(coef(model3)[2]-2*se(model3)[2],coef(model3)[3]-2*se(model3)[3],coef(model3)[4]-2*se(model3)[4])
)

# Create the multiplot
plot <- ggplot(data_multi, aes(x = PointEstimate, y = Category)) +
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

