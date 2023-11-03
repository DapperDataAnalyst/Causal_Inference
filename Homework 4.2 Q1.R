data <- read.csv(file = 'https://raw.githubusercontent.com/DapperDataAnalyst/Causal_Inference/main/Lalonde.csv', header = T)

# Part 1)
treat <- data$treat
re78 <- data$re78
effect <- mean(earn78[treat==1]) - mean(earn78[treat==0])
effect

# Part 2)
library(rstanarm)
age <- data$age
educ <- data$educ
black <- data$black
married <- data$married
nodegree <-data$nodegree
hisp <- data$hisp
re74 <- data$re74
re75 <- data$re75
model <- stan_glm(re78 ~ treat+re74+re75+nodegree+age+educ+married+hisp+black)
coef(model)

# Part 3)
library(ggplot2)

ggplot(data, aes(x = re74, fill = factor(treat))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Overlapping Histograms of re74 by treat",
       x = "re74",
       y = "Frequency") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal()
