library(ggplot2)


beauty <- read.table(file = 'https://raw.githubusercontent.com/DapperDataAnalyst/Causal_Inference/main/beauty.txt', header = T, sep = "\t", fileEncoding = "UTF-8")

model <- stan_glm(eval ~ beauty + female + age + minority + nonenglish + lower, data = beauty, refresh=0)
summary(model)
# Create a scatterplot of the data
data_plot <- ggplot(beauty, aes(x = beauty, y = eval)) +
  geom_point() +  # Scatterplot points
  labs(x = "Beauty", y = "Evaluations") +  # Axis labels
  theme_minimal()  # Minimalist theme

# Add the fitted model line (excluding unreasonable predictors)
data_plot <- data_plot + geom_smooth(method = "lm",
                                     formula = y ~ x,
                                     aes(color = "Fitted Model"),
                                     data = subset(beauty, select = c("eval", "beauty")))

# Display the plot
print(data_plot)


