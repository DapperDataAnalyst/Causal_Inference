library(rstanarm)
library(rstan)



advertising <- read.table(file = 'https://raw.githubusercontent.com/DapperDataAnalyst/Causal_Inference/main/advertising.txt', header = T, sep = "\t", fileEncoding = "UTF-8")

fit <- lm(sales_millions ~ minutes, data = advertising)
summary(fit)

fit_glm <- model <- stan_glm(sales_millions ~ minutes, data = advertising, refresh=0)
summary(fit_glm)
sims <- as.matrix(fit_glm)
min(sims[,2])

new <- data.frame(minutes=20)
y_pred <- posterior_predict(fit_glm, newdata=new)
quantile(y_pred, probs = 0.95)
