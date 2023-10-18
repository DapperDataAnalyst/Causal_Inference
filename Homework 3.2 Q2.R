library(rstanarm)
age_data <- read.table(file = 'https://raw.githubusercontent.com/DapperDataAnalyst/Causal_Inference/main/age_data.txt', header = T, sep = "\t", fileEncoding = "UTF-8")

# Part a)
model1 <- stan_glm(Y ~ Z+age, family=binomial(link="logit"), data=age_data,
                   refresh=0)
summary(model1)

# Part b)
data_t <- data.frame(age = c(age_data$age[age_data$Z==1]), Z = c(age_data$Z[age_data$Z==1]))
data_c <- data.frame(age = c(age_data$age[age_data$Z==0]), Z = c(age_data$Z[age_data$Z==0]))

pt <- predict(model1, newdata = data_t, type = "response")
pc <- predict(model1, newdata = data_c, type = "response")

num <- ((1/length(pt)) * sum(pt)) / ((1/length(pt) * sum(1-pt)))
denom <- ((1/length(pc)) * sum(pc)) / ((1/length(pc) * sum(1-pc)))
causeodds <- num / denom
causeodds

# Part c)
Y1 <- posterior_predict(model1, newdata = data_t)
Y1 <- rowMeans(Y1)
Y0 <- posterior_predict(model1, newdata = data_c)
Y0 <- rowMeans(Y0)
mean(Y1 - Y0)

# Part d)
library(ggplot2)
age_data$mod1_pred <- model1$fitted.values
p1 <- ggplot(age_data, aes(x = age, y = jitter(Y))) +
  geom_point(alpha = 0.5, aes(color = factor(Z))) +
  
  # Add regression lines for treated and control
  geom_line(aes(y = mod1_pred,
                group = factor(Z),
                color = factor(Z)),
            size = 2) +
  
  # Customize the plot
  labs(x = "Age", y = "Outcome", color = "Group") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal() 
print(p1)


# Part e)
model2 <- stan_glm(Y ~ Z+age+Z:age, family = binomial(link = "logit"), data = age_data, refresh = 0)
age_data$mod2_pred <- model2$fitted.values

p2 <- ggplot(age_data, aes(x = age, y = jitter(Y))) +
  geom_point(alpha = 0.5, aes(color = factor(Z))) +
  
  # Add regression lines for treated and control
  geom_line(aes(y = mod2_pred,
                group = factor(Z),
                color = factor(Z)),
            size = 2) +
  
  # Customize the plot
  labs(x = "Age", y = "Outcome", color = "Group") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal() 
print(p2)





