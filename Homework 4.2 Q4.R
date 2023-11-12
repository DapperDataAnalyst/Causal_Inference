library(rstanarm)
prop_data <- read.table(file = 'https://raw.githubusercontent.com/DapperDataAnalyst/Causal_Inference/main/prop_data.txt', header = T, sep = "\t", fileEncoding = "UTF-8")
Y <- prop_data$Y
Z <- prop_data$Z
X1 <- prop_data$X1
X2 <- prop_data$X2
X3 <- prop_data$X3
X4 <- prop_data$X4
X5 <- prop_data$X5

true_e <- invlogit(20*X1 - 5*X3*X4)

# Part a)
model <- stan_glm(Z ~ X1+X3:X4, family=binomial(link="logit"), refresh=0)
summary(model)
newdat <- data.frame(X1,X3,X4)
diffs <- true_e - predict(model,newdata = newdat, type = 'response')
sd(diffs)

# Part b)
model_undspec <- stan_glm(Z ~ X3:X4, family=binomial(link="logit"), refresh=0)
summary(model_undspec)
undspec_dat <- data.frame(X3,X4)
undspec_diffs <- true_e - predict(model_undspec,newdata = undspec_dat, type = 'response')
sd(undspec_diffs)

# Part c)
hist(undspec_diffs)

# Part d)
#' We are told in the prompt that the 
#' covariates are independent

# Part e)
model_overspec <- stan_glm(Z ~ X1+X2+X3+X4+X5+X3:X1+X3:X2+X3:X4+X3:X5, family=binomial(link="logit"), refresh=0)
summary(model_undspec)
overspec_dat <- data.frame(X1,
                           X2,
                           X3,
                           X4,
                           X5)
overspec_diffs <- true_e - predict(model_overspec,newdata = overspec_dat, type = 'response')
sd(overspec_diffs)

# Part f)
hist(overspec_diffs)

# Part g)
treat_true_e <- round(invlogit(20*X1[Z==1] - 5*X3[Z==1]*X4[Z==1]),3)
contr_true_e <- round(invlogit(20*X1[Z==0] - 5*X3[Z==0]*X4[Z==0]),3)

library(ggplot2)

compare_dat <- data.frame(
  value = c(treat_true_e, contr_true_e),
  group = factor(rep(c("Treatment", "Control"), c(length(treat_true_e), length(contr_true_e))))
)

ggplot(compare_dat, aes(x = value, fill = group)) +
  geom_histogram(binwidth = 0.3, position = "identity") +
  labs(
    x = "Values",
    y = "Frequency",
    title = "Overlapping Histograms of Treatment and Control"
  ) +
  scale_fill_manual(values = c("blue", "red"))

max(contr_true_e)
min(treat_true_e)
