library(rstanarm)
covariates <- read.table(file = 'https://raw.githubusercontent.com/DapperDataAnalyst/Causal_Inference/main/covariates.txt', header = T, sep = "\t", fileEncoding = "UTF-8")
errors <- read.table(file = 'https://raw.githubusercontent.com/DapperDataAnalyst/Causal_Inference/main/errors.txt', header = T, sep = "\t", fileEncoding = "UTF-8")
X1 <- covariates$X1
X2 <- covariates$X2
X3 <- covariates$X3
X4 <- covariates$X4
Z <- covariates$Z
err <- errors$x
Y <- -250*X1 + 2*X2^2 + 0.4*X3^2 - 75*log(X4) - 100*Z + err

data <- data.frame(cbind(X1,X2,X3,X4,Z,err,Y))

# Part a)
# model1 <- stan_glm(Y ~ X1+X2+X3+X4+Z, data=data, refresh=0)
model1 <- lm(Y ~ X1+X2+X3+X4+Z, data=data)
# truth <- mean(Y[Z==1]) - mean(Y[Z==0])
truth <- 100
summary(model1)
diff <- abs(model1$coefficients['Z']) - truth
diff

# Part b)
n <- length(Z)
treat_effects <- rep(NA, 1000)
for (i in 1:1000){
  Z_new <- sample(0:1, nrow(covariates), replace = TRUE)
  Y <- -250*X1 + 2*X2^2 + 0.4*X3^2 - 75*log(X4) - 100*Z_new + err
  # model_iter <- stan_glm(Y ~ X1+X2+X3+X4+Z, data=data, refresh=0)
  model_iter <- lm(Y ~ X1+X2+X3+X4+Z_new)
  treat_effects[i] <- model_iter$coefficients['Z_new']
}
mean(treat_effects) # These seem to be incorrect

# Part c)
sd(treat_effects)

# Part d)
treat_eff_mod <- rep(NA, 1000)
for (i in 1:1000){
  Z_new <- sample(0:1, nrow(covariates), replace = TRUE)
  Y <- -250*X1 + 2*X2^2 + 0.4*X3^2 - 75*log(X4) - 100*Z_new + err
  model_iter <- lm(Y ~ X1+I(X2^2)+I(X3^2)+I(log(X4))+Z_new)
  treat_eff_mod[i] <- model_iter$coefficients['Z_new']
}
mean(treat_eff_mod)
sd(treat_eff_mod)
