challenger <- read.table(file = 'https://raw.githubusercontent.com/DapperDataAnalyst/Causal_Inference/main/challenger.txt', header = T, sep = "\t", fileEncoding = "UTF-8")

# Part a)
model1 <- stan_glm(Fail ~ Temperature, family=binomial(link="logit"), data=challenger,
                  refresh=0)
summary(model1) # Note that point estimates directly out of the model are on the log-odds scale

# Part b)
new1 <- data.frame(Temperature=69)
pred_prob1 <- predict(model1, type="response", newdata=new1)
new2 <- data.frame(Temperature=70)
pred_prob2 <- predict(model1, type="response", newdata=new2)
pred_prob2 - pred_prob1

# Part e)
challenger$Temperature_Celsius <- (challenger$Temperature - 32) * (5/9)
model2 <- stan_glm(Fail ~ Temperature_Celsius, family=binomial(link="logit"), data=challenger,
                   refresh=0)
summary(model2) # Note that point estimates directly out of the model are on the log-odds scale

# Part f)
newF1 <- data.frame(Temperature=70)
pred_probF1 <- predict(model1, type="response", newdata=newF1)
newF2 <- data.frame(Temperature=65)
pred_probF2 <- predict(model1, type="response", newdata=newF2)
pred_probF2 - pred_probF1

linpredF1 <- posterior_linpred(model1, newdata=newF1)
linpredF2 <- posterior_linpred(model1, newdata=newF2)
mean(linpredF2 - linpredF1) # Not very confident that this is the correct method

# Part g)
sd(linpredF2 - linpredF1)

