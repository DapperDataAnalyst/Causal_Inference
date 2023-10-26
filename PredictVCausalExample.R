rm(list = ls())
library(rstanarm)
set.seed(51)

#' Simulate data to roughly reflect the illustration in Section 20.1
#' of the Gelman, Hill, Vehtari text
#' 
#' 100 patients receive new medical treatment
#' 100 patients receive control treatment
#' 
#' Start by simulating zero causal effect but positive predictive
#' comparison
n<- 200
n1 = n*.5
n0 = n-n1
prev_health = rep(NA, n)
Y0 = rep(NA, n)
Y1 = rep(NA, n)
Y = rep(NA, n)
Z = c(rep(1, n1), rep(0, n0))

#' The distribution of previous health status is not the same
#' in the two treatment groups
#' Healthier patients tend to receive the
#' experimental treatment, sicker patients
#' tend to receive the control
p.poor.z1 = 0.15
n.poor.z1 = round(n1*p.poor.z1)
p.fair.z1 = 0.25
n.fair.z1 = round(n1*p.fair.z1)
p.good.z1 = 1 - p.poor.z1 - p.fair.z1
n.good.z1 = n1 - n.poor.z1 - n.fair.z1
prev_health[Z==1] = rep(c(1,2,3), times = c(n.poor.z1, n.fair.z1, n.good.z1))

p.poor.z0 = 0.6
n.poor.z0 = round(n0*p.poor.z0)
p.fair.z0 = 0.25
n.fair.z0 = round(n0*p.fair.z0)
p.good.z0 = 1 - p.poor.z0 - p.fair.z0
n.good.z0 = n0 - n.poor.z0 - n.fair.z0
prev_health[Z==0] = rep(c(1,2,3), times = c(n.poor.z0, n.fair.z0, n.good.z0))

table(Z, prev_health)

#' Simulate Y0 to depend on previous health status
Y0[prev_health==1] = rnorm(sum(prev_health==1), 2, 0.5)
Y0[prev_health==2] = rnorm(sum(prev_health==2), 3, 0.5)
Y0[prev_health==3] = rnorm(sum(prev_health==3), 4, 0.5)

#' Exactly zero causal effect!
Y1 = Y0

#' "Reveal" the observed potential outcome
Y[Z==1] = Y1[Z==1]
Y[Z==0] = Y0[Z==0]

dat = data.frame(cbind(Z,prev_health,Y))

ns = rbind(c(n.poor.z0, n.fair.z0, n.good.z0), 
           c(n.poor.z1, n.fair.z1, n.good.z1))
par(mfrow = c(3,2))
for (i in 1:3)
  for(j in 0:1)
hist(Y[prev_health==i & Z==j], xlim = c(0.5, 5.5), breaks = 10,
     main = paste("PrevHealth:", i, " Z=", j, " n=", ns[j+1,i], sep=""), 
     xlab = "Y", ylab = "")

mod1 <- stan_glm(Y~Z, data=dat)
print(mod1)



#' Now lets simulate data where there is a positive causal
#' effect but zero predictive positive comparison
prev_health = rep(NA, n)
Y0 = rep(NA, n)
Y1 = rep(NA, n)
Y = rep(NA, n)
Z = c(rep(1, n1), rep(0, n0))

#' Again, the distribution of prev_health is not the same in
#' the treatment/control groups
#' Sicker patients tend to receive treatment, healthier
#' patients tend to receive control
p.poor.z1 = 0.6
n.poor.z1 = round(n1*p.poor.z1)
p.fair.z1 = 0.25
n.fair.z1 = round(n1*p.fair.z1)
p.good.z1 = 1 - p.poor.z1 - p.fair.z1
n.good.z1 = n1 - n.poor.z1 - n.fair.z1
prev_health[Z==1] = rep(c(1,2,3), times = c(n.poor.z1, n.fair.z1, n.good.z1))

p.poor.z0 = 0.15
n.poor.z0 = round(n0*p.poor.z0)
p.fair.z0 = 0.25
n.fair.z0 = round(n0*p.fair.z0)
p.good.z0 = 1 - p.poor.z0 - p.fair.z0
n.good.z0 = n0 - n.poor.z0 - n.fair.z0
prev_health[Z==0] = rep(c(1,2,3), times = c(n.poor.z0, n.fair.z0, n.good.z0))

table(Z, prev_health)

#' Simulate Y0 to be different for the different previous
#' health statuses
Y0[prev_health==1] = rnorm(sum(prev_health==1), 1.5, 0.5)
Y0[prev_health==2] = rnorm(sum(prev_health==2), 2.5, 0.5)
Y0[prev_health==3] = rnorm(sum(prev_health==3), 3.5, 0.5)

#' Causal effect of exactly 1
Y1 = Y0 + 1

Y[Z==0] = Y0[Z==0]
Y[Z==1] = Y1[Z==1]

dat <- data.frame(cbind(Z,prev_health,Y))

ns = rbind(c(n.poor.z0, n.fair.z0, n.good.z0), 
           c(n.poor.z1, n.fair.z1, n.good.z1))
par(mfrow = c(3,2))
for (i in 1:3)
  for(j in 0:1)
    hist(Y[prev_health==i & Z==j], xlim = c(0.5, 5.5), breaks = 10,
         main = paste("PrevHealth:", i, " Z=", j, " n=", ns[j+1,i], sep=""), 
         xlab = "Y", ylab = "")

mod2 <- stan_glm(Y~Z, data=dat)
print(mod2)


#' Prev_health is a confounder in this example.  Since it is observed
#' in teh data set, we can adjust for it (e.g., with a regresison model)
mod3 <- stan_glm(Y~Z+prev_health, data=dat)
print(mod3)
