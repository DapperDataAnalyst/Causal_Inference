library("rstanarm")
invlogit <- plogis

#' #### Load data
# electric_wide <- read.table("electric_wide.txt", header=TRUE)
electric_wide <- read.table(file = 'https://raw.githubusercontent.com/DapperDataAnalyst/Causal_Inference/main/electric_wide.txt', header = T, sep = "", fileEncoding = "UTF-8")
head(electric_wide)
attach(electric_wide)

# Organize data
post_test <- c(treated_posttest, control_posttest)
pre_test <- c(treated_pretest, control_pretest)
grade <- rep(electric_wide$grade, 2)
treatment <- rep(c(1,0), rep(length(treated_posttest),2))
supp <- rep(NA, length(treatment))
n_pairs <- nrow(electric_wide)
pair_id <- rep(1:n_pairs, 2)
supp[treatment==1] <- ifelse(supplement=="Supplement", 1, 0)
n <- length(post_test)
electric <- data.frame(post_test, pre_test, grade, treatment, supp, pair_id)
head(electric)



#' Naive comparison within each grade, just comapring the mean
#' outcomes in the supp (vs. replace) groups
est1 <- rep(NA,4)
est2 <- rep(NA,4)
se1 <- rep(NA,4)
se2 <- rep(NA,4)
for (k in 1:4){
  cat(paste("Grade",k,":\n"))
  ok <- (grade==k)&(!is.na(supp))
  glm_supp <- stan_glm(post_test ~ supp,
                       subset=((grade==k)&!is.na(supp)), data=electric, refresh = 0, 
                       save_warmup = FALSE, open_progress = FALSE, cores = 1)
  print(glm_supp)
  est1[k] <- coef(glm_supp)[2]
  se1[k] <- se(glm_supp)[2]
}
est1
se1

regression.2tablesA <- function (name, est1, se1, label1, file, bottom=FALSE){
  J <- length(name)
  name.range <- .6
  x.range <- range (est1+2*se1, est1-2*se1)
  A <- -x.range[1]/(x.range[2]-x.range[1])
  B <- 1/(x.range[2]-x.range[1])
  height <- .6*J
  width <- 8*(name.range+1)
  gap <- .4
  
  if (!is.na(file)) postscript(file, horizontal=F, height=height, width=width)
  par (mar=c(0,0,0,0))
  plot (c(-name.range,2+gap), c(3,-J-2), bty="n", xlab="", ylab="",
        xaxt="n", yaxt="n", xaxs="i", yaxs="i", type="n")
  text (-name.range, 2, "Subpopulation", adj=0, cex=1)
  text (.5, 2, label1, adj=.5, cex=1)
  lines (c(0,1), c(0,0))
  lines (c(A,A), c(0,-J-1), lty=2, lwd=.5)
  ax <- pretty (x.range)
  ax <- ax[(A+B*ax)>0 & (A+B*ax)<1]
  segments (A + B*ax, -.1, A + B*ax, .1, lwd=.5)
  text (A + B*ax, .7, ax, cex=1)
  text (-name.range, -(1:J), name, adj=0, cex=1)
  points (A + B*est1, -(1:J), pch=20, cex=1)
  segments (A + B*(est1-se1), -(1:J), A + B*(est1+se1), -(1:J), lwd=3)
  segments (A + B*(est1-2*se1), -(1:J), A + B*(est1+2*se1), -(1:J), lwd=.5)
  if (bottom){
    lines (c(0,1), c(-J-1,-J-1))
    segments (A + B*ax, -J-1-.1, A + B*ax, -J-1+.1, lwd=.5)
    text (A + B*ax, -J-1-.7, ax, cex=1)
  } 
  if (!is.na(file)) graphics.off()
}

par(mfrow = c(1,1))
regression.2tablesA(paste("Grade", 1:4), est1, se1, "Estimated effect of supplement,\ncompared to replacement", NA)


#' But let's look at the distribution of the 'pre_test' variable across
#' the treatment groups to see whether it's balanced

par(mfrow = c(4,2), mar=c(2,2,2,2))
for (k in 1:4){
  ok <- (grade==k)&(!is.na(supp))
  hist(pre_test[ok&supp==0], 
       main = paste("Mean Pre-Test, Z=0:", round(mean(pre_test[ok&supp==0]), 1)))
  hist(pre_test[ok&supp==1], 
       main = paste("Mean Pre-Test, Z=1:", round(mean(pre_test[ok&supp==1]), 1)))
}

#' Now fit the regressions within grade but adjust for 'pre_test',
#' which corresponds to an assumption of conditional ignorability, i.e.,
#' that the decision to supplement (vs. replace) is "as if randomized"
#' within levels of pre-test reading score
est2 = est1
se2 = se1
for (k in 1:4){
  cat(paste("Grade",k,":\n"))
  ok <- (grade==k)&(!is.na(supp))
  glm_supp <- stan_glm(post_test ~ supp + pre_test,
                       subset=((grade==k)&!is.na(supp)), data=electric, refresh = 0, 
                       save_warmup = FALSE, open_progress = FALSE, cores = 1)
  print(glm_supp)
  est2[k] <- coef(glm_supp)[2]
  se2[k] <- se(glm_supp)[2]
}

par(mfrow = c(1,1))
regression.2tablesA(paste("Grade", 1:4), est2, se2, "Estimated effect of supplement,\ncompared to replacement", NA)


#' Compare the change in effect estimates with and without
#' adjustment for pre-test reading score
par(mfrow = c(1,2))
regression.2tablesA(paste("Grade", 1:4), est1, se1, "Estimated effect of supplement,\ncompared to replacement", NA)
regression.2tablesA(paste("Grade", 1:4), est2, se2, "Estimated effect of supplement,\ncompared to replacement", NA)
