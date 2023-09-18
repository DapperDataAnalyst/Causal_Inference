balance_data <- read.table(file = 'https://raw.githubusercontent.com/DapperDataAnalyst/Causal_Inference/main/balance_data.txt', header = T, sep = "\t", fileEncoding = "UTF-8")
matched_data <- read.table(file = 'https://raw.githubusercontent.com/DapperDataAnalyst/Causal_Inference/main/matched_data.txt', header = T, sep = "\t", fileEncoding = "UTF-8")

# Part 1=================================================================
smd <- function(variable){
  abs(mean(variable[balance_data$Treat==1]) - mean(variable[balance_data$Treat==0])) / sqrt(var(variable[balance_data$Treat==1]))
}
smd(balance_data$Xcont.1)
smd(balance_data$Xcat.1)


# Part 2=================================================================
# a)
smd(balance_data$Xcont.1)
smd(balance_data$Xcont.2)
smd(balance_data$Xcat.1)
smd(balance_data$Xcat.2)

# b)
smd <- function(variable){
  abs(mean(variable[matched_data$Treat==1]) - mean(variable[matched_data$Treat==0])) / sqrt(var(variable[matched_data$Treat==1]))
}

smd(matched_data$Xcont.1)
smd(matched_data$Xcont.2)
smd(matched_data$Xcat.1)
smd(matched_data$Xcat.2)

