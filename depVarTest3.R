data <- read.csv("example1_data1.csv")
data <- data[,1:6]

vars <- sapply(1:6, function(i) var(data[,i]))
varprods <- prod(vars)


