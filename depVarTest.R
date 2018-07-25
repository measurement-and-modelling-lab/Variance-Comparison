data <- read.csv("example1_data1.csv")

v1 <- data[,2]
v2 <- data[,3]

n = length(v1) + length(v2)

top <- as.matrix(rbind(c(var(v1), cov(v1, v2)), c(cov(v1, v2), var(v2))))

form <- (var(v1) + var(v2))/2
bottom <- as.matrix(rbind(c(form, cov(v1, v2)), c(cov(v1, v2), form)))

prelambda <- det(top) / det(bottom)

lambda <- prelambda^(n/2)

chisqob <- -2 * log(lambda)

pvalue <- pchisq(chisqob, 1)

#n = sample size, nu = matrix size = 2, so df = 1