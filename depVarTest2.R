tr <- function(p) {
  result <- sum(diag(p))
  result
}

data <- read.csv("example1_data1.csv")
data <- data[,1:6]

tocompare <- cor(data)

nu <- ncol(data)
s <- cov(data)
p <- cor(data) #s
#diag(p) <- mean(diag(p))
#p <- cov2cor(p)
n <- nrow(data)

sigmahat <- (1/nu) * tr(solve(p)) * s

num <- det(s)^(n/2)
denom <- det(sigmahat %*% p)^(n/2)

lambda <- num / denom

chisqob <- -2 * log(lambda)

pvalue <- pchisq(chisqob, 1)
