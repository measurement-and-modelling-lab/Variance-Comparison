zhang2005c <- function(x){ # Zhang Zc
    n <- length(x)
    phi <- pnorm(sort((x - mean(x)) / sd(x)))
    res <- sum((log((1 / phi - 1) / ((n - 0.5) / ((1:n) - 0.75) - 1))) ^ 2)
    return(res)
}
