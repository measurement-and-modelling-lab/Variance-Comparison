stat3 <- function(x){ # Zhang Zc
    n <- length(x)
    phi <- pnorm(sort((x - mean(x)) / sd(x)))
    res <- sum((log((1 / phi - 1) / ((n - 0.5) / ((1:n) - 0.75) - 1))) ^ 2)
    return(res)
}

stat4 <- function(x) { # Zhang Za
    n <- length(x)
    phi <- pnorm(sort((x - mean(x)) / sd(x)))
    res <- -sum(((log(phi)) / (n - (1:n) + 0.5)) + ((log(1.0 - phi)) / ((1:n) - 0.5)))
    return(10.0 * res - 32.0) # See page 711 in their paper
}
