doornik1994 <- function(x) {
    ## Doornik-Hansen (1994) test of normality
    ## https://github.com/cran/normwn.test/blob/master/R/normality.test1.R

    n <- length(x)
    m1 <- sum(x) / n
    v.matrix <- 1 / sqrt(cov.wt(as.matrix(x))$cov)
    xhatprime.matrix <- t(x - m1)
    trprime <- t(v.matrix %*% xhatprime.matrix)
    v1 <- mean(trprime)
    v2 <- (n ^ (-1)) * sum((trprime - v1) ^ 2)
    v3 <- (n ^ (-1)) * sum((trprime - v1) ^ 3)
    v4 <- (n ^ (-1)) * sum((trprime - v1) ^ 4)
    rtb1 <- v3 / (v2 ^ (3 / 2))
    b2 <- v4 / v2 ^ 2
    beta <- (3 * (n ^ 2 + 27 * n - 70) * (n + 1) * (n + 3)) / ((n - 
        2) * (n + 5) * (n + 7) * (n + 9))
    w2 <- (-1) + sqrt(2 * (beta - 1))
    delta <- 1 / sqrt(log(sqrt(w2)))
    f <- (w2 - 1) / 2
    g <- (n + 1) * (n + 3) / (6 * (n - 2))
    h <- sqrt(f * g)
    y <- rtb1 * h
    z1 <- delta * log(y + sqrt(y ^ 2 + 1))
    del <- ((n - 3) * (n + 1) * (n ^ 2 + 15 * n - 4))
    aye <- ((n - 2) * (n + 5) * (n + 7) * (n ^ 2 + 27 * n - 70)) / (6 * 
        del)
    cee <- ((n - 7) * (n + 5) * (n + 7) * (n ^ 2 + 2 * n - 5)) / (6 * 
        del)
    alp <- aye + ((rtb1 ^ 2) * cee)
    kap <- ((n + 5) * (n + 7) * (n ^ 3 + 37 * n ^ 2 + 11 * n - 313)) / (12 * 
        del)
    chi <- (b2 - 1 - rtb1 ^ 2) * (2 * kap)
    chi <- abs(chi)
    z2 <- (((chi / (2 * alp)) ^ (1 / 3)) - 1 + (1 / ((9 * alp)))) * sqrt(9 * 
        alp)
    stat <- z1 ^ 2 + z2 ^ 2
    pval <- 1 - pchisq(stat, 2)

    result <-  list(method = "Doornik-Hansen (1994)",
                    doi = "https://doi.org/10.1111/j.1468-0084.2008.00537.x",
                    statistic = stat,
                    df = 2,
                    p.value = pval)

    return(result)
}
