jarque1980 <- function(x) {
    ## Jarque-Bera (1980) test of univariate normality
    ## https://doi.org/10.1016/0165-1765(80)90024-5
    ## https://github.com/cran/tsoutliers/blob/master/R/JarqueBera-test.R

    n <- length(x)
    m1 <- sum(x) / n
    m2 <- sum((x - m1) ^ 2) / n
    m3 <- sum((x - m1) ^ 3) / n
    m4 <- sum((x - m1) ^ 4) / n
    b1 <- (m3 / m2 ^ (3 / 2)) ^ 2
    b2 <- (m4 / m2 ^ 2)
    STATISTIC <- n * b1 / 6 + n * (b2 - 3) ^ 2 / 24
    PVAL <- 1 - pchisq(STATISTIC, df = 2)

    result <- list(method = "Jarque and Bera (1980)",
                   doi = "https://doi.org/10.1016/0165-1765(80)90024-5",
                   statistic = STATISTIC,
                   df = 2,
                   p.value = PVAL)

    return(result)
}
