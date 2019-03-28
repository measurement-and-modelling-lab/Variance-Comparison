shapiro1972 <- function (x) {
    ## The Shapiro and Francia's (1972) test of normality
    ## https://github.com/cran/nortest/blob/master/R/sf.test.R

    x <- sort(x[complete.cases(x)])
    n <- length(x)

    if ((n < 5 || n > 5000))
    {
        stop("sample size must be between 5 and 5000")
    }

    y <- qnorm(ppoints(n, a = 3/8))
    W <- cor(x, y)^2
    u <- log(n)
    v <- log(u)
    mu <- -1.2725 + 1.0521 * (v - u)
    sig <- 1.0308 - 0.26758 * (v + 2/u)
    z <- (log(1 - W) - mu)/sig
    pval <- pnorm(z, lower.tail = FALSE)

    result <- list(method = "Shapiro and Francia (1972)",
                   doi = "http://dx.doi.org/10.1080/01621459.1972.10481232",
                   statistic = W,
                   df = NA,
                   p.value = pval)

    return(result)

}
