pearson1900 <- function (x, n.classes = ceiling(2 * (n^(2/5))), adjust = TRUE) {
    ## Pearson's (1900) chi-square test of normality
    ## https://github.com/cran/nortest/blob/master/R/pearson.test.R

    x <- x[complete.cases(x)]
    n <- length(x)
    if (adjust) {
        dfd <- 2
    } else {
        dfd <- 0
    }
    num <- floor(1 + n.classes * pnorm(x, mean(x), sd(x)))
    count <- tabulate(num, n.classes)
    prob <- rep(1/n.classes, n.classes)
    xpec <- n * prob
    h <- ((count - xpec)^2)/xpec
    P <- sum(h)
    pvalue <- pchisq(P, n.classes - dfd - 1, lower.tail = FALSE)
    df <- n.classes - 1 - dfd
    result <- list(method = "Pearson (1900)",
                   doi = "https://doi.org/10.1080/14786440009463897",
                   statistic = P,
                   df = df,
                   p.value = pvalue)

    return(result)
}
