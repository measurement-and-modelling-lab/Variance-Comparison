bartlett1937 <- function(x, g, ...) {
    ## Bartlett's (1937) test of homogeneity of variances
    ## https://doi.org/10.1098/rspa.1937.0109
    ## https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/bartlett.test.R

    LM <- FALSE

    if (is.list(x)) {
        if (length(x) < 2L) {
            stop("'x' must be a list with at least 2 elements")
        }
        DNAME <- deparse(substitute(x))
        if (all(sapply(x, function(obj) inherits(obj, "lm")))) {
            LM <- TRUE
        }
        else {
            x <- lapply(x, function(x) x <- x[is.finite(x)])
        }
        k <- length(x)
    }
    else {
        if (length(x) != length(g)) {
            stop("'x' and 'g' must have the same length")
        }
        DNAME <- paste(deparse(substitute(x)), "and",
                       deparse(substitute(g)))
        OK <- complete.cases(x, g)
        x <- x[OK]
        g <- factor(g[OK])
        k <- nlevels(g)
        if (k < 2)
        {
            stop("all observations are in the same group")
        }
        x <- split(x, g)
    }

    if (LM) {
        n <- sapply(x, function(obj) obj$df.resid)
        v <- sapply(x, function(obj) sum(obj$residuals^2))
    } else {
        n <- sapply(x, "length") - 1
        if (any(n <= 0))
            stop("there must be at least 2 observations in each group")
        v <- sapply(x, "var")
    }

    n.total <- sum(n)
    v.total <- sum(n * v) / n.total
    statistic <- ((n.total * log(v.total) - sum(n * log(v))) /
                  (1 + (sum(1 / n) - 1 / n.total) / (3 * (k - 1))))
    df <- k - 1
    p.value <- pchisq(statistic, df, lower.tail = FALSE)

    result <- list(method = "Bartlett test of homogeneity of variances",
                   statistic = statistic,
                   df = df,
                   p.value = p.value)

    return(result)
}
