fligner1976 <- function(x, g, ...) {
    ## Fligner and Killeen's (1976) test of homogeneity of variances
    ## https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/fligner.test.R

    ## FIXME: This is the same code as in kruskal.test(), and could also
    ## rewrite bartlett.test() accordingly ...
    if (is.list(x)) {
        if (length(x) < 2L)
            stop("'x' must be a list with at least 2 elements")
        DNAME <- deparse(substitute(x))
        x <- lapply(x, function(u) u <- u[complete.cases(u)])
        k <- length(x)
        l <- sapply(x, "length")
        if (any(l == 0))
            stop("all groups must contain data")
        g <- factor(rep(1 : k, l))
        x <- unlist(x)
    }
    else {
        if (length(x) != length(g))
            stop("'x' and 'g' must have the same length")
        DNAME <- paste(deparse(substitute(x)), "and",
                       deparse(substitute(g)))
        OK <- complete.cases(x, g)
        x <- x[OK]
        g <- g[OK]
        if (!all(is.finite(g)))
            stop("all group levels must be finite")
        g <- factor(g)
        k <- nlevels(g)
        if (k < 2)
            stop("all observations are in the same group")
    }
    n <- length(x)
    if (n < 2)
        stop("not enough observations")
    ## FIXME: now the specific part begins.

    ## Careful. This assumes that g is a factor:
    x <- x - tapply(x,g,median)[g]

    a <- qnorm((1 + rank(abs(x)) / (n + 1)) / 2)
    statistic <- sum(tapply(a, g, "sum")^2 / tapply(a, g, "length"))
    statistic <- (statistic - n * mean(a)^2) / var(a)
    df <- k - 1
    p.value <- pchisq(statistic, df, lower.tail = FALSE)

    result <- list(method = "Fligner and Killeen (1976)",
                   doi = "https://doi.org/10.1080/01621459.1976.10481517",
                   statistic = statistic,
                   df = df,
                   p.value = p.value)

    return(result)
}
