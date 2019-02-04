kruskal1952 <- function(x, g, ...) {
    ## Kruskall and Wallis' (1952) test of equality of distribution
    ## https://doi.org/10.1080/01621459.1952.10483441
    ## https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/kruskal.test.R

    if (is.list(x)) {
        if (length(x) < 2L) {
            stop("'x' must be a list with at least 2 elements")
        }
        if (!missing(g)) {
            warning("'x' is a list, so ignoring argument 'g'")
        }
        DNAME <- deparse(substitute(x))
        x <- lapply(x, function(u) u <- u[complete.cases(u)])
        if (!all(sapply(x, is.numeric))) {
            warning("some elements of 'x' are not numeric and will be coerced to numeric")
        }
        k <- length(x)
        l <- sapply(x, "length")
        if (any(l == 0L)) {
            stop("all groups must contain data")
        }
        g <- factor(rep.int(seq_len(k), l))
        x <- unlist(x)
    }
    else {
        if (length(x) != length(g)) {
            stop("'x' and 'g' must have the same length")
        }
        DNAME <- paste(deparse(substitute(x)), "and",
                       deparse(substitute(g)))
        OK <- complete.cases(x, g)
        x <- x[OK]
        g <- g[OK]
        if (!all(is.finite(g))) {
            stop("all group levels must be finite")
        }
        g <- factor(g)
        k <- nlevels(g)
        if (k < 2L) {
            stop("all observations are in the same group")
        }
    }

    n <- length(x)
    if (n < 2L) {
        stop("not enough observations")
    }
    r <- rank(x)
    TIES <- table(x)
    STATISTIC <- sum(tapply(r, g, "sum")^2 / tapply(r, g, "length"))
    ## keep as n+1 to avoid (implausible) integer overflows
    STATISTIC <- ((12 * STATISTIC / (n * (n + 1)) - 3 * (n + 1)) /
                  (1 - sum(TIES^3 - TIES) / (n^3 - n)))
    PARAMETER <- k - 1L
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    names(STATISTIC) <- "Kruskal-Wallis chi-squared"
    names(PARAMETER) <- "df"

    result <- list(method = "Kruskal and Wallis (1952) rank-sum test",
                   statistic = STATISTIC,
                   df = PARAMETER,
                   p.value = PVAL)

    return(result)
}
