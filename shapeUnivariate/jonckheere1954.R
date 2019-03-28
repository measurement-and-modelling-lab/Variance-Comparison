jonckheere1954 <- function(x, g, alternative = "monotonic", ...) {
    ## Jonckheere's (1954) trend test
    ## https://github.com/cran/PMCMR/blob/master/R/jonckheere.test.R
    ## I think monotonic is a sensible default for "alternative", but I'm not sure
    
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
    alternative <- match.arg(alternative)
    n <- length(x)
    if (n < 2)
        stop("not enough observations")
    nij <- tapply(x, g, length)
    X <- matrix(NA, ncol= k, nrow = max(nij))
    j <- 0
    for (i in 1:k) {
        for (l in 1:nij[i]) {
            j = j + 1
            X[l,i] <- x[j]
        }
    }
    psi.f <- function(u) {
       if (u > 0) {
           psi <- 1
       } else if (u == 0) {
           psi <- 0.5
       } else {
           psi = 0
       }
       psi
    }
    Uij <- function(i,j,X){
        ni <- nij[i]
        nj <- nij[j]
        sumUij <- 0
        for (s in (1:ni)) {
            for (t in (1:nj)) {
                sumUij <- sumUij + psi.f(X[t,j] - X[s,i])
            }
        }
        sumUij
    }
    J <- 0
    for (i in (1:(k-1))) {
        for (j in ((i+1):k)) {
            J = J + Uij(i,j,X)
        }
    }
    mu <- (n^2 - sum(nij^2)) / 4
    st <- 0
    for (i in (1:k)) {
        st <- st + nij[i]^2 * (2 * nij[i] + 3)
        st
    }
    s <- sqrt((n^2 * (2 * n + 3) - st) / 72)
    STATISTIC <- (J - mu) / s
    if (alternative == "monotonic") {
        PVAL <- 2 * min(pnorm(STATISTIC), 1 - pnorm(STATISTIC), 0.5)    
    } else if (alternative == "increasing") {
        PVAL <- 1 - pnorm(STATISTIC)
    } else {
        PVAL <- pnorm(STATISTIC)
    }

    result <- list(method = "Jonckheere (1954) trend test",
                   doi = "https://doi.org/10.2307/2333011",
                   statistic = STATISTIC,
                   df = NA,
                   p.value = PVAL)
    return(result)
}
