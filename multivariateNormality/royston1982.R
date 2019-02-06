source("./univariateNormality/shapiro1965.R")
source("./univariateNormality/shapiro1972.R")

royston1982 <- function (data, tol = 1e-25) {
    ## Royston's (1982) test of multivariate normality
    ## https://doi.org/10.2307/2347973
    ## https://github.com/selcukorkmaz/MVN/blob/master/R/mvn.R
    
    if (dim(data)[2] < 2 || is.null(dim(data))) {
        stop("number of variables must be equal or greater than 2")
    }
    if (!is.data.frame(data) && !is.matrix(data))
        stop("Input must be one of classes \"data frame\" or \"matrix\"")
    dataframe = as.data.frame(data)
    dname <- deparse(substitute(data))
    data <- data[complete.cases(data), ]
    data <- as.matrix(data)
    p <- dim(data)[2]
    n <- dim(data)[1]
    z <- matrix(nrow <- p, ncol = 1)
    z <- as.data.frame(z)
    w <- matrix(nrow <- p, ncol = 1)
    w <- as.data.frame(w)
    data.org <- data
    if (n <= 3) {
        stop("n must be greater than 3")
    }
    else if (n >= 4 || n <= 11) {
        x <- n
        g <- -2.273 + 0.459 * x
        m <- 0.544 - 0.39978 * x + 0.025054 * x^2 - 0.0006714 *
            x^3
        s <- exp(1.3822 - 0.77857 * x + 0.062767 * x^2 - 0.0020322 *
                 x^3)
        for (i in 1:p) {
            a2 <- data[, i]
            {
                if (kurtosis(a2) > 3) {
                    w <- shapiro1972(a2)$statistic
                }
                else {
                    w <- shapiro1965(a2)$statistic
                }
            }
            z[i, 1] <- (-log(g - (log(1 - w))) - m)/s
        }
    }
    if (n > 2000) {
        stop("n must be less than 2000")
    }
    else if (n >= 12 || n <= 2000) {
        x <- log(n)
        g <- 0
        m <- -1.5861 - 0.31082 * x - 0.083751 * x^2 + 0.0038915 *
            x^3
        s <- exp(-0.4803 - 0.082676 * x + 0.0030302 * x^2)
        for (i in 1:p) {
            a2 <- data[, i]
            {
                if (kurtosis(a2) > 3) {
                    w <- shapiro1972(a2)$statistic
                }
                else {
                    w <- shapiro1965(a2)$statistic
                }
            }
            z[i, 1] <- ((log(1 - w)) + g - m)/s
        }
    }
    else {
        stop("n is not in the proper range")
    }
    u <- 0.715
    v <- 0.21364 + 0.015124 * (log(n))^2 - 0.0018034 * (log(n))^3
    l <- 5
    C <- cor(data)
    NC <- (C^l) * (1 - (u * (1 - C)^u)/v)
    T <- sum(sum(NC)) - p
    mC <- T/(p^2 - p)
    edf <- p/(1 + (p - 1) * mC)
    Res <- matrix(nrow = p, ncol = 1)
    Res <- as.data.frame(Res)
    for (i in 1:p) {
        Res <- (qnorm((pnorm(-z[, ]))/2))^2
    }
    data <- scale(data, scale = FALSE)
    Sa <- cov(data)
    D <- data %*% solve(Sa, tol = tol) %*% t(data)

    RH <- (edf * (sum(Res)))/p
    pValue <- pchisq(RH, edf, lower.tail = FALSE)

    result <- list(method = "Royston (1982)",
                   statistic = RH,
                   df = NA,
                   p.value = pValue)

    return(result)

}
