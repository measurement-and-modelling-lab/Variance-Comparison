mardia1970_skewness <- function(X) {
    ## Mardia (1970)
    ## https://doi.org/10.1093/biomet/57.3.519

    X <- as.matrix(X)
    
    n <- nrow(X)
    p <- ncol(X)

    S <- cov.wt(X, method="ML")$cov

    col_means <- apply(X, 2, mean)
    difT <- sweep(X, 2, col_means, "-")

    D <- difT%*%solve(S)%*%t(difT)
    b1p <- sum(colSums(D^3))/n^2

    df = (p*(p+1)*(p+2))/6
    MST = (n*b1p)/6
    p = 1 - pchisq(MST,df)

    result <- list(method = "Mardia (1970) skewness test",
                   statistic = MST,
                   df = df,
                   p.value = p)

    return(result)
}
