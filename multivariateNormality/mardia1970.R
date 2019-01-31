mardia1970 <- function(X) {
    ## Mardia's (1970) tests of multivariate skewness and kurtosis
    ## https://doi.org/10.1093/biomet/57.3.519
    
    n <- nrow(X)
    p <- ncol(X)

    S <- cov.wt(X, method="ML")
    S <- S[[1]]

    difT <- c()
    for (j in 1:p) {
        difT <- c(difT, as.vector(X[,j] - mean(X[,j])))
    }
    difT <- matrix(difT, nrow=n, ncol=p)

    D <- difT%*%solve(S)%*%t(difT)
    b1p <- sum(colSums(D^3))/n^2
    b2p <- sum(diag(D^2))/n


    df = (p*(p+1)*(p+2))/6
    MST = (n*b1p)/6
    P1 = 1 - pchisq(MST,df)

    MKT = (b2p-(p*(p+2)*(n-1)/(n+1)))/(sqrt((8*p*(p+2))/n))
    P2 = 2*(1-pnorm(abs(MKT), 0, 1))

    ## Assemble row of table
    skewness <- list(method = "Mardia's skewness",
                     statistic = MST,
                     df = df,
                     p.value = P1)

    kurtosis <- list(method = "Mardia's kurtosis",
                     statistic = MKT,
                     df = df,
                     p.value = P2)

    result <- rbind(data.frame(),
                    skewness,
                    kurtosis)

    return(result)
}
