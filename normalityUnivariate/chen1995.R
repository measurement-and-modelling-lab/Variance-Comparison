chen1995 <- function(x) {
    ## https://github.com/cran/PoweR/blob/master/inst/statsPureR/statsPureR.R

    n <- length(x)
    m <- qnorm(((1:n) - 0.375) / (n + 0.25))
    xo <- sort(x)
    QH <- sum(diff(xo) / diff(m)) / (n - 1) / sd(x); 
    res <- sqrt(n) * (1 - QH)

    result <- list(method = "Chen-Shapiro (1995)",
                   doi = "https://doi.org/10.1080/00949659508811711",
                   statistic = res,
                   df = NA,
                   p.value = NA)
    
    return(result)
}
