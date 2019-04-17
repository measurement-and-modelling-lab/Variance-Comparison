zhang2005b <- function(x) {
    ## https://github.com/cran/PoweR/blob/master/inst/statsPureR/statsPureR.R

    n <- length(x)
    phi <- pnorm(sort((x - mean(x)) / sd(x)))
    res <- sum((log((1 / phi - 1) / ((n - 0.5) / ((1:n) - 0.75) - 1))) ^ 2)

    result <- list(method = "Zhang-Wu (2005), Z<sub>C</sub>",
                   doi = "https://doi.org/10.1016/j.csda.2004.05.034",
                   statistic = res,
                   df = NA,
                   p.value = NA)
    
    return(result)
}
