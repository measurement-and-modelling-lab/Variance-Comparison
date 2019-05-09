zhang2005a <- function(x) {
    ## https://github.com/cran/PoweR/blob/master/inst/statsPureR/statsPureR.R

    n <- length(x)
    phi <- pnorm(sort((x - mean(x)) / sd(x)))
    res <- -sum(((log(phi)) / (n - (1:n) + 0.5)) + ((log(1.0 - phi)) / ((1:n) - 0.5)))

    result <- list(method = "Zhang-Wu (2005), Z<sub>A</sub>",
                   doi = "https://doi.org/10.1016/j.csda.2004.05.034",
                   statistic = 10.0 * res - 32.0, ## See page 711 in their paper
                   df = NA,
                   p.value = NA)
    
    return(result) 
}
