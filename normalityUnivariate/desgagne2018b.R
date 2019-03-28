desgagne2018b <- function(x){
    ## https://github.com/cran/PoweR/blob/master/inst/statsPureR/statsPureR.R

    n <- length(x) 
    euler <- -digamma(1)
    z <- (x - mean(x)) / sqrt(var(x) * (n - 1) / n) 
    z <- z[! (z == 0)] 
    K2 <- sum(z ^ 2 * log(abs(z))) / n 
    alpha.n <- -0.06 + 2.1 / n ^ 0.67
    K2.box.cox <- ((2.0 * K2) ^ alpha.n - 1.0) / alpha.n
    esp.K2.box.cox <- - ((2.0 - log(2.0) - euler) ^ (-0.06) - 1.0) / 0.06 -
        1.32 / n ^ 0.95
    var.K2.box.cox <- (1.0 / n) * ((2.0 - log(2.0) - euler) ^ (-2.12) *
                                   (3.0 * pi ^ 2 - 28.0) / 2.0 - 3.78 / n ^ 0.733)
    stat.Z.K2 <- (K2.box.cox - esp.K2.box.cox) / sqrt(var.K2.box.cox)
    p.value <- 2.0 * pnorm(abs(stat.Z.K2), lower.tail = FALSE)

    result <- list(method = "Desgagne-Micheaux's (2018) Z<sub>epd</sub>",
                   doi = "https://doi.org/10.1080/02664763.2017.1415311",
                   statistic = stat.Z.K2,
                   df = NA,
                   p.value = p.value)
    
    return(result)
}
