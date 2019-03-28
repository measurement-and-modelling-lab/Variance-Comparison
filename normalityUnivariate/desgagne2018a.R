desgagne2018a <- function(x){
    ## https://github.com/cran/PoweR/blob/master/inst/statsPureR/statsPureR.R

    n <- length(x) 
    euler <- - digamma(1)
    z <- (x - mean(x)) / sqrt(var(x) * (n - 1) / n) 
    z <- z[! (z == 0)] 
    B2 <- sum(z ^ 2 * sign(z)) / n 
    K2 <- sum(z ^ 2 * log(abs(z))) / n 
    var.B2 <- (1 / n) * (3.0 - 8.0 / pi) * (1.0 - 1.9 / n) 
    Z.B2 <-  B2 / sqrt(var.B2)
    esp.net.K2 <- ((2.0 - log(2.0) - euler) / 2.0) ^ (1.0 / 3.0) *
        (1 - 1.026 / n) 
    var.net.K2 <- (1.0 / n) * (1.0 / 72.0) *
        ((2.0 - log(2.0) - euler) / 2.0) ^ (-4.0 / 3.0) *
        (3.0 * pi ^ 2 - 28.0) * (1.0 - 2.25 / n ^ 0.8) ;
    Z.net.K2 <- ((K2 - B2 ^ 2) ^ (1.0 / 3.0) - esp.net.K2) /
        sqrt(var.net.K2)
    Xapd.stat <- Z.B2 ^ 2 + Z.net.K2 ^ 2 ;
    p.value <- pchisq(Xapd.stat, 2, lower.tail = FALSE) ;

    result <- list(method = "Desgagne-Micheaux's (2018) X<sub>apd</sub>",
                   doi = "https://doi.org/10.1080/02664763.2017.1415311",
                   statistic = Xapd.stat,
                   df = NA,
                   p.value = pval)
    
    return(result)
}
