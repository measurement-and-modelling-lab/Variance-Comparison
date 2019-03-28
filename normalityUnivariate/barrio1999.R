barrio1999 <- function(x){
    ## https://github.com/cran/PoweR/blob/master/inst/statsPureR/statsPureR.R

    n <- length(x)
    xo <- sort(x)
    dt <- 0.000001
    k <- 1:n
    sigma <- 0
    for (i in k){
        u <- seq((k[i] - 1) / n + dt / 2, k[i] / n - dt / 2, dt)
        sigma <- sigma +  sum(qnorm(u)) * xo[i] * dt
    }
    stat <- 1 - sigma ^ 2 / (var(x) * (n - 1) / n)
    
    result <- list(method = "Barrio et al. (1999)",
                   doi = "https://doi.org/10.1214/aos/1017938923",
                   statistic = stat,
                   df = NA,
                   p.value = NA)

    return(result)
}
