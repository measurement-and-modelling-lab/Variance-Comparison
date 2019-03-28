bonett2002 <-function (x) {
    ## Bonett-Seier (2002)
    
    x <- sort(x)
    n <- length(x)
    rho <- sqrt(sum((x - mean(x)) ^ 2) / n)
    tau <- sum(abs(x - mean(x))) / n
    omega <- 13.29 * (log(rho) - log(tau))
    z <- sqrt(n + 2) * (omega - 3) / 3.54
    pval <- 2* pnorm(z, lower.tail = FALSE)
        if (pval > 1) 
            pval <- 2 - pval

    result <- list(method = "Bonett-Seier (2002)",
                   doi = "https://doi.org/10.1016/S0167-9473(02)00074-9",
                   statistic = z,
                   df = NA,
                   p.value = pval)

    return(result)

}
