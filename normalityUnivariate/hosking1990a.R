hosking1990a <- function(x) {
    ## The 1st Hosking test for normality(1990)
    ## https://doi.org/10.1111/j.2517-6161.1990.tb01775.x
    ## https://github.com/cran/PoweR/blob/master/src/laws-stats/stats/stat10.cpp

    x <- sort(x)
    n <- length(x)
    b0 <- b1 <- b2 <- b3 <- l2 <- l3 <- l4 <- vtau <- vtau3 <- vtau4 <-0
    tmp1 <- n*(n-1)
    tmp2 <- tmp1 * (n-2)
    tmp3 <- tmp2 * (n-3)
    
    b0 <- b0 + x[1] + x[2] + x[3]
    b1 <- b1 + (1 * x[2]) + (2* x[3])
    b2 <- b2 + (2*x[3])
    
    for(i in 4:n){
        b0 <- b0 + x[i]
        b1 <- b1 + (i-1)*x[i]
        b2 <- b2 +((i-1)*(i-2)) * x[i]
        b3 <- b3 + ((i-1)*(i-2)*(i-3))* x[i]
    }
    b0 <- b0/n
    b1 <- b1/tmp1 
    b2 <- b2/tmp2
    b3 <- b3/tmp3
    
    l2 <- 2* b1 - b0
    l3 <- 6 * b2 - 6*b1+b0
    l4 <- (20 * b3) - (30 *b2) + (12 * b1) -b0
    
    tau3 <- l3/l2
    tau4 <- l4/l2
    
    
    ## Table 1 page 563 of Romao et al.
    if ((1 <= n)& (n<=25)){
        mutau4 <- 0.12383
        vtau3 <- 0.0088038
        vtau4 <- 0.0049295
    }
    
    if ((25 < n) & (n <= 50)) {
        mutau4 <- 0.12321;
        vtau3 <- 0.0040493;
        vtau4 <- 0.0020802;
    }
    
    if (n > 50) {
        mutau4 <- 0.12291;
        vtau3 <- 0.0019434;
        vtau4 <- 0.00095785;
    }
    statTLmom <- tau3^2 /  vtau3 + (tau4 - mutau4)^2 / vtau4
    PVAL <- 1 - pchisq(statTLmom, 2)
    
    result <- list(method = "Hosking (1990), #1",
                   statistic = statTLmom,
                   df = 2,
                   p.value = PVAL)
    return(result)
    
}
