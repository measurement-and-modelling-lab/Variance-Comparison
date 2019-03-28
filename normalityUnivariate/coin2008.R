coin2008 <- function(x) {
    ## https://github.com/cran/PoweR/blob/master/inst/statsPureR/statsPureR.R

    normorder <- function(n){
        dt <- 0.01
        t <- seq(-10, 10, dt)
        int <- function(r) {
            rinv <- n - r + 1
            logh <- lgamma(n + 1) - lgamma(rinv) - lgamma(n - rinv + 1) +
                (rinv - 1) * pnorm(t, low = FALSE, log.p = TRUE) + (n - rinv) * pnorm(t, low = TRUE, log.p = TRUE) +
                dnorm(t, log = TRUE) 
            h <- t * exp(logh)
            return(sum(h) * dt)
        }
        res <- apply(cbind(1:n), 1, int)
        return(res)
    }
    n <- length(x)
    a <- normorder(n)
    x <- sort(x)
    z <- (x - mean(x)) / sd(x)
    mod <- lm(z ~ 0 + a + I(a ^ 3))
    stat <- mod$coef[2] ^ 2
    
    result <- list(method = "Coin (2008)",
                   doi = "https://doi.org/10.1016/j.csda.2007.07.012",
                   statistic = stat,
                   df = NA,
                   p.value = NA)

    return(result)
}
