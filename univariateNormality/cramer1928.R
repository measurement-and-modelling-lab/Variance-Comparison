cramer1928 <- function (x) 
{
    ## Cramer's (1928) test of normality
    ## https://doi.org/10.1080/03461238.1928.10416862
    ## https://github.com/cran/nortest/blob/master/R/cvm.test.R

    x <- sort(x[complete.cases(x)])
    n <- length(x)

    if (n < 8)
    {
      stop("sample size must be greater than 7")
    }

    p <- pnorm((x - mean(x))/sd(x))
    W <- (1/(12 * n) + sum((p - (2 * seq(1:n) - 1)/(2 * n))^2))
    WW <- (1 + 0.5/n) * W

    if (WW < 0.0275)
    {
      pval <- 1 - exp(-13.953 + 775.5 * WW - 12542.61 * WW^2)
    }
    else if (WW < 0.051)
    {
      pval <- 1 - exp(-5.903 + 179.546 * WW - 1515.29 * WW^2)
    }
    else if (WW < 0.092)
    {
      pval <- exp(0.886 - 31.62 * WW + 10.897 * WW^2)
    }
    else if (WW < 1.1)
    {
      pval <- exp(1.111 - 34.242 * WW + 12.832 * WW^2)
    }
    else
    {
      warning("p-value is smaller than 7.37e-10, cannot be computed more accurately")
      pval <- 7.37e-10
    }

    result <- list(method = "Cramer-von Mises normality test",
                   statistic = W,
                   df = NA,
                   p.value = pval)

    return(result)
}
