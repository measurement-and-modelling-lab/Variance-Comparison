skewness <- function(x, na.rm = FALSE, type = 3) {
    if(any(ina <- is.na(x))) {
        if(na.rm) 
            x <- x[!ina]
        else
            return(NA)
    }

    if(!(type %in% (1 : 3)))
        stop("Invalid 'type' argument.")
    
    n <- length(x)
    x <- x - mean(x)
    y <- sqrt(n) * sum(x ^ 3) / (sum(x ^ 2) ^ (3/2))
    if(type == 2) {
        if(n < 3)
            stop("Need at least 3 complete observations.")
        y <- y * sqrt(n * (n - 1)) / (n - 2)
    } else if(type == 3)
        y <- y * ((1 - 1 / n)) ^ (3/2)

    y
}

kurtosis <- function(x, na.rm = FALSE, type = 3) {
    if(any(ina <- is.na(x))) {
        if(na.rm) 
            x <- x[!ina]
        else
            return(NA)
    }
    
    if(!(type %in% (1 : 3)))
        stop("Invalid 'type' argument.")
    
    n <- length(x)
    x <- x - mean(x)
    r <- n * sum(x ^ 4) / (sum(x ^ 2) ^ 2)
    y <- if(type == 1)
             r - 3
         else if(type == 2) {
             if(n < 4)
                 stop("Need at least 4 complete observations.")
             ((n + 1) * (r - 3) + 6) * (n - 1) / ((n - 2) * (n - 3))
         }
         else
             r * (1 - 1 / n) ^ 2 - 3

    y
}

henze1990 <- function(data, cov = TRUE, tol = 1e-25){
    ## Henze and Zerkler's (1990) test of multivariate normality
    ## http://dx.doi.org/10.1080/03610929008830400 
    ## https://github.com/selcukorkmaz/MVN/blob/master/R/mvn.R

    dataframe=as.data.frame(data)
    dname <- deparse(substitute(data))
    data <- data[complete.cases(data),]
    data <- as.matrix(data)
    n <- dim(data)[1]
    p <- dim(data)[2]
    data.org <- data

    if (cov){
        S <- ((n-1)/n)*cov(data)
    }
    else    {
        S <- cov(data)
    }

    dif <- scale(data, scale = FALSE)

    Dj <- diag(dif%*%solve(S, tol = tol)%*%t(dif))  #squared-Mahalanobis' distances

    Y <- data%*%solve(S, tol = tol)%*%t(data)


    Djk <- - 2*t(Y) + matrix(diag(t(Y)))%*%matrix(c(rep(1,n)),1,n) + matrix(c(rep(1,n)),n,1)%*%diag(t(Y))

    b <- 1/(sqrt(2))*((2*p + 1)/4)^(1/(p + 4))*(n^(1/(p + 4))) #smoothing
    {                                                                 #parameter
        if (qr(S)$rank == p){
            HZ = n * (1/(n^2) * sum(sum(exp( - (b^2)/2 * Djk))) - 2 *
                      ((1 + (b^2))^( - p/2)) * (1/n) * (sum(exp( - ((b^2)/(2 *
                                                                           (1 + (b^2)))) * Dj))) + ((1 + (2 * (b^2)))^( - p/2)))
        }
        else {
            HZ = n*4
        }

    }
    wb <- (1 + b^2)*(1 + 3*b^2)

    a <- 1 + 2*b^2

    mu <- 1 - a^(- p/2)*(1 + p*b^2/a + (p*(p + 2)*(b^4))/(2*a^2)) #HZ mean

    si2 <- 2*(1 + 4*b^2)^(- p/2) + 2*a^( - p)*(1 + (2*p*b^4)/a^2 + (3*p*
                                                                    (p + 2)*b^8)/(4*a^4)) - 4*wb^( - p/2)*(1 + (3*p*b^4)/(2*wb) + (p*
                                                                                                                                   (p + 2)*b^8)/(2*wb^2)) #HZ variance

    pmu <- log(sqrt(mu^4/(si2 + mu^2))) #lognormal HZ mean
    psi <- sqrt(log((si2 + mu^2)/mu^2)) #lognormal HZ variance

    pValue <- 1 - plnorm(HZ,pmu,psi) #P-value associated to the HZ statistic

    MVN = ifelse(pValue > 0.05, "YES", "NO")

    result <- list(method = "Henze-Zirkler (1990)",
                   statistic = HZ,
                   df = NA,
                   p.value = pValue)

    return(result)
}
