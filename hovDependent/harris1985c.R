harris1985c <- function(object, type = "scale"){
    ## Harris (1985)
    ## https://doi.org/10.2307/2336339
    ## https://github.com/cran/MVT/blob/master/R/homogeneity.test.R 
    
    source("./hovDependent/functions/fisherinfo.R")
    source("./hovDependent/functions/duplication.R")
    
    ## local functions
    homogeneityFit <-
        function(z, dims, settings, center, Scatter, control, type)
    {
        ctrl <- unlist(control)
        ctrl <- c(ctrl, type, 0)
        n <- dims[1]
        p <- dims[2]
        Phi <- cor(z)
        sigma2 <- sum(diag(solve(Phi, Scatter))) / p
        Scatter <- sigma2 * Phi
        distances <- mahalanobis(z, center, Scatter)
        o <- .C("variance_homogeneity",
                z = as.double(t(z)),
                dims = as.integer(dims),
                settings = as.double(settings),
                center = as.double(center),
                Scatter = as.double(Scatter),
                sigma2 = as.double(sigma2),
                Phi = as.double(Phi),
                distances = as.double(distances),
                weights = as.double(rep(1, n)),
                logLik = double(1),
                control = as.double(ctrl))
        o$Scatter <- matrix(o$Scatter, ncol = p)
        o$Phi <- matrix(o$Phi, ncol = p)
        o$eta <- o$settings[2]
        o$numIter <- o$control[5]
        o
    }
    restrictedScore <-
        function(z, weights, center, Scatter)
    {
        n <- nrow(z)
        p <- ncol(z)
        z <- sweep(z, 2, center)
        y <- weights * z
        s.center <- apply(y, 2, sum)
        s.center <- solve(f0$Scatter, s.center)
        y <- sqrt(weights) * z
        S <- crossprod(y)
        s.Scatter <- solve(Scatter, S) - n * diag(p)
        s.Scatter <- s.Scatter %*% solve(Scatter)
        Dp <- duplication(p)
        s.Scatter <- .5 * crossprod(Dp, as.vector(s.Scatter))
        s <- c(s.center, as.vector(s.Scatter))
        s
    }
    
    both <- pmatch(type, table = c("scale", "both")) - 1
    if (is.na(both))
        stop("not valid 'type' argument")
    
    ## extract object info
    fit <- object
    if (!inherits(fit, "studentFit"))
        stop("Use only with 'studentFit' objects")
    n <- fit$dims[1]
    p <- fit$dims[2]
    
    ## restricted parameter estimation
    f0 <- homogeneityFit(fit$x, fit$dims, fit$settings, fit$start$center, fit$start$Scatter, fit$control, both)
    null.fit <- list(center = f0$center, sigma2 = f0$sigma2, Phi = f0$Phi, eta = f0$eta, distances = f0$distances,
                     weights = f0$weights, logLik = f0$logLik, numIter = f0$numIter)
    
    
    Score <- restrictedScore(fit$x, f0$weights, f0$center, f0$Scatter)
    Fisher <- fisher.info(f0)
    s <- Score[-(1:p)]
    aCov <- Fisher[-(1:p),-(1:p)]
    rows <- cols <- seq.int(from = 1, length.out = p * (p + 1) / 2)
    acol <- ncol(aCov)
    if (f0$eta != 0.0)
        aCov <- aCov[rows,cols] - outer(aCov[rows,acol], aCov[rows,acol]) / aCov[acol,acol]
    else
        aCov <- aCov[rows,cols]
    R <- chol(aCov)
    s <- solve(t(R), s)
    stat <- sum(s^2) / n
    if (both) {
        aCov <- Fisher[1:p,1:p]
        s <- Score[1:p]
        R <- chol(aCov)
        s <- solve(t(R), s)
        stat <- stat + sum(s^2) / n
    }
    names(stat)<-"Score"
    
    df <- p - 1
    if (both)
        df <- df - 1
    pval <- 1 - pchisq(stat, df = df)
    
    result <- list(method = "Harris (1985) score test",
                   statistic = stat,
                   df = df,
                   p.value = pval)
    
    return(result)
    
}
