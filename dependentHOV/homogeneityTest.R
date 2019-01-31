homogeneityTest <-
function(object, test = "LRT", type = "scale")
{
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

  switch(test,
        LRT = {
          stat <- 2. * (fit$logLik - f0$logLik)
          names(stat) <- "LRT"
          method <- "Likelihood ratio test"
        },
        Wald = {
          phi  <- fit$Scatter[lower.tri(fit$Scatter, diag = TRUE)]
          phi0 <- f0$Scatter[lower.tri(f0$Scatter, diag = TRUE)]
          dif  <- phi - phi0
          Fisher <- fisher.info(fit)
          aCov <- Fisher[-(1:p),-(1:p)]
          rows <- cols <- seq.int(from = 1, length.out = p * (p + 1) / 2)
          acol <- ncol(aCov)
          if (fit$eta != 0.0)
            aCov <- aCov[rows,cols] - outer(aCov[rows,acol], aCov[rows,acol]) / aCov[acol,acol]
          else
            aCov <- aCov[rows,cols]
          R <- chol(aCov)
          dif  <- R %*% dif
          stat <- n * sum(dif^2)
          if (both) {
            dif <- fit$center - f0$center
            aCov <- Fisher[1:p,1:p]
            R <- chol(aCov)
            dif <- R %*% dif
            stat <- stat + n * sum(dif^2)
          }
          names(stat) <- "Wald"
          method <- "Wald test"
        },
        score = {
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
          method <- "Score test"
        },
        gradient = {
          phi  <- fit$Scatter[lower.tri(fit$Scatter, diag = TRUE)]
          phi0 <- f0$Scatter[lower.tri(f0$Scatter, diag = TRUE)]
          dif <- phi - phi0
          Score <- restrictedScore(fit$x, f0$weights, f0$center, f0$Scatter)
          s <- Score[-(1:p)]
          stat <- sum(s * dif)
          if (both) {
            dif <- fit$center - f0$center
            s <- Score[1:p]
            stat <- stat + sum(s * dif)
          }
          names(stat) <- "Gradient"
          method <- "Gradient test"
        },
        stop(paste("unimplemented test:", test))
        )
  df <- p - 1
  if (both)
    df <- df - 1
  pval <- 1 - pchisq(stat, df = df)

  result <- list(method = method,
                 statistic = stat,
                 df = df,
                 p.value = pval)

  return(result)
}

studentFit <-
function(x, data, family = Student(eta = .25), subset, na.action, control = mvt.control())
{
  Call <- match.call()
  if (missing(x))
    stop("'x' is not supplied")
  if (inherits(x, "formula")) {
    mt <- terms(x, data = data)
    if (attr(mt, "response") > 0)
      stop("response not allowed in formula")
    attr(mt, "intercept") <- 0
    mf <- match.call(expand.dots = FALSE)
    names(mf)[names(mf) == "x"] <- "formula"
    mf$family <- mf$control <- NULL
    mf[[1L]] <- as.name("model.frame")
    mf <- eval.parent(mf)
    na.act <- attr(mf, "na.action")
    z <- model.matrix(mt, mf)
  }
  else {
    z <- as.matrix(x)
    if (!missing(subset))
      z <- z[subset, , drop = FALSE]
    if (!missing(na.action))
      z <- na.omit(z)
    else
      z <- na.fail(z)
  }
  if (!is.numeric(z))
    stop("Student.fit applies only to numerical variables")
  znames <- dimnames(z)[[2]]
  dz <- dim(z)
  n <- dz[1]
  p <- dz[2]

  ## initial estimates
  center <- apply(z, 2, mean)
  f <- (n - 1) / n
  Scatter <- f * var(z)
  distances <- mahalanobis(z, center, Scatter)
  
  ## extract family info
  if (!inherits(family, "Student.family"))
    stop("Use only with 'Student.family' objects")
  if (is.null(family$family))
    stop("'family' not recognized")
  kind <- family$kind
  if ((kind < 0) || (kind > 1))
    stop("not valid 'family' object")
  settings <- c(kind, unlist(family$pars))

  ## set control values
  if (missing(control))
    control <- mvt.control()
  ctrl <- unlist(control)
  ctrl <- c(ctrl, 0)

  ## Call fitter
  now <- proc.time()
  fit <- .C("EM_fit",
            z = as.double(t(z)),
            dims = as.integer(dz),
            settings = as.double(settings),
            center = as.double(center),
            Scatter = as.double(Scatter),
            distances = as.double(distances),
            weights = as.double(rep(1, n)),
            logLik = double(1),
            control = as.double(ctrl))
  speed <- proc.time() - now

  ## creating the output object
  out <- list(call = Call,
              x = z,
              dims = dz,
              family = family,
              settings = fit$settings,
              start = list(center = center, Scatter = Scatter),
              center = fit$center,
              Scatter = matrix(fit$Scatter, ncol = p),
              logLik = fit$logLik,
              numIter = fit$control[4],
              control = control,
              weights = fit$weights,
              distances = fit$distances,
              speed = speed,
              converged = FALSE)
  names(out$center) <- znames
  dimnames(out$Scatter) <- list(znames, znames)
  eta <- out$settings[2]
  if (!control$fix.shape && kind != 0) {
    eta <- signif(eta, 4)
    out$family$call <- call(out$family$family, eta = eta)
  }
  out$eta <- out$settings[2]
  if (out$numIter < control$maxIter)
    out$converged <- TRUE
  class(out) <- "studentFit"
  out
}

Student.family <-
function(object, ...)
UseMethod("family")

Student <-
function(eta = 0.25)
{
  cl <- match.call()
  if ((eta < 0 ) || (eta > 1/2))
      stop("eta must be in [0,1/2)")
  pars <- list(eta = eta)
  pnames <- list(eta = "shape parameter")
  kind <- ifelse(eta == 0, 0, 1)
  structure(list(family = "Student",
                 call = cl,
                 pars = pars,
                 pnames = pnames,
                 npars = 1,
                 kind = kind),
            class = "Student.family")
}

mvt.control <-
function(maxIter = 2000, tolerance = 1e-6, fix.shape = FALSE)
{
  list(maxIter = maxIter, tolerance = tolerance, fix.shape = fix.shape)
}

hessian <- function(f, x0, h = .Machine$double.eps^(1/4), ...) {
    if (!is.numeric(x0))
        stop("Argument 'x0' must be a numeric vector.")

    fun <- match.fun(f)
    f <- function(x) fun(x, ...)

    n <- length(x0)
    if (length(f(x0)) != 1)
        stop("Function 'f' must be a univariate function of n variables.")

    if (n == 1)
        return(matrix(fderiv(f, x0, n = 2, h = h), nrow = 1, ncol = 1))

    H <- matrix(NA, nrow = n, ncol = n)
    hh <- diag(h, n)
    for (i in 1:(n-1)) {
        hi <- hh[, i]
        H[i, i] <- (f(x0-hi) - 2*f(x0) + f(x0+hi)) / h^2
        for (j in (i+1):n) {
            hj <- hh[, j]
            H[i, j] <- (f(x0+hi+hj) - f(x0+hi-hj) - f(x0-hi+hj) + f(x0-hi-hj)) / (4*h^2)
            H[j, i] <- H[i, j]
        }
    }
    hi <- hh[, n]
    H[n, n] <- (f(x0-hi) - 2*f(x0) + f(x0+hi)) / h^2

    return(H)
}

fisher.info <-
function(object)
{
    ## local functions
    c.eta <- function(eta) {
        eta / (1 - 2 * eta)
    }
    c.phi <- function(eta, p) {
        (1 + p * eta) / (1 + (p + 2) * eta)
    }
    c.mu <- function(eta, p) {
        c.phi(eta, p) / (1 - 2 * eta)
    }
    beta.dot <- function(eta, p) {
        dif <- trigamma(.5 * (1 + p * eta) / eta) - trigamma(.5 / eta)
        -.5 * dif / eta^2
    }
    N.matrix <- function(n = 2) {
        ## returns a N matrix of order 'n'
        sqr <- n^2
        K <- commutation(n)
        (K + diag(sqr)) / 2
    }
    asSymmetric <- function(x) {
        ## force x to be symmetric
        (x + t(x)) / 2
    }
    
    Scatter <- object$Scatter
    eta <- object$eta

    p <- ncol(Scatter)
    if (nrow(Scatter) != p)
        stop("problem in the estimation algorithm")
    if (!isSymmetric(Scatter))
        Scatter <- asSymmetric(Scatter)
    
    Dp <- duplication(p)
    Np <- N.matrix(p)

    inv.Scatter <- solve(Scatter)
    if (!isSymmetric(inv.Scatter))
        inv.Scatter <- asSymmetric(inv.Scatter)
    vec.Scatter <- as.vector(inv.Scatter)

    ## Fisher information matrix about 'center'
    fisher.center <- c.mu(eta, p) * inv.Scatter
    if (!isSymmetric(fisher.center))
        fisher.center <- asSymmetric(fisher.center)

    ## Fisher information matrix about 'Scatter'
    fisher.Scatter <- 2 * c.phi(eta, p) * kronecker(inv.Scatter, inv.Scatter) %*% Np
    fisher.Scatter <- fisher.Scatter + (c.phi(eta, p) - 1) * outer(vec.Scatter, vec.Scatter)
    fisher.Scatter <- .25 * crossprod(Dp, fisher.Scatter %*% Dp)
    if (!isSymmetric(fisher.Scatter))
        fisher.Scatter <- asSymmetric(fisher.Scatter)

    ## crossed Fisher information about 'Scatter' and 'eta'
    fisher.cross <- -(c.eta(eta) * (p + 2)) / ((1 + p * eta) * (1 + (p + 2) * eta))
    fisher.cross <- fisher.cross * crossprod(Dp, vec.Scatter)

    ## Fisher information about 'eta'
    fisher.eta <- 1 + p * eta * (1 - 4 * eta) - 8 * eta^2
    fisher.eta <- fisher.eta / ((1 + p * eta) * (1 + (p + 2) * eta))
    fisher.eta <- fisher.eta * p / (1 - 2 * eta)^2
    fisher.eta <- -.5 * (fisher.eta - beta.dot(eta, p)) / eta^2
    
    ## forming the Fisher information matrix
    fisher <- matrix(0, nrow = p + ncol(Dp) + 1, ncol = p + ncol(Dp) + 1)
    fisher[1:p, 1:p] <- fisher.center
    rows <- cols <- seq.int(from = p + 1, length.out = ncol(Dp))
    fisher[rows, cols] <- fisher.Scatter
    cols <- ncol(fisher)
    fisher[rows, cols] <- fisher[cols, rows] <- fisher.cross
    fisher[cols, cols] <- fisher.eta
    
    fisher
}

duplication <-
function(n = 1)
{   ## returns a duplication matrix with order 'n'
    ## posted at R-help by Charles Berry, 2006-09-09
    n <- as.integer(n)
    mat <- diag(n)
    index <- seq(n * (n + 1) / 2)
    mat[lower.tri(mat, TRUE)] <- index
    mat[upper.tri(mat)] <- t(mat)[upper.tri(mat)]
    outer(c(mat), index, function(x, y) ifelse(x == y, 1, 0))
}



commutation <-
function(n = 2)
{   ## returns a commutation matrix with order 'n' (only for square matrices)
    ## fast algorithm by Feng Li <Feng.Li@stat.su.se>, 2012-03-14
    n <- as.integer(n)
    sqr <- n^2
    mat <- matrix(0, nrow = sqr, ncol = sqr)
    m0 <- seq_len(sqr)
    n0 <- as.vector(t(matrix(m0, nrow = n, ncol = n)))
    ind <- cbind(m0, n0)
    dims <- c(sqr, sqr)
    indMat <- matrix(ind, ncol = length(dims))
    idx1 <- cumprod(dims[-length(dims)])
    idx2 <- indMat[, -1, drop = FALSE] - 1
    idxRaw <- rowSums(idx1 * idx2) + indMat[, 1]    
    mat[idxRaw] <- 1
    mat
}
