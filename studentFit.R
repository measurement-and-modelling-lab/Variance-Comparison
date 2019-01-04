homogeneity.test <-
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

  ## output object
  dimnames(f0$Scatter) <- dimnames(fit$Scatter)
  z <- list(statistic = stat, parameter = df, p.value = pval, estimate = fit$Scatter,
            null.value = f0$Scatter, method = method, null.fit = null.fit)
  if (is.null(fit$call$data))
    z$data <- fit$call$x
  else
    z$data <- fit$call$data
  z$family <- fit$family
  class(z) <- "homogeneity.test"
  z
}

print.homogeneity.test <- function(x, digits = 4, ...)
{
  ## local functions
  print.symmetric <-
  function(z, digits = digits, ...)
  {
    ll <- lower.tri(z, diag = TRUE)
    z[ll] <- format(z[ll], ...)
    z[!ll] <- ""
    print(z, ..., quote = F)
  }

  cat("\n")
  cat(paste(x$method, "for equality of variances", sep = " "), "\n")
  cat("\n")
  cat("data:", x$data, "\n")
  out <- character()
  out <- c(out, paste(names(x$statistic), "statistic =",
                      format(round(x$statistic, digits = digits))))
  out <- c(out, paste("df =", x$parameter))
  out <- c(out, paste("p-value =", format(round(x$p.value, digits = digits))))
  cat(strwrap(paste(out, collapse = ", ")), sep = "\n")
  cat("alternative hypothesis: true variances are not equal.\n")
  #print(x$null.value, ...)
  cat("\n")
  cat("sample estimate:\n")
  print.symmetric(x$estimate, digits = digits)
  invisible(x)
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

print.studentFit <-
function(x, digits = 4, ...)
{
  ## local functions
  print.symmetric <-
  function(z, digits = digits, ...)
  {
    ll <- lower.tri(z, diag = TRUE)
    z[ll] <- format(z[ll], ...)
    z[!ll] <- ""
    print(z, ..., quote = F)
  }
  cat("Call:\n")
  x$call$family <- x$family$call
  dput(x$call, control = NULL)
  if (x$converged)
    cat("Converged in", x$numIter, "iterations\n")
  else
    cat("Maximum number of iterations exceeded")
  cat("\nCenter:\n ")
  print(format(round(x$center, digits = digits)), quote = F, ...)
  cat("\nScatter matrix estimate:\n")
  if (x$dims[2] <= 5)
    print.symmetric(x$Scatter, digits = digits)
  else {
    print.symmetric(x$Scatter[1:5,1:5], digits = digits)
    cat("...")
  }
  nobs <- x$dims[1]
  cat("\nNumber of Observations:", nobs, "\n")
  invisible(x)
}

Student.family <-
function(object, ...)
UseMethod("family")

print.Student.family <-
function (x, ...) 
cat(" Family:", deparse(x$call), "\n")

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



fit.st <- function(data, ...){
  #if(is.timeSeries(data)) data <- series(data)
  mu <- mean(data)
  m2 <- mean((data - mu)^2)
  m4 <- mean((data - mu)^4)
  nu <- 4 + (6 * m2^2) / (m4 - 3 * m2^2)
  sigma <- sqrt((nu - 2) * m2 / nu)
  theta <- c(nu, mu, sigma)
  negloglik <- function(theta, y){
    - sum(log(dt((y - theta[2]) / abs(theta[3]), df = abs(theta[1]))) - log(abs(theta[3])))
  }
  optimfit <- optim(theta, fn = negloglik, y = data, ...)
  par.ests <- optimfit$par
  ifelse(optimfit$convergence == 0, converged <- TRUE, converged <- FALSE)
  par.ests[1] <- abs(par.ests[1])
  par.ests[2] <- abs(par.ests[2])
  nItheta <- hessian(negloglik, par.ests, y = data)
  asymp.cov <- solve(nItheta)
  loglh.max <- -negloglik(par.ests, y = data)
  par.ses <- sqrt(diag(asymp.cov))
  names(par.ests) <- c("nu", "mu", "sigma")
  names(par.ses) <- names(par.ests)
  dimnames(asymp.cov) <- list(names(par.ests), names(par.ests))
  list(converged = converged, par.ests = par.ests, par.ses = par.ses,
       asymp.cov = asymp.cov, ll.max = loglh.max)
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


## https://github.com/cran/MVT/blob/master/src/init.c
## https://github.com/cran/MVT/blob/master/src/fitter.c

## library(dplyr)
## read.csv("~/example1_data1.csv") %>%
##     filter(groups == 1) %>%
##     select(a, b) %>%
##     studentFit()
