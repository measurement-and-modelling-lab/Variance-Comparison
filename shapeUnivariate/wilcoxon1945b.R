wilcoxon1945b <- function(x, y = NULL, alternative = c("two.sided", "less", "greater"),
                         mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
                         conf.int = FALSE, conf.level = 0.95, ...) {
  ## Wilcoxon's (1945) test of equality of distribution
  ## https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/wilcox.test.R
  ## Wilcoxon (1945) rank-sum test
  
  alternative <- match.arg(alternative)
  if(!missing(mu) && ((length(mu) > 1L) || !is.finite(mu))) {
    stop("'mu' must be a single number")
  }
  if(conf.int) {
    if(!((length(conf.level) == 1L)
         && is.finite(conf.level)
         && (conf.level > 0)
         && (conf.level < 1)))
      stop("'conf.level' must be a single number between 0 and 1")
  }
  
  if(!is.numeric(x)) stop("'x' must be numeric")
  if(!is.null(y)) {
    if(!is.numeric(y)) stop("'y' must be numeric")
    DNAME <- paste(deparse(substitute(x)), "and",
                   deparse(substitute(y)))
    if(paired) {
      if(length(x) != length(y))
        stop("'x' and 'y' must have the same length")
      OK <- complete.cases(x, y)
      x <- x[OK] - y[OK]
      y <- NULL
    }
    else {
      x <- x[is.finite(x)]
      y <- y[is.finite(y)]
    }
  } else {
    DNAME <- deparse(substitute(x))
    if(paired)
      stop("'y' is missing for paired test")
    x <- x[is.finite(x)]
  }
  
  if(length(x) < 1L)
    stop("not enough (finite) 'x' observations")
  CORRECTION <- 0
  
  if(length(y) < 1L)
    stop("not enough 'y' observations")
  METHOD <- "Wilcoxon (1945) rank-sum test"
  r <- rank(c(x - mu, y))
  n.x <- as.double(length(x))
  n.y <- as.double(length(y))
  if(is.null(exact))
    exact <- (n.x < 50) && (n.y < 50)
  STATISTIC <- c("W" = sum(r[seq_along(x)]) - n.x * (n.x + 1) / 2)
  TIES <- (length(r) != length(unique(r)))
  if(exact && !TIES) {
    PVAL <-
      switch(alternative,
             "two.sided" = {
               p <- if(STATISTIC > (n.x * n.y / 2))
                 pwilcox(STATISTIC - 1, n.x, n.y, lower.tail = FALSE)
               else
                 pwilcox(STATISTIC, n.x, n.y)
               min(2 * p, 1)
             },
             "greater" = {
               pwilcox(STATISTIC - 1, n.x, n.y, lower.tail = FALSE)
             },
             "less" = pwilcox(STATISTIC, n.x, n.y))
    if(conf.int) {
      ## Exact confidence interval for the location parameter
      ## mean(x) - mean(y) in the two-sample case (cf. the
      ## one-sample case).
      alpha <- 1 - conf.level
      diffs <- sort(outer(x, y, "-"))
      cint <-
        switch(alternative,
               "two.sided" = {
                 qu <- qwilcox(alpha/2, n.x, n.y)
                 if(qu == 0) qu <- 1
                 ql <- n.x*n.y - qu
                 achieved.alpha <- 2*pwilcox(trunc(qu)-1,n.x,n.y)
                 c(diffs[qu], diffs[ql + 1])
               },
               "greater" = {
                 qu <- qwilcox(alpha, n.x, n.y)
                 if(qu == 0) qu <- 1
                 achieved.alpha <- pwilcox(trunc(qu)-1,n.x,n.y)
                 c(diffs[qu], +Inf)
               },
               "less" = {
                 qu <- qwilcox(alpha, n.x, n.y)
                 if(qu == 0) qu <- 1
                 ql <- n.x*n.y - qu
                 achieved.alpha <- pwilcox(trunc(qu)-1,n.x,n.y)
                 c(-Inf, diffs[ql + 1])
               })
      if (achieved.alpha-alpha > alpha/2) {
        warning("Requested conf.level not achievable")
        conf.level <- 1 - achieved.alpha
      }
      attr(cint, "conf.level") <- conf.level
      ESTIMATE <- c("difference in location" = median(diffs))
    }
  }
  else {
    NTIES <- table(r)
    z <- STATISTIC - n.x * n.y / 2
    SIGMA <- sqrt((n.x * n.y / 12) *
                    ((n.x + n.y + 1)
                     - sum(NTIES^3 - NTIES)
                     / ((n.x + n.y) * (n.x + n.y - 1))))
    if(correct) {
      CORRECTION <- switch(alternative,
                           "two.sided" = sign(z) * 0.5,
                           "greater" = 0.5,
                           "less" = -0.5)
      METHOD <- paste(METHOD, "with continuity correction")
    }
    z <- (z - CORRECTION) / SIGMA
    PVAL <- switch(alternative,
                   "less" = pnorm(z),
                   "greater" = pnorm(z, lower.tail=FALSE),
                   "two.sided" = 2 * min(pnorm(z),
                                         pnorm(z, lower.tail=FALSE)))
    if(conf.int) {
      ## Asymptotic confidence interval for the location
      ## parameter mean(x) - mean(y) in the two-sample case
      ## (cf. one-sample case).
      ##
      ## Algorithm not published, for a documentation see the
      ## one-sample case.
      alpha <- 1 - conf.level
      mumin <- min(x) - max(y)
      mumax <- max(x) - min(y)
      wdiff <- function(d, zq) {
        dr <- rank(c(x - d, y))
        NTIES.CI <- table(dr)
        dz <- (sum(dr[seq_along(x)])
               - n.x * (n.x + 1) / 2 - n.x * n.y / 2)
        CORRECTION.CI <-
          if(correct) {
            switch(alternative,
                   "two.sided" = sign(dz) * 0.5,
                   "greater" = 0.5,
                   "less" = -0.5)
          } else 0
        SIGMA.CI <- sqrt((n.x * n.y / 12) *
                           ((n.x + n.y + 1)
                            - sum(NTIES.CI^3 - NTIES.CI)
                            / ((n.x + n.y) * (n.x + n.y - 1))))
        if (SIGMA.CI == 0)
          stop("cannot compute confidence interval when all observations are tied", call.=FALSE)
        (dz - CORRECTION.CI) / SIGMA.CI - zq
      }
      root <- function(zq) {
        ## in extreme cases we need to return endpoints,
        ## e.g.  wilcox.test(1, 2:60, conf.int=TRUE)
        f.lower <- wdiff(mumin, zq)
        if(f.lower <= 0) return(mumin)
        f.upper <- wdiff(mumax, zq)
        if(f.upper >= 0) return(mumax)
        uniroot(wdiff, c(mumin, mumax),
                f.lower = f.lower, f.upper = f.upper,
                tol = 1e-4, zq = zq)$root
      }
      cint <- switch(alternative,
                     "two.sided" = {
                       l <- root(zq = qnorm(alpha/2, lower.tail = FALSE))
                       u <- root(zq = qnorm(alpha/2))
                       c(l, u)
                     },
                     "greater" = {
                       l <- root(zq = qnorm(alpha, lower.tail = FALSE))
                       c(l, +Inf)
                     },
                     "less" = {
                       u <- root(zq = qnorm(alpha))
                       c(-Inf, u)
                     })
      attr(cint, "conf.level") <- conf.level
      correct <- FALSE # no continuity correction for estimate
      ESTIMATE <- c("difference in location" =
                      uniroot(wdiff, c(mumin, mumax), tol = 1e-4,
                              zq = 0)$root)
    }
    
    if(exact && TIES) {
      warning("cannot compute exact p-value with ties")
      if(conf.int)
        warning("cannot compute exact confidence intervals with ties")
    }
  }
  
  names(mu) <- if(paired || !is.null(y)) "location shift" else "location"
    result <- list(method = "Wilcoxon (1945) rank-sum test", ## won't say 'with continuity correction'
                   doi = "https://doi.org/10.230/3001968",
                   statistic = STATISTIC,
                   df = NA,
                   p.value = as.numeric(PVAL))
  
  return(result)
}
