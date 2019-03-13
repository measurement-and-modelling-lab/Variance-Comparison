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
