levene1960 <- function (y, group, center=median, ...) {
    ## Leven's (1960) test of homogeneity of variances
    ## https://github.com/cran/car/blob/master/R/leveneTest.R

    if (!is.numeric(y)) {
        stop(deparse(substitute(y)), " is not a numeric variable")
    }

    if (!is.factor(group)) {
        group <- as.factor(group)
    }

    valid <- complete.cases(y, group)
    meds <- tapply(y[valid], group[valid], center, ...)
    resp <- abs(y - meds[group])
    table <- anova(lm(resp ~ group))[, c(1, 4, 5)]

    df <- paste(table$Df, collapse=", ")
    statistic <- table$F[1]
    p.value <- table$"Pr(>F)"[1]

    result <- list(method = "Levene (1960)",
                   doi = "https://doi.org/10.1137/1003016",
                   statistic = statistic,
                   df = df,
                   p.value = p.value)

    return(result)
}
