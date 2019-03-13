mardia1970_omnibus_skewness <- function(X, grouping) {
    ## Mardia (1970)
    ## https://doi.org/10.1093/biomet/57.3.519

    groups <- split(X, grouping)
    j <- length(groups)

    skewness_output <- data.frame()
    for (i in 1:j) {
        new_row <- mardia1970_skewness(groups[[j]])
        skewness_output <- rbind(skewness_output, new_row)
    }
    skewness.chisq <- sum(skewness_output$statistic) ## stat = sum of chi squares
    skewness.df <- sum(skewness_output$df) ## df = sum of df
    skewness.p <- 1 - pchisq(skewness.chisq, skewness.df)

    result <- data.frame(method = "Mardia (1970) omnibus skewness test",
                         statistic = skewness.chisq,
                         df = skewness.df,
                         p.value = skewness.p)

    result$statistic <- round(result$statistic, 2)
    result$p.value <- round(result$p.value, 2)

    return(result)
}
