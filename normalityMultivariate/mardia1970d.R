mardia1970_omnibus_kurtosis <- function(X, grouping) {
    ## Mardia (1970)
    ## https://doi.org/10.1093/biomet/57.3.519

    groups <- split(X, grouping)
    j <- length(groups)

    kurtosis_output <- data.frame()
    for (i in 1:j) {
        new_row <- mardia1970_kurtosis(groups[[j]])
        kurtosis_output <- rbind(kurtosis_output, new_row)
    }
    kurtosis.chisq <- sum(kurtosis_output$statistic^2) ## stat = sum of squared Z scores
    kurtosis.df <- j
    kurtosis.p <- 1 - pchisq(kurtosis.chisq, kurtosis.df)

    result <- data.frame(method = "Mardia (1970) omnibus kurtosis test",
                         statistic = kurtosis.chisq,
                         df = kurtosis.df,
                         p.value = kurtosis.p)

    result$statistic <- round(result$statistic, 2)
    result$p.value <- round(result$p.value, 2)

    return(result)
}
