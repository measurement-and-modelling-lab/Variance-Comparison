function(dataSub, groupID, groups, varButtons, isDependent) {
    testLabels <- c()
    finalResults <- c()


    hack <- (length(groups) > 1) & (length(varButtons) > 1)

    if (isDependent || hack) {

        if (!hack & length(varButtons) == 1) {
            return(invisible(T))
        }

        ## homogeneity.test <- dget("homogeneity.test.R")

        ## Run tests
        sf <- studentFit(dataSub)
        results <- list(homogeneity.test(sf, test = "LRT"),
                        homogeneity.test(sf, test = "Wald"),
                        homogeneity.test(sf, test = "score"),
                        homogeneity.test(sf, test = "gradient"))

        testLabels <- unlist(lapply(results, '[', 6))

        for(i in 1:length(results)) {
            finalResults[[i]] <- c(testLabels[i],
                                   results[[i]]$statistic,
                                   results[[i]]$parameter,
                                   results[[i]]$p.value)
        }

    } else {

        ## source("leveneTest.R")

        results <- list(bartlett.test(dataSub, groupID),
                        fligner.test(dataSub, groupID),
                        leveneTest(dataSub, groupID),
                        leveneTest(dataSub, groupID, center = mean),
                        leveneTest(dataSub, groupID, center = mean, trim = 0.1),
                        leveneTest(rank(dataSub), groupID, center = mean),
                        leveneTest(rank(dataSub), groupID))

        testLabels <- c("Bartlett's K<sup>2</sup>",
                        "Fligner-Killeen &chi;<sup>2</sup>",
                        "Levene's F (median)",
                        "Levene's F (mean)",
                        "Levene's F (10% trimmed mean)",
                        "Levene's F (nonparametric)",
                        "Levene's F (nonparametric - median)")

        for(i in 1:length(results)) {

            if (i >= 3) {
                finalResults[[i]] <- c(testLabels[i],
                                       results[[i]]$`F value`[1],
                                       paste(results[[i]]$Df, collapse = ", "),
                                       results[[i]]$`Pr(>F)`[1])
            } else {
                finalResults[[i]] <- c(testLabels[i],
                                       results[[i]]$statistic,
                                       results[[i]]$parameter,
                                       results[[i]]$p.value)
            }
        }
    }


    ##bind and round
    finalResults <- do.call(rbind, finalResults)
    finalResults[,c(2,4)] <- round(as.numeric(finalResults[,c(2,4)]), 5)

    ##label tests and columns (extracts test name from results)
    colnames(finalResults) <- c("Test", "Statistic", "df", "p-value")
    finalResults[,1] <- paste0("<b>", finalResults[,1], "</b>")

    ##output
    return(finalResults)
}
