function(data, groupingVar, groups, varButtons) {
  nrow <- nrow(data)
  data[,as.numeric(groupingVar)] <- as.factor(data[,as.numeric(groupingVar)])
  data <- as.numeric(data)
  data <- matrix(data = data, nrow = nrow)
  
  #extracts variables based on check boxes
  dataSub <- data[data[, as.numeric(groupingVar)] 
                  %in% as.numeric(groups), as.numeric(varButtons)]
  groupID <- data[data[, as.numeric(groupingVar)] 
                  %in% as.numeric(groups), as.numeric(groupingVar)]
  groupID <- factor(groupID)
  
  errors <- dget("errors.R")
  errors(dataSub)
  
  testLabels <- c()
  finalResults <- c()
  
  if (length(groups) == 1 & length(varButtons) == 1) {
    
    #tests
    # ad.test requires more than 7 values, add error
    results <- list(ks.test(dataSub, "pnorm"), shapiro.test(dataSub), ad.test(dataSub), ShapiroFranciaTest(dataSub), cvm.test(dataSub))
    
    for(i in 1:length(results)) {
      finalResults[[i]] <- c(results[[i]]$method, results[[i]]$statistic, ifelse(!exists("results[[i]]$parameter"), "N/A", results[[i]]$parameter), 
                             results[[i]]$p.value)
    }
  }
  else {
    
    dataSub <- split(dataSub, groupID)
    dataSub <- lapply(dataSub, function(x) as.matrix(x, ncol = varButtons))
    MultivariateSK <- dget("MultivariateSK.R")
    
    results <- MultivariateSK(dataSub)
    
    skew.table <- results[[1]]
    chisum <- sum(skew.table[,3])
    dfsum <- sum(skew.table[,4])
    psum <- 1 - pchisq(chisum, dfsum)
    topRow <- c("Mardia's &chi; (Skewness)", chisum, dfsum, psum)
    
    kurt.table <- results[[2]]
    chisum <- sum(kurt.table[,3])
    dfsum <- nrow(kurt.table)
    psum <- 1 - pchisq(chisum, dfsum)
    bottomRow <- c("Mardia's &chi; (Kurtosis)", chisum, dfsum, psum)
    
    finalResults <- list(topRow, bottomRow)
  }
  
  #bind and round
  finalResults <- do.call(rbind, finalResults)
  finalResults[,c(2,4)] <- round(as.numeric(finalResults[,c(2,4)]), 5)
  
  #label tests and columns (extracts test name from results)
  colnames(finalResults) <- c("Test", "Statistic", "df", "p-value")
  finalResults[,1] <- paste0("<b>", finalResults[,1], "</b>")
  
  #output
  return(finalResults)
}