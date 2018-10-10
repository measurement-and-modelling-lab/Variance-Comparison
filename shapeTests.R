function(dataSub, groupID, groups, isDependent, block) {
  testLabels <- c()
  finalResults <- c()
  
  if (isDependent == T) {
    if (length(groups) == 2) {
      #tests
      results <- list(wilcox.test(dataSub ~ groupID, paired = TRUE))
      testLabels <- unlist(lapply(results, '[', 6))
    } else {
      results <- list(friedman.test(dataSub, groupID, block))
      testLabels <- "Friedman Rank Sum Test"
    }
    
    for(i in 1:length(results)) {
      finalResults[[i]] <- c(testLabels, results[[i]]$statistic, ifelse(is.null(results[[i]]$parameter), "N/A", results[[i]]$parameter), 
                             results[[i]]$p.value)
    }
  }
  else {
    
    if (length(groups) == 2) {
      results <- list(wilcox.test(dataSub ~ groupID))
    } else if(length(groups) > 2) {
      results <- list(kruskal.test(dataSub ~ groupID))
    } else {
      return(invisible(T))
    }
    
    testLabels <- results[[1]]$method
    
    for(i in 1:length(results)) {
        finalResults[[i]] <- c(testLabels[i], results[[i]]$statistic, ifelse(is.null(results[[i]]$parameter), "N/A", results[[i]]$parameter), 
                               results[[i]]$p.value)
    }
  }
  
  
  
  #bind and round
  finalResults <- matrix(finalResults[[1]], nrow=1)
  finalResults[,c(2,4)] <- round(as.numeric(finalResults[,c(2,4)]), 5)
  
  #label tests and columns (extracts test name from results)
  colnames(finalResults) <- c("Test", "Statistic", "df", "p-value")
  finalResults[,1] <- paste0("<b>", finalResults[,1], "</b>")
  
  #output
  return(finalResults)
}