function(data, groupingVar, groups, varButtons, isDependent) {
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
  
  testLabels <- c()
  finalResults <- c()
  
  if (isDependent == T) {
    
    #if (length(varButtons) == 1) { return(invisible(T)) }
    
    if (length(groups) == 2) {
      #tests
      results <- list(wilcox.test(dataSub ~ groupID, paired = TRUE))
    } else {
      return(invisible(T))
    }
    
    testLabels <- unlist(lapply(results, '[', 6))
    
    for(i in 1:length(results)) {
      finalResults[[i]] <- c(testLabels[i], results[[i]]$statistic, ifelse(is.null(results[[i]]$parameter), "N/A", results[[i]]$parameter), 
                             results[[i]]$p.value)
    }
  }
  else {
    
    if (length(groups) == 2) {
      results <- list(wilcox.test(dataSub ~ groupID))
      print(results)
      str(results)
    } else {
      results <- list(kruskal.test(dataSub ~ groupID))
      print(results)
      str(results)
    }
    
    testLabels <- results[[1]]$method
    
    for(i in 1:length(results)) {
        finalResults[[i]] <- c(testLabels[i], results[[i]]$statistic, ifelse(is.null(results[[i]]$parameter), "N/A", results[[i]]$parameter), 
                               results[[i]]$p.value)
    }
  }
  
  
  
  #bind and round
  finalResults <- matrix(finalResults[[1]], nrow=1)
  print(finalResults)
  finalResults[,c(2,4)] <- round(as.numeric(finalResults[,c(2,4)]), 5)
  
  #label tests and columns (extracts test name from results)
  colnames(finalResults) <- c("Test", "Statistic", "df", "p-value")
  finalResults[,1] <- paste0("<b>", finalResults[,1], "</b>")
  
  #output
  return(finalResults)
}