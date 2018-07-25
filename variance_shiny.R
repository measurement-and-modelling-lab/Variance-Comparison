server <- function(input, output) {
  
  values <- reactiveValues()
  values$scores <- ""
  values$header <- ""
  
  #runs tests and outputs results
  output$results <- reactive ({
    
    #import html to format table
    formatOut <- dget("tablegen.R")
    
    #check for necessary inputs
    validate(need(input$varButtons, ""))
    validate(need(input$file, ""))
    
    #read file
    data <- values$scores
    data[,as.numeric(input$groupingVar)] <- as.factor(data[,as.numeric(input$groupingVar)])
    data <- as.numeric(data)
    data <- matrix(data = data, nrow = nrow(values$scores))
    
    #extracts variables based on check boxes
    dataSub <- data[data[, as.numeric(input$groupingVar)] 
                    %in% as.numeric(input$groups), as.numeric(input$varButtons)]
    
    testLabels <- c()
    finalResults <- c()
    
    if (length(input$groups) == 1) {
      
      if (length(input$varButtons) == 1) { return() }
      
      sf <- studentFit(dataSub)
      
      #tests
      results <- list(homogeneity.test(sf, test = "LRT"), homogeneity.test(sf, test = "Wald"), 
                      homogeneity.test(sf, test = "score"), homogeneity.test(sf, test = "gradient"))
      
      testLabels <- unlist(lapply(results, '[', 6))
      
      for(i in 1:length(results)) {
        finalResults[[i]] <- c(testLabels[i], results[[i]]$statistic, results[[i]]$parameter, 
                               results[[i]]$p.value)
      }
    }
    else {
      groupID <- data[data[, as.numeric(input$groupingVar)] 
                      %in% as.numeric(input$groups), as.numeric(input$groupingVar)]
      groupID <- factor(groupID)
      
      results <- list(bartlett.test(dataSub, groupID), fligner.test(dataSub, groupID), 
                      leveneTest(dataSub, groupID), leveneTest(dataSub, groupID, center = mean), 
                      leveneTest(dataSub, groupID, center = mean, trim = 0.1), 
                      leveneTest(rank(dataSub), groupID, center = mean), leveneTest(rank(dataSub), groupID))
      
      testLabels <- c("Bartlett's K<sup>2</sup>", "Fligner-Killeen &chi;<sup>2</sup>", "Levene's F (median)",
                      "Levene's F (mean)", "Levene's F (10% trimmed mean)", "Levene's F (nonparametric)", 
                      "Levene's F (nonparametric - median)")
      
      for(i in 1:length(results)) {
        if (i >= 3) {
          finalResults[[i]] <- c(testLabels[i], results[[i]]$`F value`[1], paste(results[[i]]$Df, collapse = ", "), 
                                 results[[i]]$`Pr(>F)`[1])
        }
        else {
          finalResults[[i]] <- c(testLabels[i], results[[i]]$statistic, results[[i]]$parameter, results[[i]]$p.value)
        }
      }
    }
    
    
    
    #bind and round
    finalResults <- do.call(rbind, finalResults)
    finalResults[,c(2,4)] <- round(as.numeric(finalResults[,c(2,4)]), 5)
    
    #label tests and columns (extracts test name from results)
    colLabels <- c("Test", "Statistic", "df", "p-value")
    
    #output
    finalResults <- as.matrix(rbind(colLabels, finalResults))
    capture.output(formatOut(finalResults, T))
  })
  
  
  
  output$buttons1 <- reactive ({
    
    #checks for uploaded file and reads as matrix
    validate(need(input$file, ""))
    file <- input$file[[4]]
    data <- as.matrix(read.csv(file, header = F))
    
    cols <- ncol(data)
    if (is.character(data)) {
      values$header <- data[1,]
      values$scores <- data[-1,]
      data <- data[-1,]
    }
    else {
      values$scores <- data
      values$header <- 1:cols
    }
    
    #creates a named list of numbers for check boxes
    choices <- 1:cols
    names(choices) <- values$header
    #sends to UI
    HTML(paste0(radioButtons("groupingVar", label = "Grouping Variable",
                       choices = choices)))
  })
  
  output$buttons2 <- reactive ({
    
    #checks for uploaded file and reads as matrix
    validate(need(input$groupingVar, ""))
    
    groups <- values$scores[,as.numeric(input$groupingVar)]
    groups <- unique(groups)
    groupFactor <- as.numeric(as.factor(groups))

    #to check if variable continuous
    if (length(groups) == nrow(values$scores)) { 
      return()
      }
    #groups <- sort(groups)
    names(groupFactor) <- groups
    HTML(paste0(checkboxGroupInput("groups", label = "Groups",
                                                          choices = groupFactor)))
  })
  
  output$buttons3 <- reactive ({
    
    #checks for uploaded file and reads as matrix
    validate(need(input$groups, ""))
    
    choices <- (1:ncol(values$scores))[-as.numeric(input$groupingVar)]
    names(choices) <- values$header[-as.numeric(input$groupingVar)]
    
    if (length(input$groups) == 1) {
      HTML(paste0(checkboxGroupInput("varButtons", label = "Variables",
                                                            choices = choices)))
    }
    else {
      HTML(paste0(radioButtons("varButtons", label = "Variables",
                                                      choices = choices)))
    }
  })
}


ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Patua+One');
                    h1 {
                    font-family: 'Patua One';
                    font-weight: bold;
                    line-height: 1.1;
                    color: #333;
                    }
                    td {
                      white-space: nowrap;
                      width: 1px;
                      padding-left: 8px;
                      padding-right: 8px;
                      padding-top: 0px;
                      padding-bottom: 0px;
                      color: #717171;
                    }
                    "))
  ),
  
  headerPanel("", windowTitle = ""),
  
  sidebarPanel(
    fileInput("file", label = "File input"),
    uiOutput("buttons1"),
    uiOutput("buttons2"),
    uiOutput("buttons3")
    ),
  
  mainPanel(
    htmlOutput("results")
  )
  
)

shinyApp(ui = ui, server = server)
