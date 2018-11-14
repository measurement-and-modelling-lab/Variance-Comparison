shinyServer(function(input, output, session) {
  
  values <- reactiveValues()
  values$scores <- ""
  values$header <- ""
  
  # runs tests and outputs results
  output$results <- reactive ({
    # check for necessary inputs
    validate(need(input$varButtons, ""))
    validate(need(input$file, ""))
    
    # error checking and output functions
    errors <- dget("errors.R")
    source("htmlTable.R")
    # test functions
    varTests <- dget("varianceTests.R")
    shapeTests <- dget("shapeTests.R")
    normalityTests <- dget("normalityTests.R")
    
    data <- values$scores
    nrow <- nrow(data)
    data[,as.numeric(input$groupingVar)] <- as.factor(data[,as.numeric(input$groupingVar)])
    data <- as.numeric(data)
    data <- matrix(data = data, nrow = nrow)
    
    #extracts variables based on check boxes
    dataSub <- data[data[, as.numeric(input$groupingVar)] 
                    %in% as.numeric(input$groups), as.numeric(input$varButtons)]
    groupID <- data[data[, as.numeric(input$groupingVar)] 
                    %in% as.numeric(input$groups), as.numeric(input$groupingVar)]
    groupID <- factor(groupID)
    
    if (input$isDependent & (input$test == "shape") & (length(input$groups) > 2)) {
      blockVector <- data[data[, as.numeric(input$groupingVar)] 
                          %in% as.numeric(input$groups), as.numeric(input$block)]
    }
    else { blockVector <- NA }
    
    errors(dataSub)
    
    # tests for variance
    if (input$test == "variance") {
      
      # stops if too many groups for dependent samples test
      if (input$isDependent & (length(input$groups) > 1)) { stop("Select only one group for dependent samples test.") }
      
      # function call for variance tests with error catching
      tryCatch(
        output <- varTests(dataSub, groupID, input$groups, input$varButtons, input$isDependent),
        error = function(e) stop("An error occurred."))
      # exits if correct inputs not selected
      if(output[1] == T) { return() }
      
      # output format
      htmlTable(output, align = "lccc")
    } else if (input$test == "shape") {
      
      # function call for shape tests with error catching
      tryCatch(
        output <- shapeTests(dataSub, groupID, input$groups, input$isDependent, blockVector),
        error = function(e) stop("An error occurred."))
      # exits if correct inputs not selected
      if(output[1] == T) { return() }
      
      # format output
      htmlTable(output, align = "lccc")
    } else if (input$test == "normality") {
      
      # funtion call for normality tests with error catching
      tryCatch(
        output <- normalityTests(dataSub, groupID, input$groups, input$varButtons),
        error = function(e) stop("An error occurred."))
      # exits if correct inputs not selected
      if(output[1] == T) { return() }
      
      # format output
      htmlTable(output, align = "lccc")
    } else {
      # exit if nothing selected
      return()
    }
  })
  
  
  # select the grouping variable
  output$buttons1 <- reactive ({
    
    # checks for uploaded file and reads as matrix
    validate(need(input$file, ""))
    file <- input$file[[4]]
    data <- as.matrix(read.csv(file, header = F))
    
    # creates variable with number of columns
    cols <- ncol(data)
    
    if ((cols - 1) > 16) { stop("Too many variables")}
    
    # checks for row and column names
    # assigns data and names to scores and header (numbers cols if no names)
    if (is.character(data)) {
      values$header <- data[1,]
      values$scores <- data[-1,]
      data <- data[-1,]
    } else {
      values$scores <- data
      values$header <- 1:cols
    }
    
    # creates a named list of numbers for check boxes
    choices <- 1:cols
    names(choices) <- values$header
    # sends to UI
    HTML(paste0(radioButtons("groupingVar", label = "Grouping Variable",
                             choices = choices)))
  })
  
  # select which groups to use
  output$buttons2 <- reactive ({
    
    # checks for uploaded file and reads as matrix
    validate(need(input$groupingVar, ""))
    
    # extracts and factors grouping variable
    groups <- values$scores[,as.numeric(input$groupingVar)]
    groups <- unique(groups)
    groupFactor <- as.numeric(as.factor(groups))
    
    if(groups[1] > 16) { stop("Too many groups") }
    
    # to check if variable continuous
    # figure out a better way to do this
    if (length(groups) == nrow(values$scores)) { 
      stop("There is only one participant per group!")
    }
    
    names(groupFactor) <- groups
    HTML(paste0(checkboxGroupInput("groups", label = "Groups",
                                   choices = groupFactor)))
  })
  
  # select dependent variables
  output$buttons3 <- reactive ({
    
    # checks for uploaded file and reads as matrix
    validate(need(input$groups, ""))
    
    # removes grouping variable from choices
    choices <- (1:ncol(values$scores))[-as.numeric(input$groupingVar)]
    names(choices) <- values$header[-as.numeric(input$groupingVar)]
    
    # allows for selection of multiple variables if only one group is selected
    if (length(input$groups) == 1) {
      HTML(paste0(checkboxGroupInput("varButtons", label = "Variables",
                                     choices = choices)))
    } else {
      HTML(paste0(radioButtons("varButtons", label = "Variables",
                               choices = choices)))
    }
  })
  
  # choose blocks
  output$buttons4 <- reactive ({
    validate(need(input$varButtons, ""))
    
    if (input$isDependent & (input$test == "shape") & (length(input$groups) > 2)){
      choices <- (1:ncol(values$scores))[-c(as.numeric(input$groupingVar), as.numeric(input$varButtons))]
      names(choices) <- values$header[-c(as.numeric(input$groupingVar), as.numeric(input$varButtons))]
      
      HTML(paste0(radioButtons("block", label = "Block",
                               choices = choices)))
    }
  })
})
