# load required packages
require(MVT) || install.packages("MVT")
require(car) || install.packages("car")
require(stats) || install.packages("stats")
require(htmlTable) || install.packages("htmlTable")
require(nortest) || install.packages("nortest")
require(DescTools) || install.packages("DescTools")
require(TeachingDemos) || install.packages("TeachingDemos")
require(PoweR) || install.packages("PoweR")

server <- function(input, output) {
  
  values <- reactiveValues()
  values$scores <- ""
  values$header <- ""
  
  # runs tests and outputs results
  output$results <- reactive ({
    # check for necessary inputs
    validate(need(input$varButtons, ""))
    validate(need(input$file, ""))
    
    # tests for variance
    if (input$test == "variance") {
      # loads variance function
      tests <- dget("varianceTests.R")
    
      # function call for variance tests
      varOutput <- tests(values$scores, input$groupingVar, input$groups, input$varButtons, input$isDependent)
      # exits if correct inputs not selected
      if(varOutput == T) { return() }
      
      # output format
      htmlTable(varOutput, align = "lccc")
    } else if (input$test == "shape") {
      # loads tests for shape
      tests <- dget("shapeTests.R")
      
      # function call for shape tests
      varOutput <- tests(values$scores, input$groupingVar, input$groups, input$varButtons, input$isDependent)
      # exits if correct inputs not selected
      if(varOutput == T) { return() }
      
      # format output
      htmlTable(varOutput, align = "lccc")
    } else if (input$test == "normality") {
      # loads tests for normality
      tests <- dget("normalityTests.R")
      
      # funtion call for normality tests
      varOutput <- tests(values$scores, input$groupingVar, input$groups, input$varButtons)
      # exits if correct inputs not selected
      if(varOutput == T) { return() }
      
      # format output
      htmlTable(varOutput, align = "lccc")
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
    
    if(groups > 16) { stop("Too many groups") }

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
}

# creates UI
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Patua+One');
                    body { min-width: 450px };
                    th { color: #808080 }"))),
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
                    body { min-width: 450px };
                    th { color: #808080 }
                    "))
  ),
  
  headerPanel("", windowTitle = ""),
  
  # adds selection buttons to side panel
  sidebarPanel(
    # dropdown for choosing test type
    selectInput("test", label = "Choose calculation", 
                choices = list("Homogeneity of variance" = "variance", "Shape" = "shape", 
                               "Normality" = "normality")),
    # data file loading
    fileInput("file", label = "File input"),
    # select box for dependent samples
    checkboxInput("isDependent", label = "Dependent samples?", value = TRUE),
    # grouping variable
    uiOutput("buttons1"),
    # selection of groups
    uiOutput("buttons2"),
    # dependent variables
    uiOutput("buttons3")
    ),
  
  # assigns output to main panel
  mainPanel(
    htmlOutput("results")
  )
  
)

shinyApp(ui = ui, server = server)
