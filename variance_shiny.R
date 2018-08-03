require(MVT) || install.packages("MVT")
require(car) || install.packages("car")
require(stats) || install.packages("stats")
require(htmlTable) || install.packages("htmlTable")

server <- function(input, output) {
  
  values <- reactiveValues()
  values$scores <- ""
  values$header <- ""
  
  #runs tests and outputs results
  output$results <- reactive ({
    if (input$test == "variance") {
      varianceTests <- dget("varianceTests.R")
    
      #check for necessary inputs
      validate(need(input$varButtons, ""))
      validate(need(input$file, ""))
    
      varOutput <- varianceTests(values$scores, input$groupingVar, input$groups, input$varButtons)
      if(varOutput == T) { return() }
    
      htmlTable(varOutput, align = "lccc")
    }
    else {
      return()
    }
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
  
  sidebarPanel(
    selectInput("test", label = "Choose calculation", 
                choices = list("Homogeneity of variance" = "variance", "Shape" = "shape")),
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
