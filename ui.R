require(car) || install.packages("car")
require(stats) || install.packages("stats")
require(htmlTable) || install.packages("htmlTable")
require(nortest) || install.packages("nortest")
#require(DescTools) || install.packages("DescTools")
require(TeachingDemos) || install.packages("TeachingDemos")
#require(MVT) || install.packages("MVT")
#require(PoweR) || install.packages("PoweR")

shinyUI(fluidPage(
  
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
    uiOutput("buttons3"),
    uiOutput("buttons4")
  ),
  
  
  
  # assigns output to main panel
  mainPanel(
    htmlOutput("results")
  )
  
))