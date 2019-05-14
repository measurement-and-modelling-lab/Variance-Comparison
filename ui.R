require(shiny) || install.packages(shiny)
require(shinythemes) || install.packages(shinythemes)

shinyUI(fluidPage(theme = "simplex.css",

  HTML('<link rel="stylesheet" type="text/css" href="index.css">'),
  
  headerPanel("", windowTitle = ""),
  
  sidebarPanel(
    selectInput("test", label = "Choose calculation", 
                choices = list("Homogeneity of variance" = "hov",
                               "Shape comparison" = "shape",
                               "Normality" = "normality")),
    fileInput("file", label = "File input"),
    uiOutput("chooseGroupingVar"),
    uiOutput("chooseGroups"),
    uiOutput("chooseDV"),
    uiOutput("chooseUnivariateNormalityTests")),
  
  mainPanel(
    htmlOutput("results"))))