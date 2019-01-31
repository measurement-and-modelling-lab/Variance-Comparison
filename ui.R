require(shiny) || install.packages(shiny)
require(shinythemes) || install.packages(shinythemes)

shinyUI(fluidPage(theme = "simplex.css",

  HTML('<link rel="stylesheet" type="text/css" href="index.css">'),
  
  headerPanel("", windowTitle = ""),
  
  sidebarPanel(
    selectInput("test", label = "Choose calculation", 
                choices = list("Homogeneity of variance" = "hov",
                               "Shape" = "shape", 
                               "Normality" = "normality")),
    checkboxInput("has_header", "Does your data have a header?", value = FALSE),
    fileInput("file", label = "File input"),
    uiOutput("chooseGroupingVar"),
    uiOutput("chooseGroups"),
    uiOutput("chooseDV")),
  
  mainPanel(
    htmlOutput("results"))))
