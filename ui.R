require(shiny) || install.packages(shiny)
require(shinythemes) || install.packages(shinythemes)

shinyUI(fluidPage(theme = "simplex.css",

  HTML('<br>
  
    <link rel="stylesheet" type="text/css" href="index.css">
  <style>
    html {
       overflow-y: scroll;
       }
       </style>
    <title>Analytics^2 - About</title>
         <div class="bar">
    <b class="title">Measurement and Modelling Lab &nbsp; - &nbsp; Tools</b><br class="rwd-break"><b class="link">
    <a href="https://shiny.rcg.sfu.ca/u/tmustaph/MML-R2/"><font color="white">MML-R2</font></a>
    &emsp;&nbsp;<a href="https://shiny.rcg.sfu.ca/u/tmustaph/MML-Multicorr/"><font color="white">MML-Multicorr</font></a>
    &emsp;&nbsp;<a href="https://shiny.rcg.sfu.ca/u/tmustaph/MML-WBCORR/"><font color="white">MML-WBCORR</font></a>
    &emsp;&nbsp;<a href="https://shiny.rcg.sfu.ca/u/tmustaph/csv-generator/"><font color="white">CSV Generator</font></a>
    &emsp;&nbsp;<a href="https://shiny.rcg.sfu.ca/u/tmustaph/distribution-tests/"><font color="#00ca8a">Distribution Tests</font></a>





        </b>
        </div>
         
         
         
  '),
                  
  HTML("<br>"),
                  
                  
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Patua+One');
                    h1 {
                      font-family: 'Patua One';
                      font-weight: bold;
                      line-height: 1.1;
                      color: #333;
                    }
                    body { min-width: 450px; }
                    sub { vertical-align: 25%; font-size: 70%; }
                    th {
                      white-space: nowrap;
                      width: 1px;
                      padding-left: 8px;
                      padding-right: 8px;
                      padding-top: 0px;
                      padding-bottom: 0px;
                      color: #717171;
                    }
                    input[type=number]::-webkit-outer-spin-button,
                    input[type=number]::-webkit-inner-spin-button {
                        -webkit-appearance: none;
                        margin: 0;
                    }
                    input[type=number] {
                        -moz-appearance:textfield;
                    }
                    .shiny-file-input-progress {
                      margin-top: -28px !important;
                      margin-left: 85px !important;
                      z-index:1000;
                      width: -webkit-calc(100% - 94px);
                      width:    -moz-calc(100% - 94px);
                      width:         calc(100% - 94px);
                      opacity: 1;
                      position:relative;
                    }
                    .noheader {
                      padding-left: 8px;
                      padding-right: 8px;
                    }
    "))
  ),
  headerPanel("", windowTitle = "Distribution Tests"),
  sidebarLayout(
  
  sidebarPanel(
    selectInput("test", label = "Choose calculation", 
                choices = list("Homogeneity of variance" = "hov",
                               "Shape comparison" = "shape",
                               "Normality" = "normality")),
    fileInput("file", label = "File input"),
    uiOutput("chooseGroupingVar"),
    uiOutput("chooseGroups"),
    uiOutput("chooseDV")),
  
    mainPanel(
      tabsetPanel(
        id = "inTabset",
        #tabPanel(value = "about", "About", includeHTML("./documentation/about.html")),
        tabPanel(value = "output", "Output", htmlOutput("results"))
      )
    )
  ),
  HTML('<br>'),
  HTML('
    <link rel="stylesheet" type="text/css" href="index.css">
    <div class="bar2">
     <b class="bottom">
     <font color="#717171">Provided by the</font>
     <a href="http://members.psyc.sfu.ca/labs/mml"><font color=white>Measurement and Modelling Lab</font></a>
     <font color="#717171"> at</font>
     <a href="https://www.sfu.ca/"><font color=white> SFU</font></a>
     </b>
     </div><br>')
  
  
))
