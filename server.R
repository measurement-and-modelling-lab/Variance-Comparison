shinyServer(function(input, output, session) {

    ## Global variables
    values <- reactiveValues()
    values$scores <- ""
    values$colnames <- ""
    univariateNormalityTests <- list("agostino1971" = 1,
                                                           "agostino1973" = 2,
                                                           "anderson1954" = 3,
                                                           "barrio1999" = 4,
                                                           "bonett2002" = 5,
                                                           "bonettseier2002" = 6,
                                                           "bontemps2005a" = 7,
                                                           "bontemps2005b" = 8,
                                                           "cabana1994a" = 9,
                                                           "cabana1994b" = 10,
                                                           "chen1995" = 11,
                                                           "coin2008" = 12,
                                                           "cramer1928" = 13,
                                                           "desgagne2013" = 14, 
                                                           "desgagne2018a" = 15, 
                                                           "desgagne2018b" = 16, 
                                                           "doornik1994" = 17,
                                                           "epps1983" = 18,
                                                           "filliben1975" = 19,
                                                           "gel2007" = 20, 
                                                           "gel2008" = 21,
                                                           "glen2001" = 22, 
                                                           "hosking1990a" = 23,
                                                           "hosking1990b" = 24,
                                                           "hosking1990c" = 25,
                                                           "hosking1990d" = 26,
                                                           "jarque1980" = 27,
                                                           "lilliefors1967" = 28,
                                                           "martinez1981" = 29,
                                                           "pearson1900" = 30,
                                                           "rahman1997" = 31,
                                                           "shapiro1965" = 32,
                                                           "shapiro1972" = 33,
                                                           "spiegelhalter1977" = 34,
                                                           "zhang1999a" = 35,
                                                           "zhang1999b" = 36,
                                                           "zhang2005a" = 37,
                                                           "zhang2005b" = 38)

    ## Create a checkbox ui element for choosing a grouping variable
    output$chooseGroupingVar <- reactive ({

        ## Ensure that a file has been uploaded
        validate(need(input$file, ""))

        source("hasHeader.R")

        ## Read data as a matrix
        file <- input$file[[4]]
        include_header <- hasHeader(file)
        values$scores <- read.csv(file, header=include_header)

        ## Create a variable list
        choices <- 1:ncol(values$scores)
        names(choices) <- colnames(values$scores)

        ## Output ui element
        HTML(paste0(radioButtons("groupingVar",
                                 label = "Grouping Variable",
                                 choices = choices)))
    })


    ## Select which groups to use
    output$chooseGroups <- reactive ({

        ## Ensure that a grouping variable has been selected
        validate(need(input$groupingVar, ""))

        ## Extract and grouping variable
        groupingVar <- as.numeric(input$groupingVar)
        groups <- values$scores[,groupingVar]
        groups <- unique(groups)

        ## Output ui element
        HTML(paste0(checkboxGroupInput("groups",
                                       label = "Groups",
                                       choices = groups)))
    })

    ## Choose the dependent variables
    output$chooseDV <- reactive ({

        ## Ensure that groups have been selected
        validate(need(input$groups, ""))

        ## Construct a list of variables, excluding the grouping variable
        groupingVar <- as.numeric(input$groupingVar)
        p <- ncol(values$scores)
        choices <- (1:p)[-groupingVar]

        ## Output ui element
        HTML(paste0(checkboxGroupInput("outcomeVar",
                                       label = "Variables",
                                       choices = choices)))
    })
    
    output$chooseUnivariateNormalityTests <- reactive ({
      validate(need(length(input$groups) == 1, ""))
      validate(need(length(input$outcomeVar) == 1 , ""))
      HTML(paste0(selectInput("selectedTests",
                              label = "Select Tests",
                              choices = names(univariateNormalityTests),
                              multiple = TRUE),
                  checkboxInput("selectAllButton",
                                label = "Run All",
                                value = FALSE)))
    })
    ## Run tests and output results
    output$results <- reactive ({

        ## Check for necessary inputs
        validate(need(input$outcomeVar, ""))
        validate(need(input$file, ""))

        ## Fetch necessary values
        groupingVar <- as.numeric(input$groupingVar)
        variables <- as.numeric(input$outcomeVar)
        groups <- as.numeric(input$groups)
        data <- values$scores

        ## Subset the data
        data <- data[data[,groupingVar] == groups, ]
        values <- data[, variables]
        groups <- data[,groupingVar]

        if (input$test == "hov") {

            source("./hovDependent/hovDependent.R")
            source("./hovIndependent/hovIndependent.R")
            
            dependent <- length(variables) > 1 & length(input$groups) >= 1
            independent <- length(variables) == 1 & length(input$groups) > 1

            if (dependent) {
                table <- hovDependent(values, groups)
            } else if (independent) {
                table <- hovIndependent(values, groups)
            } else {
                return()
            }

        } else if (input$test == "normality") {

            source("./normalityUnivariate/normalityUnivariate.R")
            source("./normalityMultivariate/normalityMultivariate.R")

            univariate <- length(variables) == 1
            one_group <- length(input$groups) == 1

            if (univariate & one_group) {
                validate(need(input$selectAllButton == TRUE | input$selectAllButton == FALSE, ""))
                if(!input$selectAllButton){
                  validate(need(input$selectedTests, ""))
                }
                selectionList <- c()
                for(name in input$selectedTests){
                  selectionList <- c(selectionList,get(name,univariateNormalityTests))
                }
                table <- newNormalityUnivariate(values,selectionList,input$selectAllButton)
            } else if (!univariate & one_group) {
                table <- normalityMultivariate(values)
            } else if (!univariate & !one_group) {
                table <- rbind(mardia1970_omnibus_skewness(values, groups),
                               mardia1970_omnibus_kurtosis(values, groups))
            } else {
                return()
            }

        } else if (input$test == "shape") {

            source("./shapeUnivariate/shapeUnivariate.R")
            source("./shapeMultivariate/shapeMultivariate.R")

            univariate <- length(input$groups) > 1 & length(variables) == 1
            multivariate <- length(input$groups) == 1 & length(variables) > 1

            if (univariate) {
                table <- shapeUnivariate(values, groups)
            } else if (multivariate) {
                table <- shapeMultivariate(values)
            } else {
                return()
            }

        }

        ## Make test names link to doi
        table$method <- paste0("<a href=", table$doi, ">", table$method, "</a>")
        table$doi <- NULL

        ## Generate nice table
        colnames(table) <- c("Method", "Statistic", "df", "p-value")
        source("tablegen.R")
        output <- tableGen(table)

        ## Link to critical value tables
        if (input$test == "normality" & length(variables) == 1 & length(input$groups) == 1) {
            output <- paste0(output, "Note: Decision tables for tests without p-values are located
                                      <a href=\"critical_values.html\"><font color=\"#00ca8a\">here</font></a>.")
        }

        HTML(output)
        
    })
})
