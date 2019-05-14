normalityUnivariate <- function(data) {
  source("./normalityUnivariate/agostino1971.R")
  source("./normalityUnivariate/agostino1973.R")
  source("./normalityUnivariate/anderson1954.R")
  source("./normalityUnivariate/barrio1999.R")
  source("./normalityUnivariate/bonett2002.R")
  source("./normalityUnivariate/bonettseier2002.R")
  source("./normalityUnivariate/bontemps2005a.R")
  source("./normalityUnivariate/bontemps2005b.R")
  source("./normalityUnivariate/cabana1994a.R")
  source("./normalityUnivariate/cabana1994b.R")
  source("./normalityUnivariate/chen1995.R")
  source("./normalityUnivariate/coin2008.R")
  source("./normalityUnivariate/cramer1928.R")
  source("./normalityUnivariate/desgagne2013.R")
  source("./normalityUnivariate/desgagne2018a.R")
  source("./normalityUnivariate/desgagne2018b.R")
  source("./normalityUnivariate/doornik1994.R")
  source("./normalityUnivariate/epps1983.R")
  source("./normalityUnivariate/filliben1975.R")
  source("./normalityUnivariate/gel2007.R")
  source("./normalityUnivariate/gel2008.R")
  source("./normalityUnivariate/glen2001.R")
  source("./normalityUnivariate/hosking1990a.R")
  source("./normalityUnivariate/hosking1990b.R")
  source("./normalityUnivariate/hosking1990c.R")
  source("./normalityUnivariate/hosking1990d.R")
  source("./normalityUnivariate/jarque1980.R")
  source("./normalityUnivariate/lilliefors1967.R")
  source("./normalityUnivariate/martinez1981.R")
  source("./normalityUnivariate/pearson1900.R")
  source("./normalityUnivariate/rahman1997.R")
  source("./normalityUnivariate/shapiro1965.R")
  source("./normalityUnivariate/shapiro1972.R")
  source("./normalityUnivariate/spiegelhalter1977.R")
  source("./normalityUnivariate/zhang1999a.R")
  source("./normalityUnivariate/zhang1999b.R")
  source("./normalityUnivariate/zhang2005a.R")
  source("./normalityUnivariate/zhang2005b.R")
    ## Test the normality of a vector of scores

    ## Load tests

    ## Run each test and assemble as data frame
    output <- rbind(data.frame(),
                    agostino1971(data),
                    agostino1973(data),
                    anderson1954(data),
                    barrio1999(data),
                    bonett2002(data),
                    bonettseier2002(data),
                    bontemps2005a(data),
                    bontemps2005b(data),
                    cabana1994a(data),
                    cabana1994b(data),
                    chen1995(data),
                    coin2008(data),
                    cramer1928(data),
                    desgagne2013(data), 
                    desgagne2018a(data), 
                    desgagne2018b(data), 
                    doornik1994(data),
                    epps1983(data),
                    filliben1975(data),
                    gel2007(data), 
                    gel2008(data),
                    glen2001(data), 
                    hosking1990a(data),
                    hosking1990b(data),
                    hosking1990c(data),
                    hosking1990d(data),
                    jarque1980(data),
                    lilliefors1967(data),
                    martinez1981(data),
                    pearson1900(data),
                    rahman1997(data),
                    shapiro1965(data),
                    shapiro1972(data),
                    spiegelhalter1977(data),
                    zhang1999a(data),
                    zhang1999b(data),
                    zhang2005a(data),
                    zhang2005b(data))

    ## Format output
    output$statistic <- round(output$statistic, 2)
    output$p.value <- round(output$p.value, 2)

    ## Return output
    return(output)
    
}
data<- read.csv("data.csv")
data <- data[,1]

## List of all functions available for univariate normality testing

## test selection list
selected <- c(1,2,5,7,9)

## Get names from funcMapping list
for(name in names(funcMapping)){
  ##print(get(name,funcMapping))
}

## Only run and ouput selected functions
newNormalityUnivariate <- function(data,selectedFunctions,selectAll){
  source("./normalityUnivariate/agostino1971.R")
  source("./normalityUnivariate/agostino1973.R")
  source("./normalityUnivariate/anderson1954.R")
  source("./normalityUnivariate/barrio1999.R")
  source("./normalityUnivariate/bonett2002.R")
  source("./normalityUnivariate/bonettseier2002.R")
  source("./normalityUnivariate/bontemps2005a.R")
  source("./normalityUnivariate/bontemps2005b.R")
  source("./normalityUnivariate/cabana1994a.R")
  source("./normalityUnivariate/cabana1994b.R")
  source("./normalityUnivariate/chen1995.R")
  source("./normalityUnivariate/coin2008.R")
  source("./normalityUnivariate/cramer1928.R")
  source("./normalityUnivariate/desgagne2013.R")
  source("./normalityUnivariate/desgagne2018a.R")
  source("./normalityUnivariate/desgagne2018b.R")
  source("./normalityUnivariate/doornik1994.R")
  source("./normalityUnivariate/epps1983.R")
  source("./normalityUnivariate/filliben1975.R")
  source("./normalityUnivariate/gel2007.R")
  source("./normalityUnivariate/gel2008.R")
  source("./normalityUnivariate/glen2001.R")
  source("./normalityUnivariate/hosking1990a.R")
  source("./normalityUnivariate/hosking1990b.R")
  source("./normalityUnivariate/hosking1990c.R")
  source("./normalityUnivariate/hosking1990d.R")
  source("./normalityUnivariate/jarque1980.R")
  source("./normalityUnivariate/lilliefors1967.R")
  source("./normalityUnivariate/martinez1981.R")
  source("./normalityUnivariate/pearson1900.R")
  source("./normalityUnivariate/rahman1997.R")
  source("./normalityUnivariate/shapiro1965.R")
  source("./normalityUnivariate/shapiro1972.R")
  source("./normalityUnivariate/spiegelhalter1977.R")
  source("./normalityUnivariate/zhang1999a.R")
  source("./normalityUnivariate/zhang1999b.R")
  source("./normalityUnivariate/zhang2005a.R")
  source("./normalityUnivariate/zhang2005b.R")
  functionList <- list(agostino1971,
                       agostino1973,
                       anderson1954,
                       barrio1999,
                       bonett2002,
                       bonettseier2002,
                       bontemps2005a,
                       bontemps2005b,
                       cabana1994a,
                       cabana1994b,
                       chen1995,
                       coin2008,
                       cramer1928,
                       desgagne2013, 
                       desgagne2018a, 
                       desgagne2018b, 
                       doornik1994,
                       epps1983,
                       filliben1975,
                       gel2007, 
                       gel2008,
                       glen2001, 
                       hosking1990a,
                       hosking1990b,
                       hosking1990c,
                       hosking1990d,
                       jarque1980,
                       lilliefors1967,
                       martinez1981,
                       pearson1900,
                       rahman1997,
                       shapiro1965,
                       shapiro1972,
                       spiegelhalter1977,
                       zhang1999a,
                       zhang1999b,
                       zhang2005a,
                       zhang2005b)
  
  
  output<-data.frame()
  outputList <- list()
  
  if(!selectAll){
    for(i in selectedFunctions){
      output<-rbind(output,data.frame(funcList[[i]](data)))
    }
  }else{
    for(i in 1:length(funcList)){
      output<-rbind(output,data.frame(funcList[[i]](data)))
    }
  }
  ## Format output
  output$statistic <- round(output$statistic, 2)
  output$p.value <- round(output$p.value, 2)
  
  ## Return output
  return (output)
}
