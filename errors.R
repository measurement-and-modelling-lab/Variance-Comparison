function (data) {
    ## "data" is a list of data matrices

    data.length <- length(data)
    
    for (jj in 1:data.length) {

        group <- data[[jj]]
        N <- nrow(group)
        cols <- ncol(group)
        
        if (NA %in% as.numeric(group)) {
            stop("Data contains non-numeric values")
        }

        if (NA %in% group) {
            message <- paste("Raw data matrix", jj, "has at least one empty entry")
            stop(message)
        }
    }
}
