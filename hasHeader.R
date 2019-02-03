hasHeader <- function(path) {

    with_header <- read.csv(path, header=TRUE)
    with_header <- as.matrix(with_header)

    without_header <- read.csv(path, header=FALSE)
    without_header <- as.matrix(without_header)

    if (!is.numeric(with_header)) {
        stop("All data must be numeric.")
    } else {
        is.numeric(with_header) & !is.numeric(without_header)
    }

}
