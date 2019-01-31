tableGen <- function (table) {
    ## Create an html table from a matrix or data frame

    table <- as.matrix(table)

    htmlTable <- '<table border="1">'

    ## Create a header from the column names
    header <- paste0('<td align="center"style="border-bottom:1px solid black"><b>', colnames(table), '</b></td>', collapse='')
    header <- paste0('<tr>', header, '</tr>')
    htmlTable <- paste0(htmlTable, header)

    for (i in 1:nrow(table)) {
        row <- paste0('<td align="right">', table[i,], '</td>', collapse='')
        row <- paste0('<tr>', row, '</tr>')
        htmlTable <- paste0(htmlTable, row)
    }

    htmlTable <- paste0(htmlTable, '</table>')

}
