MESSAGES <- function(x) {
    cat(attr(x, "MESSAGE"), fill = 80)
}

csvkitSchema <- function(x, columns.to.match = NULL) {
    if (is.null(columns.to.match)) {
        columns.to.match <- c("ColName", "StartPos", "ColWidth")
        if(!all(columns.to.match %in% names(x))) {
            stop("Please specify which column names match 'variable name',
                 'starting position', and 'variable width'")
        }
    }
    mycsvkit <- x[columns.to.match]
    names(mycsvkit) <- c("column", "start", "length")
    if (is.null(attributes(x)$original.dictionary[2])) {
        outname <- readline(prompt = "Please specify the output filename: ")
    } else {
        outname <- paste(attributes(x)$original.dictionary[2], 
                         ".csv", sep = "")
    }
    write.csv(mycsvkit, 
              file = outname, row.names = FALSE)
}