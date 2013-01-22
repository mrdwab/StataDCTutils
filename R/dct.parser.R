#'Parse a Stata dictionary file for use in R
#'
#'R cannot read Stata's dictionary files directly. This function parses the
#'dictionary file to a \code{data.frame} that can be used to further process
#'the data files and make them usable with R.
#'
#'Many datasets are distributed as a combination of Stata \code{.dat} (data,
#'usually fixed-width-format), \code{.dct} (dictionary), and \code{.do} (other
#'commands for Stata, for example recoding the data and so on) files. The
#'dictionary files are used to tell Stata details like which column in the data
#'file represents the starting position of the data for a given variable, how
#'many columns should be read for that given variable, what the storage type of
#'that variable is, and what that variable's name and label shoud be.
#'
#'The expected workflow might include (1) parsing the dictionary file using
#'\code{\link{dct.parser}}, (2) converting the fixed width data file to a csv
#'file using \code{csvkit} after generating a csvkit \emph{schema} file using
#'\code{\link{csvkitSchema}}, (3) reading in the file using your preferred
#'method (for example, \code{\link{sqldf}}, \code{\link{read.csv}}, or another
#'appropriate method), (4) re-assigning some of the metadata extracted from the
#'dictionary file to your newly imported dataset.
#'
#'@param dct Stata dictionary file, most often with a \code{.dct} extension.
#'@param includes A complete dictionary file includes (usually in this order),
#'the column starting position, the storage type of the variable, the variable
#'name, the width of the column, and the variable label. Delete any which are
#'not relevant to your dictionary file.
#'@param preview If you are not sure what values to select for \code{includes},
#'use the \code{preview = TRUE} argument to see the first few lines of the
#'relevant portion of the dictionary file to decide what the dictionary file
#'structure is.
#'@author Ananda Mahto
#'@seealso \code{\link{read.dta}}
#'@references \itemize{ \item Stata data types:
#'\url{http://www.stata.com/help.cgi?datatypes} \item Stata help for
#'fixed-format data:
#'\url{http://www.stata.com/support/faqs/data-management/reading-fixed-format-data/}
#'\item Initial version of function on Stack Overflow:
#'\url{http://stackoverflow.com/questions/14224321/reading-dat-and-dct-directly-from-r}
#'}
#'@keywords ~kwd1 ~kwd2
#'@examples
#'
#'## Read an example dictionary file
#'data(sipp84fp)
#'## Write the data to a dictionary file
#'currentdir <- getwd()
#'setwd(tempdir())
#'writeLines(sipp84fp_dct, "sipp84fp.dct")
#'dct.parser("sipp84fp.dct", preview = TRUE)
#'sipp84_R_dict <- dct.parser("sipp84fp.dct")
#'head(sipp84_R_dict)
#'setwd(currentdir)
#'
dct.parser <- function(dct, includes = c("StartPos", "StorageType", "ColName", 
                                         "ColWidth", "VarLabel"),
                       preview = FALSE) {
  temp <- readLines(dct)
  temp <- temp[grepl("_column", temp)]
  
  if (isTRUE(preview)) {
    head(temp)
  } else {
    possibilities <- c("StartPos", "StorageType", 
                       "ColName", "ColWidth", "VarLabel")
    classes <- c("numeric", "character", "
                 character", "numeric", "character")
    pattern <- c(StartPos = ".*\\(([0-9 ]+)\\)",
                 StorageType = "(byte|int|long|float|double|str[0-9]+)",
                 ColName = "(.*)",
                 ColWidth = "%([0-9.]+)[a-z]+",
                 VarLabel = "(.*)")
    
    mymatch <- match(includes, possibilities)
    
    pattern <- paste(paste(pattern[mymatch], 
                           collapse ="\\s+"), "$", sep = "")    
    
    metadata <- setNames(lapply(seq_along(mymatch), function(x) {
      out <- gsub(pattern, paste("\\", x, sep = ""), temp)
      out <- gsub("^\\s+|\\s+$", "", out)
      out <- gsub('\"', "", out, fixed = TRUE)
      class(out) <- classes[mymatch][x] ; out }), 
                         possibilities[mymatch])
    
    implicit.dec <- grepl("\\.[1-9]", metadata[["ColWidth"]])
    if (any(implicit.dec)) {
      message("Some variables may need to be corrected for implicit decimals. 
              Try 'MESSAGES(output_from_dct.parser)' for more details.")
      metadata[["Decimals"]] <- rep(NA, length(metadata[["ColWidth"]]))
      metadata[["Decimals"]][implicit.dec] <-
        as.numeric(gsub("[0-9]+\\.", "", 
                        metadata[["ColWidth"]][implicit.dec]))
      metadata[["ColWidth"]] <- floor(as.numeric(metadata[["ColWidth"]]))
    }
    
    metadata[["ColName"]] <- make.names(
      gsub("\\s", "", metadata[["ColName"]]))
    
    metadata <- data.frame(metadata)
    
    if ("StorageType" %in% includes) {
      metadata <- 
        within(metadata, {
          colClasses <- ifelse(
            StorageType == "byte", "raw",
            ifelse(StorageType %in% c("double", "long", "float"), 
                   "numeric", 
                   ifelse(StorageType == "int", "integer",
                          ifelse(substr(StorageType, 1, 3) == "str", 
                                 "character", NA))))
        })
    }
    if (any(implicit.dec)) {
      attr(metadata, "MESSAGE") <- c(sprintf("%s", paste(
        "Some variables might need to be corrected for implicit decimals. 
        A variable, 'Decimals', has been created in the metadata that
        indicates the number of decimal places the variable should hold. 
        To correct the output, try (where your stored output is 'mydf'): 
        
        lapply(seq_along(mydf[!is.na(Decimals)]), 
        function(x) mydf[!is.na(Decimals)][x]
        / 10^Decimals[!is.na(Decimals)][x])
        
        The variables in question are:
        ")), sprintf("%s", metadata[["ColName"]][!is.na(metadata[["Decimals"]])]))
            class(attr(metadata, "MESSAGE")) <- c(
                "MESSAGE", class(attr(metadata, "MESSAGE")))
        }
        attr(metadata, "original.dictionary") <- 
            c(dct, basename(dct))
        metadata
    }
}
