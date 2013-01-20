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
