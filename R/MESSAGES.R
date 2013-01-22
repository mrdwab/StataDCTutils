#'Print messages from parsed dictionary files
#'
#'This is a simple convenience/utility function to print a nicely formatted
#'message that might be stored in the output of a dictionary file parsed using
#'\code{\link{dct.parser}}.
#'
#'
#'@param x The object that contains the message.
#'@author Ananda Mahto
#'@examples
#'
#'## Read an example dictionary file
#'data(sampleDctData)
#'## Write the data to a dictionary file
#'currentdir <- getwd()
#'setwd(tempdir())
#'writeLines(sipp84fp_dct, "sipp84fp.dct")
#'sipp84_R_dict <- dct.parser("sipp84fp.dct")
#'MESSAGES(sipp84_R_dict)
#'#'setwd(currentdir)
#'
MESSAGES <- function(x) {
  cat(attr(x, "MESSAGE"), fill = 80)
}
