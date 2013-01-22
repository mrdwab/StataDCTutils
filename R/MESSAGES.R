#'Print messages from parsed dictionary files
#'
#'This is a simple convenience/utility function to print a nicely formatted
#'message that might be stored in the output of a dictionary file parsed using
#'\code{\link{dct.parser}}.
#'
#'
#'@param x The object that contains the message.
#'@author Ananda Mahto
#'@keywords ~kwd1 ~kwd2
#'@examples
#'
#'## Read an example dictionary file
#'data(sipp84fp)
#'## Write the data to a dictionary file
#'writeLines(sipp84fp_dct, "sipp84fp.dct")
#'sipp84_R_dict <- dct.parser("sipp84fp.dct")
#'MESSAGES(sipp84_R_dict)
#'
MESSAGES <- function(x) {
  cat(attr(x, "MESSAGE"), fill = 80)
}
