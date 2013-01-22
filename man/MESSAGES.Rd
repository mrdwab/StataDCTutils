\name{MESSAGES}
\alias{MESSAGES}
\title{Print messages from parsed dictionary files}
\usage{
  MESSAGES(x)
}
\arguments{
  \item{x}{The object that contains the message.}
}
\description{
  This is a simple convenience/utility function to print a
  nicely formatted message that might be stored in the
  output of a dictionary file parsed using
  \code{\link{dct.parser}}.
}
\examples{
## Read an example dictionary file
data(sampleDctData)
## Write the data to a dictionary file
currentdir <- getwd()
setwd(tempdir())
writeLines(sipp84fp_dct, "sipp84fp.dct")
sipp84_R_dict <- dct.parser("sipp84fp.dct")
MESSAGES(sipp84_R_dict)
#'setwd(currentdir)
}
\author{
  Ananda Mahto
}

