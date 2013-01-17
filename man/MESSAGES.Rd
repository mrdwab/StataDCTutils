\name{MESSAGES}
\alias{MESSAGES}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print messages from parsed dictionary files
}
\description{
This is a simple convenience/utility function to print a nicely formatted message that might be stored in the output of a dictionary file parsed using \code{\link{dct.parser}}.
}
\usage{
MESSAGES(x)
}
\arguments{
  \item{x}{
The object that contains the message.
}
}
\author{
Ananda Mahto
}
\examples{
## Read an example dictionary file
data(sipp84fp)
## Write the data to a dictionary file
writeLines(sipp84fp_dct, "sipp84fp.dct")
sipp84_R_dict <- dct.parser("sipp84fp.dct")
MESSAGES(sipp84_R_dict)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }
