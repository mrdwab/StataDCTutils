\name{data66_dct}
\alias{data66_dct}
\docType{data}
\title{
Example dictionary file accompanying \code{\link{data66_dat}} 
}
\description{
Example dictionary file accompanying \code{\link{data66_dat}} to be parsed using \code{\link{dct.parser}} and then possibly using other tools from the \code{StataDCTutils} package.
}
\usage{
data(sampleDctData)
## data66_dct is a character vector
## Write the dictionary to a text file in your working directory
writeLines(data66_dct, "data66.dct")
}
\source{
Obtained from \url{http://faculty-staff.ou.edu/L/Carlos.E.Lamarche-1/ec5243/data.html}
}
\keyword{datasets}
