\name{data66_dat}
\alias{data66_dat}
\docType{data}
\title{
Example dataset to be converted to a csv file
}
\description{
This is a sample dataset useful to demonstrate conversion to csv using \code{\link{csvkit.fwf2csv}} after parsing the dictionary file (using \code{\link{dct.parser}}) and creating a csvkit schema file (using \code{\link{csvkit.schema}}).
}
\usage{
data(sampleDctData)
## data66_dat is a character vector
## Recreate the data file in your working directory
writeLines(data66_dat, "data66.dat")
}
\source{
Obtained from \url{http://faculty-staff.ou.edu/L/Carlos.E.Lamarche-1/ec5243/data.html}
}
\keyword{datasets}
