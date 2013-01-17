\name{sipp84fp}
\alias{sipp84fp}
\docType{data}
\title{
Survey of Income and Program Participation (SIPP) - 1984 Panel
}
\description{
Dictionary file for the full 1984 panel for NBER's Survey of Income and Program Participation (SIPP).
}
\usage{
data(sipp84fp)
}
\format{
  The format is:
 chr "sipp84fp"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
\url{http://thedataweb.rm.census.gov/ftp/sipp_ftp.html#sipp84}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(sipp84fp)
## Write the data to a dictionary file
writeLines(sipp84fp_dct, "sipp84fp.dct")
}
\keyword{datasets}
