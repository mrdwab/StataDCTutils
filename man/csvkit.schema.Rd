\name{csvkit.schema}
\alias{csvkit.schema}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert a parsed dictionary file to a csvkit schema file
}
\description{
After parsing a \code{.dct} dictionary file with the \code{\link{dct.parser}} function, it may be useful to convert that file to a schema that can be used by \emph{csvkit}, a useufl Pyton tool for working with csv files. In particular, this creates a schema that allows you to convert a fixed width format file to a csv file.
}
\usage{
csvkit.schema(x, columns.to.match = NULL)
}
\arguments{
  \item{x}{
Your input \code{data.frame}. Must include at least the following information in separate columns: the variable names, the starting position of the variable, and the length of the variable in the fixed width file.
}
  \item{columns.to.match}{
By default, if the input file is the output of \code{\link{dct.parser}}, the values for this argument do not need to be specified. If you are using your own \code{data.frame}, specify which columns contain the (1) variable name, (2) the starting position, and (3) the width of the variable.
}
}
\details{
This function will write a csv file to your current working directory. It takes the name of the original parsed dictionary file appended with .csv by default (which is stored as an attribute of the \code{data.frame} created during the dictionary parsing step). If that attribute is not present, it prompts the user for a file name, which should be provided \emph{not quoted}.
}
\references{
csvkit's \code{in2csv} documentation: \url{https://csvkit.readthedocs.org/en/latest/scripts/in2csv.html}
}
\author{
Ananda Mahto
}
\examples{
## Read an example dictionary file
data(sampleDctData)
## Write the data to a dictionary file
currentdir <- getwd()
setwd(tempdir())
writeLines(sipp84fp_dct, "sipp84fp.dct")
sipp84_R_dict <- dct.parser("sipp84fp.dct")
list.files(pattern=".dat|.dct|.csv")
csvkit.schema(sipp84_R_dict)
list.files(pattern=".dat|.dct|.csv")
setwd(currentdir)
}
\keyword{ csvkit }
\keyword{ in2csv }
\keyword{ fwf }
