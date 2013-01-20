\name{dct.parser}
\alias{dct.parser}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parse a Stata dictionary file for use in R
}
\description{
R cannot read Stata's dictionary files directly. This function parses the dictionary file to a \code{data.frame} that can be used to further process the data files and make them usable with R. 
}
\usage{
dct.parser(dct, 
           includes = c("StartPos", "StorageType", "ColName", 
                        "ColWidth", "VarLabel"), 
           preview = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dct}{
Stata dictionary file, most often with a \code{.dct} extension. 
}
  \item{includes}{
A complete dictionary file includes (usually in this order), the column starting position, the storage type of the variable, the variable name, the width of the column, and the variable label. Delete any which are not relevant to your dictionary file.
}
  \item{preview}{
If you are not sure what values to select for \code{includes}, use the \code{preview = TRUE} argument to see the first few lines of the relevant portion of the dictionary file to decide what the dictionary file structure is.
}
}
\details{
Many datasets are distributed as a combination of Stata \code{.dat} (data, usually fixed-width-format), \code{.dct} (dictionary), and \code{.do} (other commands for Stata, for example recoding the data and so on) files. The dictionary files are used to tell Stata details like which column in the data file represents the starting position of the data for a given variable, how many columns should be read for that given variable, what the storage type of that variable is, and what that variable's name and label shoud be. 

The expected workflow might include (1) parsing the dictionary file using \code{\link{dct.parser}}, (2) converting the fixed width data file to a csv file using \code{csvkit} after generating a csvkit \emph{schema} file using \code{\link{csvkitSchema}}, (3) reading in the file using your preferred method (for example, \code{\link{sqldf}}, \code{\link{read.csv}}, or another appropriate method), (4) re-assigning some of the metadata extracted from the dictionary file to your newly imported dataset.
}
\references{
\itemize{
  \item Stata data types: \url{http://www.stata.com/help.cgi?datatypes}
  \item Stata help for fixed-format data: \url{http://www.stata.com/support/faqs/data-management/reading-fixed-format-data/}
  \item Initial version of function on Stack Overflow: \url{http://stackoverflow.com/questions/14224321/reading-dat-and-dct-directly-from-r}
}
}
\author{
Ananda Mahto
}

\seealso{
\code{\link{read.dta}}
}
\examples{
## Read an example dictionary file
data(sipp84fp)
## Write the data to a dictionary file
writeLines(sipp84fp_dct, "sipp84fp.dct")
dct.parser("sipp84fp.dct", preview = TRUE)
sipp84_R_dict <- dct.parser("sipp84fp.dct")
head(sipp84_R_dict)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
