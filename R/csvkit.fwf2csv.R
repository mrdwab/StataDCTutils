#'Convenience function to create a csv file from a fixed-width file
#'
#'This is purely a convenience function to use the start and width definitions
#'from a dictionary file to convert a fixed-width file to a csv file using
#'\code{in2csv} from \code{csvkit} using a \code{\link{system}} call.
#'
#'This function essentially makes a \code{\link{system}} call to \code{in2csv}
#'from \code{csvkit} and instantly returns to the R prompt while the processing
#'continues in the background. For small files, the conversion happens very
#'quickly. For larger files, you can expect to wait a while.
#'
#'The csv file might be considerably larger than the flat-file, particularly if
#'the dictionary file defines overlapping columns, as some files do. You can
#'verify the entire file was written by checking the number of lines in the
#'file (perhaps using another \code{\link{system}} call to \code{wc}, for
#'example \code{system("wc -l path/to/flat-file"); system("wc -l
#'path/to/csv")}). The csv file should have one file more than the data file
#'since it would include a line of headers.
#'
#'@param datafile The name of the flat data file (optionally including the path
#'if the file is not in the working directory).
#'@param schema The name of the schema file (perhaps generated using
#'\code{\link{dct.parser}} and \code{\link{csvkit.schema}}) that defines the
#'variable names, start positions, and column widths (can optionally include
#'the file path if the file is not in the working directory).
#'@param output The desired name of the output file.
#'@note %% ~~further notes~~
#'@author Ananda Mahto
#'@seealso \code{\link{csvkit.schema}}
#'@references csvkit's \code{in2csv} documentation:
#'\url{https://csvkit.readthedocs.org/en/latest/scripts/in2csv.html}
#'@keywords ~kwd1 ~kwd2
#'@examples
#'
#'## Read an example dictionary file
#'data(sampleDctData)
#'## Write the dictionary to a dictionary file
#'## Write the data to a data file
#'currentdir <- getwd()
#'setwd(tempdir())
#'list.files(pattern=".dat|.dct|.csv")
#'writeLines(data66_dct, "data66.dct")
#'writeLines(data66_dat, "data66.dat")
#'## Is everything there in the dictionary file?
#'dct.parser("data66.dct", preview = TRUE)
#'# Missing the storage type, so remove that from includes
#'data66_dict <- dct.parser("data66.dct", 
#'                          includes = c("StartPos", "ColName",
#'                                       "ColWidth", "VarLabel"))
#'list.files(pattern=".dat|.dct|.csv")
#'csvkit.schema(data66_dict)
#'list.files(pattern=".dat|.dct|.csv")
#'csvkit.fwf2csv(datafile = "data66.dat",
#'               schema   = "data66.dct.csv",
#'               output   = "data66-FINAL.csv")
#'Sys.sleep(10)
#'list.files(pattern=".dat|.dct|.csv")
#'read.csv("data66-FINAL.csv", nrows = 5)
#'setwd(currentdir)
#'
csvkit.fwf2csv <- function(datafile, schema, output) {
  if (Sys.which("in2csv") == "") stop("Please install csvkit before proceeding:
  See: https://csvkit.readthedocs.org/en/latest/index.html")
  system(paste("in2csv -f fixed -s ", schema, datafile, " > ", output), 
         wait = FALSE)
  message(
    "Depending on the size of your input file, this may take a while!
Look to see if the ", dQuote("in2csv"), " process is still running in your 
task manager. If it is, the output file is still being written. If not, 
you should check the resulting csv in your working directory to verify 
that the correct number of lines has been written to it.
While you wait, you can continue working on other tasks in R.")
}
