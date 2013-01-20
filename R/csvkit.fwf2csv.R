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