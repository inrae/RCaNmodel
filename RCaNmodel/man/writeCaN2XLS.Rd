% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writeCaN2XLS.R
\name{writeCaN2XLS}
\alias{writeCaN2XLS}
\title{writeCaN2XLS}
\usage{
writeCaN2XLS(file, myCaNmod)
}
\arguments{
\item{file}{the name of the export file}

\item{myCaNmod}{a CaNmod object to be exported}
}
\description{
writes an xlsx file that would correspond to the template of the CaNMod
object
}
\examples{
myCaNmod <- buildCaN(system.file("extdata",
 "CaN_template_mini.xlsx", package = "RCaNmodel"))
 writeCaN2XLS(paste(tempdir(),
                         "/export.xlsx",
                         sep = ""),
                   myCaNmod)

}
