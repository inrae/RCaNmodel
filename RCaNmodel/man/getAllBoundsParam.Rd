% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getAllBoundsParam.R
\name{getAllBoundsParam}
\alias{getAllBoundsParam}
\title{getAllBoundsParam
Computes the possible bounds for all parameters of a polytope defined by
A.x<=b and C.x=v or by a CaNmod object}
\usage{
getAllBoundsParam(x, progressBar = TRUE)
}
\arguments{
\item{x}{either a CaNmod oject or a named list with at least a matrix A and
a vector b (A.x<=b) and optionnally a matrix C and a vector v (C.x=v)}

\item{progressBar}{should a progress bar be displayed (default TRUE)}
}
\value{
a datafame with first column corresponding to colnames(A), and
corresponding lower bounds (column 2) and upper bounds (column 3)
}
\description{
getAllBoundsParam
Computes the possible bounds for all parameters of a polytope defined by
A.x<=b and C.x=v or by a CaNmod object
}
\examples{
n <- 20
A1 <- -diag(n)
b1 <- as.matrix(rep(0,n))
A2 <- diag(n)
b2 <- as.matrix(rep(1,n))
A <- rbind(A1,A2)
b <- rbind(b1,b2)
X0 <- getAllBoundsParam(list(A = A, b = b))
}
