#' getAllBoundsParam
#' Computes the possible bounds for all parameters of a polytope defined by
#' A.x<=b and C.x=v or by a CaNmod object
#' @param x either a CaNmod oject or a named list with at least a matrix A and
#' a vector b (A.x<=b) and optionnally a matrix C and a vector v (C.x=v)
#' @param progressBar should a progress bar be displayed (default TRUE)

#'
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#'
#' @return a datafame with first column corresponding to colnames(A), and
#' corresponding lower bounds (column 2) and upper bounds (column 3)
#' @examples
#' n <- 20
#' A1 <- -diag(n)
#' b1 <- as.matrix(rep(0,n))
#' A2 <- diag(n)
#' b2 <- as.matrix(rep(1,n))
#' A <- rbind(A1,A2)
#' b <- rbind(b1,b2)
#' X0 <- getAllBoundsParam(list(A = A, b = b))
#' @export

getAllBoundsParam <- function(x,
                              progressBar = TRUE) {
  x <- reformatX(x)
  A <- x$A
  b <- x$b
  C <- x$C
  v <- x$v


  nbparam <- ncol(A)
  if (is.null(colnames(A))) {
    colnames(A) <- paste("col", seq_len(ncol(A)), sep = "")
  }
  if (is.null(colnames(C)) & !is.null(C))
    colnames(C) <- colnames(A)

  presolvedmin <- presolveLPMod(A, b, C, v, sense = "min")
  presolvedmax <- presolveLPMod(A, b, C, v, sense = "max")

  x <- list(A = A, b = b, C = C, v = v)

  if (progressBar)
    pb <- txtProgressBar(min = 0, max = nbparam, style = 3)
  bounds <- sapply(1:nbparam, function(p) {
    if (progressBar)
      setTxtProgressBar(pb, p)
    c(getParamMinMax(x, p, presolvedmin, FALSE),
      getParamMinMax(x, p, presolvedmax, TRUE))
  })
  data.frame(
    param = colnames(A),
    lowerbound = bounds[1, ],
    upperbound = bounds[2, ]
  )
}
