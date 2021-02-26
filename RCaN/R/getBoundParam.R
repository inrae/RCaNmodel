#' getBoundParam
#' Computes the possible bounds for a parameter p of a polytope defined
#' by A.x<=b and C.x=v or by the polytope of the CaNmod object
#' @param x either a CaNmod oject or a named list with at least a matrix A and
#' a vector b (A.x<=b) and optionally a matrix C and a vector v (C.x=v)
#' @param p the index of the parameter for which the bounds should be computed
#'
#' @importFrom ROI ROI_solve
#'
#' @return a vector with lower bounds and upper bounds
#' @examples
#' n <- 20
#' A1 <- -diag(n)
#' b1 <- as.matrix(rep(0,n))
#' A2 <- diag(n)
#' b2 <- as.matrix(rep(1,n))
#' A <- rbind(A1,A2)
#' b <- rbind(b1,b2)
#' X0 <- getBoundParam(list(A = A, b = b),1)
#'
#' @export

getBoundParam <- function(x, p) {
  x <- reformatX(x)
  A <- x$A
  b <- x$b
  C <- x$C
  v <- x$v

  if (is.null(colnames(A))) {
    colnames(A) <- paste("col", seq_len(ncol(A)), sep = "")
  }

  if (is.null(colnames(C)) & !is.null(C))
    colnames(C) <- colnames(A)

  nbparam <- ncol(A)
  if (is.null(C)) {
    C <- matrix(0, 0, nbparam)
    v <- numeric(0)
  }



  presolvedmin<- presolveLPMod(A, b, C, v, sense = "min")
  presolvedmax<- presolveLPMod(A, b, C, v, sense = "max")
  x <- list(A = A, b = b, C = C, v = v)


  res <- c(getParamMinMax(x, p, presolvedmin, FALSE),
           getParamMinMax(x, p, presolvedmax, TRUE))
  return (res)
}
