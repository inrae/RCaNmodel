#' getAllBoundsParam
#' Computes the possible bounds for all parameters of a polytope defined by
#' A.x<=b and C.x=v
#' @param A the matrix of inequality A.x<=b
#' @param b the vector A.x<=b
#' @param C the matrix of equality C.x=v (default NULL for no equality)
#' @param v the vector of equality C.x=v (default NULL for no equality
#'
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#'
#' @return a datrame with first column corresponding to colnames(A), and
#' corresponding lower bounds (column 2) and upper bounds (column 3)
#' @examples
#' n <- 20
#' A1 <- -diag(n)
#' b1 <- as.matrix(rep(0,n))
#' A2 <- diag(n)
#' b2 <- as.matrix(rep(1,n))
#' A <- rbind(A1,A2)
#' b <- rbind(b1,b2)
#' X0 <- getAllBoundsParam(A,b)
#' @export

getAllBoundsParam <- function(A, b, C = NULL, v = NULL) {
  nbparam <- ncol(A)
  if (is.null(colnames(A))) {
    colnames(A) <- paste("col", seq_len(ncol(A)), sep = "")
  }
  pb <- txtProgressBar(min = 0, max = nbparam, style = 3)
  bounds <- sapply(1:nbparam, function(p) {
    setTxtProgressBar(pb, p)
    getBoundParam(A, b, p, C, v)
  })
  data.frame(
    param = colnames(A),
    lowerbound = bounds[1, ],
    upperbound = bounds[2, ]
  )
}
