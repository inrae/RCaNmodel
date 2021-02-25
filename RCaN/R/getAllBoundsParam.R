#' getAllBoundsParam
#' Computes the possible bounds for all parameters of a polytope defined by
#' A.x<=b and C.x=v or by a CaNmod object
#' @param x either a CaNmod oject or a named list with at least a matrix A and
#' a vector b (A.x<=b) and optionnally a matrix C and a vector v (C.x=v)
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

getAllBoundsParam <- function(x) {
  x <- reformatX(x)
  A <- x$A
  b <- x$b
  C <- x$C
  v <- x$v


  nbparam <- ncol(A)
  if (is.null(colnames(A))) {
    colnames(A) <- paste("col", seq_len(ncol(A)), sep = "")
  }
  presolved <- presolveLPMod(A, b, C, v)
  if (nrow(presolved$lhs) > 0){
    A2 <- presolved$lhs[presolved$dir == "<=", ]
    b2 <- presolved$rhs[presolved$dir == "<="]
    C2 <- presolved$lhs[presolved$dir == "=", ]
    v2 <- presolved$rhs[presolved$dir == "="]
    lower <- presolved$lower
    upper <- presolved$upper
  } else{
    A2 <- A
    b2 <- b
    C2 <- C
    v2 <- v
    lower <- rep(0, ncol(A))
    upper <- rep(Inf, ncol(A))
  }

  pb <- txtProgressBar(min = 0, max = nbparam, style = 3)
  bounds <- sapply(1:nbparam, function(p) {
    setTxtProgressBar(pb, p)
    if (colnames(A)[p] %in% colnames(A2)){
      p2 <- match(colnames(A)[p], colnames(A2))
      bound <-getBoundParam(list(A = A2,
                                 b = b2,
                                 C = C2,
                                 v = v2),
                            p2, lower, upper, presolve = FALSE)
    } else {
      bound <-rep(presolved$fixed[colnames(A)[p]],2)
    }
    bound
  })
  data.frame(
    param = colnames(A),
    lowerbound = bounds[1, ],
    upperbound = bounds[2, ]
  )
}
