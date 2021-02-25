#' getBoundParam
#' Computes the possible bounds for a parameter p of a polytope defined
#' by+A.x<=b and C.x=v or by the polytope of the CaNmod object
#' @param x either a CaNmod oject or a named list with at least a matrix A and
#' a vector b (A.x<=b) and optionnally a matrix C and a vector v (C.x=v)
#' @param p the index of the parameter for which the bounds should be computed
#' @param lower lower bounds (default NULL for bounds 0)
#' @param upper upper bounds (default NULL for bounds Inf)
#' @param presolve should presolve should be done
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

getBoundParam <- function(x, p,
                          lower = NULL, upper = NULL,
                          presolve = TRUE) {
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
  if (is.null(lower))
    lower <- rep(0, ncol(A))
  if (is.null(upper))
    upper <- rep(Inf, ncol(A))
  if (presolve) {
    presolved <- presolveLPMod(A, b, C, v)
    if (nrow(presolved$lhs) > 0){
      A2 <- presolved$lhs[presolved$dir == "<=", ]
      b2 <- presolved$rhs[presolved$dir == "<="]
      C2 <- presolved$lhs[presolved$dir == "=", ]
      v2 <- presolved$rhs[presolved$dir == "="]
      lower <- presolved$lower
      upper <- presolved$upper
    }
  } else {
    A2 <- A
    b2 <- b
    C2 <- C
    v2 <- v
    lower <- lower
    upper <- upper
  }
  solved <- c(FALSE,FALSE)
  ntry <- 0



  if (colnames(A)[p] %in% colnames(A2)){
    while (!all(solved) & ntry < 3) {
      ob <- rep(0, nbparam)
      ob[p] <- 1
      lp_model <- defineLPMod(A2, b2, C2, v2, lower, upper,
                              maximum = TRUE, ob = ob)
      res <- ROI_solve(lp_model, solver = "lpsolve")
      if (res$status$code != 0)
        res <- ROI_solve(lp_model, solver = "glpk",
                         control=list(tm_limit = 1000))

      if (res$status$code == 0) {
        upbound <- res$solution[p]
        solved[1] <- TRUE
      } else  {
        upbound <- NA
      }
      lp_model <- defineLPMod(A2, b2, C2, v2, lower, upper,
                              maximum = FALSE, ob = ob)
      res <- ROI_solve(lp_model, solver = "lpsolve")
      if (res$status$code != 0)
        res <- ROI_solve(lp_model, solver = "glpk",
                         control=list(tm_limit = 1000))

      if (res$status$code == 0) {
        lowbound <- res$solution[p]
        solved[2] <- TRUE
      } else {
        lowbound <- NA
      }
      ntry <- ntry + 1
    }
    res <- c(lowbound, upbound)
  } else {
    res <-rep(presolved$fixed[colnames(A)[p]],2)
  }
  return (res)
}
