#' getBoundParam
#' Computes the possible bounds for a parameter p of a polytope defined
#' by+A.x<=b and C.x=v
#' @param A the matrix of inequality A.x<=b
#' @param b the vector A.x<=b
#' @param p the index of the parameter for which the bounds should be computed
#' @param C the matrix of equality C.x=v (default NULL for no equality)
#' @param v the vector of equality C.x=v (default NULL for no equality
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
#' X0 <- getBoundParam(A,b,1)
#'
#' @export

getBoundParam <- function(A, b, p, C = NULL, v = NULL,
                          lower = NULL, upper = NULL,
                          presolve = TRUE) {
  nbparam <- ncol(A)
  if (is.null(C)) {
    C <- matrix(0, 0, nbparam)
    v <- numeric(0)
  }
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
  }
  solved <- c(FALSE,FALSE)
  ntry <- 0



  if (colnames(A)[p] %in% colnames(A2)){
    while (!all(solved) & ntry < 3) {
      ob <- rep(0, nbparam)
      ob[p] <- 1
      lp_model <- defineLPMod(A2, b2, C2, v2, lower, upper,
                              maximum = TRUE, ob = ob)
      res <- ROI_solve(lp_model, solver = "glpk")

      if (res$status$code == 0) {
        upbound <- res$solution[p]
        solved[1] <- TRUE
      } else  {
        upbound <- NA
      }
      lp_model <- defineLPMod(A2, b2, C2, v2, lower, upper,
                              maximum = FALSE, ob = ob)
      res <- ROI_solve(lp_model, solver = "glpk")

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
