#' getParamMinMax
#' Computes the minum value bounds for a parameter p of a polytope defined
#' by+A.x<=b and C.x=v or by the polytope of the CaNmod object
#' @param x either a CaNmod object or a named list with at least a matrix A and
#' a vector b (A.x<=b) and optionally a matrix C and a vector v (C.x=v)
#' @param p the index of the parameter for which the bounds should be computed
#' @param presolved an object returned by \link{presolveLPMod}
#' @param maximum should ob be maximized
#' @importFrom ROI ROI_solve
#'
#' @return a vector with lower bounds and upper bounds
#'

getParamMinMax <- function(x, p, presolved, maximum) {
  if (!is.logical(maximum))
    stop("error in getParamMinMax, maximum should be logical")
  x <- reformatX(x)
  A <- x$A
  b <- x$b
  C <- x$C
  v <- x$v

  if (nrow(presolved$lhs) > 0){
    A2 <- presolved$lhs[presolved$dir == "<=", ]
    b2 <- presolved$rhs[presolved$dir == "<="]
    C2 <- presolved$lhs[presolved$dir == "=", ]
    v2 <- presolved$rhs[presolved$dir == "="]
    lower <- presolved$lower
    upper <- presolved$upper
  } else {
    A2 <- matrix(0, 0, 0) #model totally solved by presolve
  }

  solved <- FALSE
  ntry <- 0

  if (colnames(A)[p] %in% colnames(A2)){
    while (!all(solved) & ntry < 3) {
      ob <- rep(0, ncol(A2))
      ob[p] <- 1
      lp_model <- defineLPMod(A2, b2, C2, v2, lower, upper,
                              maximum = maximum, ob = ob)
      res <- ROI_solve(lp_model,
                       solver = "lpsolve",
                       control = list(
                         scaling = c("extreme",
                                     "equilibrate",
                                     "integers")))
      if (res$status$code != 0)
        res <- ROI_solve(lp_model, solver = "glpk",
                         control=list(tm_limit = 1000))

      if (res$status$code == 0) {
        bound <- res$solution[p]
        solved[1] <- TRUE
      } else  {
        bound <- NA
      }
      ntry <- ntry + 1
    }
    res <- bound
  } else {
    res <- presolved$fixed[colnames(A)[p]]
  }
  return (res)
}
