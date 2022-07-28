#' getParamMinMax
#' Computes the minum value bounds for a parameter p of a polytope defined
#' by+A.x<=b and C.x=v or by the polytope of the CaNmod object
#' @param OP an OP object
#' @param p index of the parameter
#'
#' @return a vector with lower bounds and upper bounds
#'

getParamMinMax <- function(OP, p) {
  solved <- FALSE
  ntry <- 0
  while (ntry < 3) {
    if (requireNamespace("ROI.plugin.cbc", quietly = TRUE)){
      res <- ROI_solve(OP,
                       solver = "cbc",
                       control = list(logLevel = 0))
    } else {
      res <- ROI_solve(OP,
                       solver = "lpsolve",
                       control = list(
                         scaling = c("extreme",
                                     "equilibrate",
                                     "integers")))
    }

    if (res$status$code == 0) {
      bound <- res$solution[p]
      solved[1] <- TRUE
    } else  {
      bound <- NA
    }
    ntry <- ntry + 1
  }
  res <- bound

  return (res)
}
