#' getParamMinMax
#' Computes the minimum value bounds for a parameter p of a polytope defined
#' by+A.x<=b and C.x=v or by the polytope of the CaNmod object
#' @param OP an OP object
#' @param p index of the parameter
#' @param solution if TRUE, returns a list with the vector of solution
#' @return a vector with lower bounds and upper bounds
#'

getParamMinMax <- function(OP, p, solution = FALSE) {
  solved <- FALSE
  ntry <- 0
  while (ntry < 3 & !solved) {
    res <- ROI_solve(OP,
                     solver = "lpsolve",
                     control = list(
                       presolve = c( #rows not added to avoid removing a var
                       "lindep",
                       "rowdominate",
                       "mergerows"),
                       scaling = c("extreme",
                                   "equilibrate",
                                   "integers"),
                       timeout = 30))
    if (requireNamespace("ROI.plugin.cbc", quietly = TRUE)
        & res$status$msg$code != 0){
      res2 <- ROI_solve(OP,
                        solver = "cbc",
                        control = list(logLevel = 0))
      if (res2$status$msg$code == 0)
        res <- res2
    }
    
    if (res$status$msg$code == 0) {
      bound <- res$solution[p]
      sol <- res$solution  
      solved <- TRUE
    } else  {
      sol <- rep(NA, length(res$solution))
      bound <- NA
    }
    ntry <- ntry + 1
  }
  res <- bound
  if (solution)
    return (sol)
  return (res)
}
