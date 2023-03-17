#' findInitPoint
#'
#' find a random initial point by averaging results of random lp problems
#' @param A a matrix of inequality constraints A x <=b
#' @param b a vector A x <=b
#' @param C a matrix of equality constraints C x = v, should be null in the
#' absence of such constraints
#' @param v a vector of equality constraints C x = v, should be null in the
#' absence of such constraints
#' @return a random initial point or a vector of NA if enable to find a solution
#' @export
#'
#' @examples
#' n <- 20
#' A1 <- -diag(n)
#' b1 <- as.matrix(rep(0,n))
#' A2 <- diag(n)
#' b2 <- as.matrix(rep(1,n))
#' A <- rbind(A1,A2)
#' b <- rbind(b1,b2)
#' findInitPoint(A = A, b = b)
#'
#' @export
#'
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel clusterEvalQ
#' @importFrom parallel clusterExport
#' @importFrom parallel stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom doParallel stopImplicitCluster
#' @importFrom coda mcmc
#' @importFrom coda mcmc.list
#' @importFrom foreach foreach
#' @importFrom foreach %dopar%
#' @importFrom foreach %do%
#' @importFrom stats runif
findInitPoint <- function(A, 
                          b,
                          C = NULL,
                          v = NULL) {
  
  
  X0 <- replicate(100, {
    find_init <- FALSE
    nbiter <- 0
    x0 <- rep(NA, ncol(A))
    while (nbiter < 100 & !find_init) {
      lp_model <- defineLPMod(A, b, C, v,
                              maximum = runif(1) > 0.5,
                              ob = runif(ncol(A)))
      
      res <- RCaNmodel:::ROI_solve(lp_model, solver = "lpsolve",
                                   control = list(presolve = c("rows",
                                                               "lindep",
                                                               "rowdominate",
                                                               "mergerows"),
                                                  scaling = c("extreme",
                                                              "equilibrate",
                                                              "integers")))
      if (requireNamespace("ROI.plugin.cbc", quietly = TRUE) &
          res$status$msg$code == 5){
        res <- RCaNmodel:::ROI_solve(lp_model,
                                     solver = "cbc",
                                     control = list(logLevel = 0))
      }
      
      x0 <- res$solution
      
      if (res$status$msg$code == 0)
        find_init <- TRUE
      nbiter <- nbiter + 1
    }
    x0
  })
  return (rowMeans(X0, na.rm = TRUE))
  
}