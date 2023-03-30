#' findInitPoint
#'
#' find a random initial point by averaging results of random lp problems
#' @param A a matrix of inequality constraints A x <=b
#' @param b a vector A x <=b
#' @param C a matrix of equality constraints C x = v, should be null in the
#' absence of such constraints
#' @param v a vector of equality constraints C x = v, should be null in the
#' absence of such constraints
#' @param lower minimal bounds for paramaters, by default set to zero
#' @param upper maximum bounds for paramaters, by default set to Inf
#' @param nbpoints number of points use build the initial point, each point
#' requires the lpsolve run so increases time, but should improve the chain
#' exploration
#' @param progressBar a switch to indicate whether a progress bar is wanted
#' @return a random initial point or a vector of NA if enable to find a solution
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @importFrom lpSolveAPI set.objfn
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
#' @importFrom stats runif
findInitPoint <- function(A, 
                          b,
                          C = NULL,
                          v = NULL,
                          lower = NULL,
                          upper = NULL,
                          nbpoints = 100,
                          progressBar = FALSE) {
  if (is.null(lower)) lower <- rep(0, ncol(A))
  if (is.null(upper)) upper <- rep(Inf, ncol(A))
  
  print("## searching intial values")
  if (progressBar)
    pb <- txtProgressBar(min = 0, max = nbpoints, style = 3)
  lp_model <- defineLPMod(A, b, C, v,
                          maximum = FALSE,
                          lower = lower,
                          upper = upper,
                          ob = runif(ncol(A), -1, 1))
  res <- ROI_solve(lp_model, solver = "lpsolve",
                   control = list(presolve = c("rows",
                                               "lindep",
                                               "rowdominate",
                                               "mergerows"),
                                  scaling = c("extreme",
                                              "equilibrate",
                                              "integers")))
  X0 <- sapply(seq_len(ncol(A)), function(i) {
    if (progressBar)
      setTxtProgressBar(pb, i)
    find_init <- FALSE
    nbiter <- 0
    x0 <- rep(NA, ncol(A))
    while (nbiter < 100 & !find_init) {
      ob <- rep(0, ncol(A))
      ob[i] <- 1
      set.objfn(lp_model$lp_model, ob)
      res <- ROI_solve(lp_model, solver = "lpsolve")
      if (res$status$msg$code == 0) {
        find_init <- TRUE
        x0 <- res$solution
      } else {
        lp_model <- defineLPMod(A, b, C, v,
                                maximum = FALSE,
                                lower = lower,
                                upper = upper,
                                ob = runif(ncol(A), -1, 1))
        res <- ROI_solve(lp_model, solver = "lpsolve",
                         control = list(presolve = c("rows",
                                                     "lindep",
                                                     "rowdominate",
                                                     "mergerows"),
                                        scaling = c("extreme",
                                                    "equilibrate",
                                                    "integers")))
      }
      nbiter <- nbiter + 1
    }
    x0
  })
  X02 <- sapply(seq_len(ncol(A)), function(i) {
    if (progressBar)
      setTxtProgressBar(pb, i)
    find_init <- FALSE
    nbiter <- 0
    x0 <- rep(NA, ncol(A))
    while (nbiter < 100 & !find_init) {
      ob <- rep(0, ncol(A))
      ob[i] <- -1
      set.objfn(lp_model$lp_model, ob)
      res <- ROI_solve(lp_model, solver = "lpsolve")
      if (res$status$msg$code == 0) {
        find_init <- TRUE
        x0 <- res$solution
      } else {
        lp_model <- defineLPMod(A, b, C, v,
                                maximum = FALSE,
                                lower = lower,
                                upper = upper,
                                ob = runif(ncol(A), -1, 1))
        res <- ROI_solve(lp_model, solver = "lpsolve",
                         control = list(presolve = c("rows",
                                                     "lindep",
                                                     "rowdominate",
                                                     "mergerows"),
                                        scaling = c("extreme",
                                                    "equilibrate",
                                                    "integers")))
      }
      nbiter <- nbiter + 1
    }
    x0
  })
  return (rowMeans(cbind(X0, X02), na.rm = TRUE))
  
}