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
#' @param progressBar a switch to indicate whether a progress bar is wanted
#' @return a random initial point or a vector of NA if enable to find a solution
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @importFrom lpSolveAPI set.objfn
#' @importFrom lpSolveAPI lp.control
#' @importFrom ROI objective
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
                          progressBar = FALSE) {
  if (is.null(lower)) lower <- rep(0, ncol(A))
  if (is.null(upper)) upper <- rep(Inf, ncol(A))
  
  writeLines("## searching intial values")
  angles <- getAllBoundsParam(list(A = A,
                                   b = b,
                                   C = C,
                                   v = v),
                              lower = lower,
                              upper = upper,
                              progressBar = progressBar,
                              solution = TRUE)
  return (colMeans(angles, na.rm = TRUE))
  
}