#' getBoundParam
#' Computes the possible bounds for a parameter p of a polytope defined
#' by+A.x<=b and C.x=v
#' @param A the matrix of inequality A.x<=b
#' @param b the vector A.x<=b
#' @param p the index of the parameter for which the bounds should be computed
#' @param C the matrix of equality C.x=v (default NULL for no equality)
#' @param v the vector of equality C.x=v (default NULL for no equality
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

getBoundParam <- function(A, b, p, C = NULL, v = NULL) {
  nbparam <- ncol(A)
  if (is.null(C)) {
    C <- matrix(0, 0, nbparam)
    v <- numeric(0)
  }
  lp_model <- defineLPMod(A, b, C, v)
  ncontr <- length(get.constr.value(lp_model))
  solved <- c(FALSE,FALSE)
  ntry <- 0
  while (!all(solved) & ntry < 3) {
    set.objfn(lp_model, 1, p)
    lp.control(lp_model, sense = "max")
    res <- solve.lpExtPtr(lp_model)
    if (res == 0) {
      upbound <-
        (get.primal.solution(lp_model,
                             orig = TRUE)[(ncontr + 1):(ncontr + nbparam)])[p]
      solved[1] <- TRUE
    } else if (res == 3) {
      upbound <- Inf
      solved[1] <- TRUE
    } else {
      upbound <- NA
    }
    lp_model <- defineLPMod(A, b, C, v)
    lp.control(lp_model, sense = "min")
    set.objfn(lp_model, 1, p)
    res <- solve.lpExtPtr(lp_model)
    if (res == 0) {
      lowbound <-
        (get.primal.solution(lp_model,
                            orig = TRUE)[(ncontr + 1):(ncontr + nbparam)])[p]
      solved[2] <- TRUE
    } else if (res == 3) {
      lowbound <- -Inf
      solved[2] <- TRUE
    } else {
      lowbound <- NA
    }
    ntry <- ntry + 1
  }
  c(lowbound, upbound)
}
