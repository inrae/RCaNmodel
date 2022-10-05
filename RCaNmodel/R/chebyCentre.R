#' chebyCentre
#' Computes the centroid of a convex polytope. chebyCentre and chebyCenter are
#' synonyms.
#'
#' @param A a matrix
#' @param b a vector of length equals to nrow(A)
#' @param lower lower bounds (per default 0)
#' @param upper upper bounds (per default Inf)
#'
#' @section Details:
#' This code is a translation of the matlab code that can be found there
#' (https://bit.ly/3b6Avav)
#' It computes the centroid of the complex polytope defined by
#' \eqn{A \cdot x \leqslant   b}
#'
#' @return a vector corresponding to the centroid of the polytope
#'
#' @examples
#' n <- 20
#' A1 <- -diag(n)
#' b1 <- as.matrix(rep(0,n))
#' A2 <- diag(n)
#' b2 <- as.matrix(rep(1,n))
#' A <- rbind(A1,A2)
#' b <- rbind(b1,b2)
#' X0 <- chebyCentre(A,b)
#' @export

chebyCentre <- function(A, b, lower = NULL, upper = NULL) {
  if (!is.null(lower)){
    nonnull <- which(is.finite(lower))
    if (length(nonnull) > 0){
      bounds <- matrix(0, length(nonnull), ncol(A))
      bounds[cbind(seq_len(length(nonnull)),
                   nonnull)] <- - 1
      A <- rbind(A, bounds)
      b <- c(b, -lower[nonnull])
    }
    lower = c(lower,-Inf)
  } else{
    A <- rbind(A, -diag(ncol(A)))
    b <- c(b, rep(0, ncol(A)))
  }
  
  if (!is.null(upper)){
    nonnull <- which(is.finite(upper))
    if (length(nonnull) > 0){
      bounds <- matrix(0, length(nonnull), ncol(A))
      bounds[cbind(seq_len(length(nonnull)),
                   nonnull)] <- - 1
      A <- rbind(A, bounds)
      b <- c(b, -upper[nonnull])
    }
    upper = c(upper, Inf)
  } else{
    A <- rbind(A, diag(ncol(A)))
    b <- c(b, rep(Inf, ncol(A)))
  }
  
  n <- dim(A)[1]
  p <- dim(A)[2]
  
  an <- rowSums(A ^ 2) ^ 0.5
  A1 <- matrix(data = 0,
               nrow = n,
               ncol = p + 1)
  A1[, 1:p] <- A

  A1[, p + 1] <- an

  f <- rep(0, p + 1)
  f[p + 1] <- -1

  lp_mod <- defineLPMod(A1,
                        b,
                        lower = lower,
                        upper = upper,
                        maximum = FALSE,
                        ob = f)

  res <- ROI_solve(lp_mod, solver = "lpsolve",
                   control = list(presolve = c("rows",
                                               "lindep",
                                               "rowdominate",
                                               "mergerows")))

  if (requireNamespace("ROI.plugin.cbc", quietly = TRUE)
      & res$status$code == 5){
    res <- ROI_solve(lp_mod,
                     solver = "cbc",
                     control = list(logLevel = 0))
  }

  x <- res$solution

  return(x[-p - 1])
}

#' @rdname chebyCentre
#' @export
chebyCenter <- chebyCentre
