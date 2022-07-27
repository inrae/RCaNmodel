#' chebyCentre
#' Computes the centroid of a convex polytope. chebyCentre and chebyCenter are
#' synonyms.
#'
#' @param A a matrix
#' @param b a vector of length equals to nrow(A)
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

chebyCentre <- function(A, b) {
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
                        maximum = FALSE,
                        ob = f)
  if (requireNamespace("ROI.plugin.cbc", quietly = TRUE)){
    res <- ROI_solve(lp_model,
                     solver = "cbc",
                     control = list(logLevel = 0))
  } else {
    res <- ROI_solve(lp_mod, solver = "lpsolve",
                     control = list(presolve = c("rows",
                                                 "lindep",
                                                 "rowdominate",
                                                 "mergerows")))
  }
  if (res$status$code != 0 &
      requireNamespace("ROI.plugin.clp", quietly = TRUE)){
    res <- ROI_solve(lp_mod, solver = "clp", control = list(amount = 0))
  }

  x <- res$solution

  return(x[-p - 1])
}

#' @rdname chebyCentre
#' @export
chebyCenter <- chebyCentre
