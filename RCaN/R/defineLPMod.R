#' defineLPMod
#' Specify an lp_model (mainly for internal use)
#'
#' @param A a matrix of inequality constraints A x <=b
#' @param b a vector A x <=b
#' @param C a matrix of equality constraints C x = v, should be null in the
#' absence of such constraints
#' @param v a vector of equality constraints C x = v, should be null in the
#' absence of such constraints
#' @param lower minimal bounds for paramaters, by default set to zero
#' @param maximum tells whether the objective function is maximised or not
#' @param ob the coefficient of the ojective function (default all 1)
#'
#' @return returns an OP object
#'
#' @importFrom ROI OP
#' @importFrom ROI L_constraint
#' @importFrom ROI V_bound
#' @importFrom ROI L_objective
#'


defineLPMod <-
  function(A,
           b,
           C = NULL,
           v = NULL,
           lower = NULL,
           maximum=TRUE,
           ob = NULL) {
    nbparam <- ncol(A)
    if (is.null(lower)) lower <- rep(0, ncol(A))
    if (is.null(ob)) ob <- rep(1, ncol(A))
    if (is.null(C)) {
      C <- matrix(0, 0, nbparam)
      v <- numeric(0)
    }
    dir <- c(rep("<=", nrow(A)),rep("==", nrow(C)))
    rhs <- c(b,v)
    lfs <- as.matrix(rbind(A,C))
    if (all(lower == 0)){
      return (OP(constraints = L_constraint(lfs,dir,rhs),
                 maximum = maximum,
                 objective = L_objective(ob)))
    } else {
      return (OP(V_bound(li = which(lower != 0),
                         ld = lower[lower != 0]),
                 constraints = L_constraint(lfs,dir,rhs),
                 maximum = maximum,
                 objective = L_objective(ob)))

    }

  }
