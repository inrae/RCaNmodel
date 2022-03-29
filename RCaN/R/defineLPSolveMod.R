#' defineLPSolveMod
#' Specify a lpsolve odel (mainly for internal use)
#'
#' @param A a matrix of inequality constraints A x <=b
#' @param b a vector A x <=b
#' @param C a matrix of equality constraints C x = v, should be null in the
#' absence of such constraints
#' @param v a vector of equality constraints C x = v, should be null in the
#' absence of such constraints
#' @param lower minimal bounds for paramaters, by default set to zero
#' @param upper maximum bounds for paramaters, by default set to Inf
#' @param maximum tells whether the objective function is maximised or not
#' @param ob the coefficient of the ojective function (default all 1)
#'
#' @return returns an OP object
#'
#' @importFrom lpSolveAPI make.lp
#' @importFrom lpSolveAPI set.bounds
#' @importFrom lpSolveAPI set.rhs
#' @importFrom lpSolveAPI set.constr.type
#' @importFrom lpSolveAPI lp.control


defineLPSolveMod <-
  function(A,
           b,
           C = NULL,
           v = NULL,
           lower = NULL,
           upper = NULL,
           maximum=TRUE,
           ob = NULL) {
    sense <- ifelse(maximum, "max", "min")
    nbparam <- ncol(A)
    if (is.null(lower)) lower <- rep(0, ncol(A))
    if (is.null(upper)) upper <- rep(Inf, ncol(A))
    if (is.null(ob)) ob <- rep(1, ncol(A))
    if (is.null(C)) {
      C <- matrix(0, 0, nbparam)
      v <- numeric(0)
    }

    if (is.null(rownames(A)) & nrow(A) > 0) {
      rownames(A) <- paste("ineq", seq_len(nrow(A)))
    }
    if (is.null(rownames(C)) & nrow(C) > 0) {
      rownames(C) <- paste("eq", seq_len(nrow(C)))
    }
    if (is.null(colnames(A))) {
      colnames(A) <- paste("param", seq_len(ncol(A)))
    }
    lp_model <- make.lp(nrow(A) + nrow(C), nbparam)
    set.bounds(lp_model, lower = lower, upper = upper)
    for (p in 1:nbparam) {
      set.column(lp_model, p, c(A[, p], C[, p]))
    }
    set.rhs(lp_model, c(b, v))
    set.constr.type(lp_model, c(rep("<=", nrow(A)), rep("=", nrow(C))))
    dimnames(lp_model) <- list(c(rownames(A), rownames(C)), colnames(A))

    lp.control(lp_model, sense = sense)
    set.objfn(lp_model, ob)
    return (lp_model)
  }
