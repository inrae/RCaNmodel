#' defineLPMod
#' Specify a ROI (mainly for internal use)
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
#' @importFrom ROI OP
#' @importFrom ROI L_constraint
#' @importFrom ROI V_bound
#' @importFrom ROI L_objective



defineLPMod <-
  function(A,
           b,
           C = NULL,
           v = NULL,
           lower = NULL,
           upper = NULL,
           maximum = TRUE,
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


    dir <- c(rep("<=", nrow(A)),rep("==", nrow(C)))
    rhs <- c(b,v)
    lfs <- as.matrix(rbind(A,C))
    bounds <- V_bound(ui = seq_len(nbparam),
                      li = seq_len(nbparam),
                      ub = upper,
                      lb = lower,
                      ud = Inf, ld = 0, nobj = nbparam)

    if (all(lower == 0) & all(upper == Inf)){
      OP <- OP(constraints = L_constraint(lfs,dir,rhs),
               maximum = maximum,
               objective = L_objective(ob))
    } else {
      OP <- OP(bounds = bounds,
               constraints = L_constraint(lfs,dir,rhs),
               maximum = maximum,
               objective = L_objective(ob))
    }
    OP$lp_model <- defineLPSolveMod(A,
                                    b,
                                    C = C,
                                    v = v,
                                    lower = lower,
                                    upper = upper,
                                    maximum = maximum,
                                    ob = ob)
    return (OP)
  }


