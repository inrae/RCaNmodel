#' defineLPMod
#' Specify an lp_model (mainly for internal use) and writes an lp model file
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
#' @importFrom lpSolveAPI make.lp
#' @importFrom lpSolveAPI set.bounds
#' @importFrom lpSolveAPI set.rhs
#' @importFrom lpSolveAPI set.constr.type
#' @importFrom lpSolveAPI lp.control
#' @importFrom lpSolveAPI write.lp


defineLPMod <-
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
    write.lp(lp_model,
              paste0(tempdir(), "/lp_mod.lp"),
              "lp",
              c(FALSE,FALSE))
    return (OP)
  }
