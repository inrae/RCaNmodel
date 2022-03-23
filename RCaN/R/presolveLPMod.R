#' presolveLPMod
#' simplify the lp problem using lpsolveAPI
#'
#' @param A a matrix of inequality constraints A x <=b
#' @param b a vector A x <=b
#' @param C a matrix of equality constraints C x = v, should be null in the
#' absence of such constraints
#' @param v a vector of equality constraints C x = v, should be null in the
#' absence of such constraints
#' @param lower minimal bounds for paramaters, by default set to zero
#' @param upper maximal bounds for parameters, by default Inifinty
#' @param sense min or max (maximisation of minimation)
#' @return a list with various elements about the presolved lp model
#'
#' @importFrom lpSolveAPI make.lp
#' @importFrom lpSolveAPI set.bounds
#' @importFrom lpSolveAPI set.rhs
#' @importFrom lpSolveAPI set.constr.type
#' @importFrom lpSolveAPI lp.control
#' @importFrom lpSolveAPI solve.lpExtPtr
#' @importFrom lpSolveAPI set.column
#' @importFrom lpSolveAPI get.column
#' @importFrom lpSolveAPI get.bounds
#' @importFrom lpSolveAPI get.rhs
#' @importFrom lpSolveAPI get.constr.type
#' @importFrom lpSolveAPI set.objfn
#' @importFrom lpSolveAPI get.primal.solution
#' @importFrom lpSolveAPI delete.lp
#' @importFrom ROI OP
#' @importFrom ROI L_constraint
#' @importFrom ROI V_bound
#' @importFrom ROI L_objective
#'

presolveLPMod <-
  function(A,
           b,
           C = NULL,
           v = NULL,
           lower = NULL,
           upper = NULL,
           sense = "max") {
    if (! sense %in% c("min", "max"))
      stop("error in presolve, sense should be min or max")
    maximum <- sense == "max"
    nbparam <- ncol(A)
    presolve <- c("rows",
                  "lindep",
                  "rowdominate",
                  "mergerows",
                  "coldominate")
    if (is.null(lower)) lower <- rep(0, ncol(A))
    if (is.null(upper)) upper <- rep(Inf, ncol(A))
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

    lp.control(lp_model, sense = sense, presolve = presolve)
    set.objfn(lp_model, rep(1, nbparam))
    res <- solve.lpExtPtr(lp_model)


    dir <- get.constr.type(lp_model)
    rhs <- get.rhs(lp_model)
    lhs <- matrix(0,length(rhs), dim(lp_model)[2])
    dimnames(lhs) <- dimnames(lp_model)
    names(rhs) <- dimnames(lp_model)[[1]]
    for (i in seq_len(dim(lp_model)[2]))
      lhs[get.column(lp_model,i)$nzrow[get.column(lp_model,i)$nzrow!=0], i] <-
      get.column(lp_model,i)$column[get.column(lp_model,i)$nzrow!=0]
    bounds <- get.bounds(lp_model)
    lower <- bounds$lower
    if (all(lower == 0)) lower <- NULL
    upper <- bounds$upper
    if (all(is.infinite(upper))) upper <- NULL
    fixed <- NA
    if (ncol(lhs) < ncol(A)){
      sol <- get.primal.solution(lp_model, orig = TRUE)[- (1:(nrow(A)+nrow(C)))]
      fixed <- sol[! colnames(A) %in% colnames(lhs)]
      names(fixed) <- colnames(A)[! colnames(A) %in% colnames(lhs)]
    }



    A2 <- lhs[dir == "<=", ]
    b2 <- rhs[dir == "<="]
    C2 <- lhs[dir == "=", ]
    v2 <- rhs[dir == "="]
    delete.lp(lp_model)
    if (nrow(lhs) > 0){
      OP <- defineLPMod(A2, b2, C2, v2, bounds$lower, bounds$upper,
                        maximum = maximum, ob = rep(1, ncol(lhs)))
    } else {
      OP <- NULL
    }
    res <- list(A = A2,
                b = b2,
                C = C2,
                v = v2,
                lower = lower,
                upper = upper,
                fixed = fixed,
                OP = OP)
    return (res)
  }

