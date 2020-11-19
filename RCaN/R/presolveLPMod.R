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
#'
#' @return a vector corresponding to the centroid of the polytope
#'
#' @importFrom lpSolveAPI make.lp
#' @importFrom lpSolveAPI set.bounds
#' @importFrom lpSolveAPI set.rhs
#' @importFrom lpSolveAPI set.constr.type
#' @importFrom lpSolveAPI lp.control
#' @importFrom lpSolveAPI set.objfn
#' @importFrom lpSolveAPI solve.lpExtPtr
#' @importFrom lpSolveAPI set.column
#' @importFrom lpSolveAPI get.column
#' @importFrom lpSolveAPI get.bounds
#' @importFrom lpSolveAPI get.rhs
#' @importFrom lpSolveAPI get.constr.type


presolveLPMod <-
  function(A,
           b,
           C = NULL,
           v = NULL,
           lower = NULL) {
    presolve = c("rows", "lindep", "cols","rowdominate","coldominate","mergerows")
    nbparam <- ncol(A)
    presolve <- c("rows",
                  "lindep",
                  "rowdominate")
    if (is.null(lower)) lower <- rep(0, ncol(A))
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
    set.bounds(lp_model, lower = lower)
    lp.control(lp_model, "presolve" = presolve, "verbose" = "neutral")
    for (p in 1:nbparam) {
      set.column(lp_model, p, c(A[, p], C[, p]))
    }
    set.rhs(lp_model, c(b, v))
    set.constr.type(lp_model, c(rep("<=", nrow(A)), rep("=", nrow(C))))
    dimnames(lp_model) <- list(c(rownames(A), rownames(C)), colnames(A))

    set.objfn(lp_model, runif(nbparam))
    lp.control(lp_model, sense = "max")
    res <- solve.lpExtPtr(lp_model)


    dir <- get.constr.type(lp_model)
    rhs <- get.rhs(lp_model)
    lhs <- matrix(0,length(rhs),dim(lp_model)[2])
    for (i in seq_len(dim(lp_model)[2]))
      lhs[get.column(lp_model,i)$nzrow,i] <- get.column(lp_model,i)$column
    bounds <- get.bounds(lp_model)
    bounds <- list(lower = list(ind = which(bounds$lower != 0),
                                val = bounds$lower[bounds$lower!= 0]),
                   upper = list(ind = which(bounds$upper !=  0),
                                val = bounds$upper[bounds$upper != 0]))
    return (list(lhs = lhs,
                 dir = dir,
                 rhs = rhs,
                 bounds = bounds))
  }

