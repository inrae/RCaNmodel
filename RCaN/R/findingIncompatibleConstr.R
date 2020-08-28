#' findingIncompatibleConstraints
#' help to dectect constraints that leads to an empty polytope
#' @param A the matrix of inequality A.x<=b
#' @param b the vector A.x<=b
#' @param C the matrix of equality C.x=v (default NULL for no equality)
#' @param v the vector of equality C.x=v (default NULL for no equality
#'
#' @return a list of vector. To be fitted, constraints corresponding to the
#' first elements of each vector of the list should be relaxed.
#' Those constraints appear to be incompatible with following constraints in the
#'  Vector. For example, list(c(A,B),c(D,E, F)) means that A and B should be
#'  relaxed and that
#' A is incompatible with B and that D is incompatible with both E and F
#'
#' @importFrom lpSolveAPI add.column
#' @importFrom lpSolveAPI add.column
#'
#' @examples
#' n <- 20
#' A1 <- -diag(n)
#' b1 <- as.matrix(rep(0,n))
#' A2 <- diag(n)
#' b2 <- as.matrix(rep(1,n))
#' A <- rbind(A1,A2)
#' b <- rbind(b1,b2)
#' C <- matrix(c(1,rep(0,n-1)),1)
#' v <- 3
#' X0 <- findingIncompatibleConstr(A,b,C,v)
#'
#' @export


findingIncompatibleConstr <- function(A, b, C=NULL, v=NULL) {
  nbparam <- ncol(A)
  lp_model <- defineLPMod(A, b, C, v)
  nbineq <- nrow(A)
  if (is.null(C)) {
    C <- matrix(0, 0, nbparam)
    v <- numeric(0)
  }
  nbeq <- nrow(C)
  if (is.null(rownames(A))) {
    rownames(A) <- paste("inequality", seq_len(nrow(A)))
  }
  if (is.null(rownames(C)) & nrow(C) > 0) {
    rownames(C) <- paste("equality", seq_len(nrow(C)))
  }
  ####add slack variable for inequality constraint
  for (s in seq_len(nrow(A))) {
    slack <- rep(0, nbineq + nbeq)
    slack[s] <- -1
    add.column(lp_model, slack)
    dimnames(lp_model)[[2]][nbparam + s] <-
      paste("slack", rownames(A)[s])
  }
  for (s in seq_len(nrow(C))) {
    slack <- rep(0, nbineq + nbeq)
    slack[nbineq + s] <- -1
    add.column(lp_model, slack)
    add.column(lp_model, -slack)
    dimnames(lp_model)[[2]][nbparam + nbineq + 2 * (s - 1) + 1] <-
      paste("slack", rownames(C)[s])
    dimnames(lp_model)[[2]][nbparam + nbineq + 2 * (s - 1) + 2] <-
      paste("slackbis", rownames(C)[s])
  }
  contr_name <- dimnames(lp_model)[[1]]
  param_name <- dimnames(lp_model)[[2]]
  set.objfn(lp_model, c(rep(1, nbparam), rep(1000, nbineq + 2 * nbeq)))
  res <- solve.lpExtPtr(lp_model)
  solutions <-
    get.primal.solution(lp_model, orig = TRUE)[- (1:(nbeq + nbineq))]
  problematic <-
    param_name[which(solutions > 0 & (seq_len(length(solutions))) > (nbparam))]
  if (length(problematic) > 0) {
    results <- lapply(seq_len(length(problematic)), function(p) {
      lp_model <- defineLPMod(A, b, C, v)
      for (s in seq_len(nrow(A))) {
        if (paste("slack", rownames(A)[s]) != problematic[p]) {
          slack <- rep(0, nbineq + nbeq)
          slack[s] <- -1
          add.column(lp_model, slack)
          dimnames(lp_model)[[2]][length(dimnames(lp_model)[[2]])] <-
            paste("slack", rownames(A)[s])
        }
      }
      for (s in seq_len(nrow(C))) {
        if (paste("slack", rownames(C)[s]) != problematic[p] &
            paste("slackbis", rownames(C)[s]) != problematic[p]) {
          slack <- rep(0, nbineq + nbeq)
          slack[nbineq + s] <- -1
          add.column(lp_model, slack)
          add.column(lp_model, -slack)
          dimnames(lp_model)[[2]][length(dimnames(lp_model)[[2]]) - 1] <-
            paste("slack", rownames(C)[s])
          dimnames(lp_model)[[2]][length(dimnames(lp_model)[[2]])] <-
            paste("slackbis", rownames(C)[s])
        }
      }
      contr_name <- dimnames(lp_model)[[1]]
      param_name <- dimnames(lp_model)[[2]]
      set.objfn(lp_model, c(rep(1, nbparam), rep(1000, dim(lp_model)[2] -
                                                   nbparam)))
      res <- solve.lpExtPtr(lp_model)
      solutions <-
        get.primal.solution(lp_model, orig = TRUE)[- (1:(nbeq + nbineq))]
      c(gsub("^\\s*\\w*", "", problematic[p]),
        gsub("^\\s*\\w*", "",
             param_name[which(solutions > 0 &
                                (seq_len(length(solutions))) > (nbparam))]))
    })
    print("###polytope is ok when following constraints are relaxed:")
    print(paste(gsub("^\\s*\\w*","", problematic), collapse = ", "))
    print("####Those constraints seem incompatible with:")
    print(lapply(results,function(x)
      paste(x[1], ": ", paste(x[-1], collapse=", "), sep = "")))
    return(results)
  } else{
    return("no problem detected")
  }
}
