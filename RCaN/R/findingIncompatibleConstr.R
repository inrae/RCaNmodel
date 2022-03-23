#' findingIncompatibleConstraints
#' help to dectect constraints that leads to an empty polytope defined
#' by+A.x<=b and C.x=v or by the polytope of the CaNmod object
#' @param x either a CaNmod oject or a named list with at least a matrix A and
#' a vector b (A.x<=b) and optionnally a matrix C and a vector v (C.x=v)
#'
#' @return a list of vector. To be fitted, constraints corresponding to the
#' first elements of each vector of the list should be relaxed.
#' Those constraints appear to be incompatible with following constraints in the
#'  Vector. For example, list(c(A,B),c(D,E, F)) means that A and B should be
#'  relaxed and that
#' A is incompatible with B and that D is incompatible with both E and F
#'
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
#' X0 <- findingIncompatibleConstr(list(A = A, b = b, C = C, v = v))
#'
#' @export


findingIncompatibleConstr <- function(x) {
  x <- reformatX(x)
  A <- x$A
  b <- x$b
  C <- x$C
  v <- x$v

  nbparam <- ncol(A)
  nbineq <- nrow(A)
  if (is.null(C)) {
    C <- matrix(0, 0, nbparam)
    v <- numeric(0)
  }
  Aslacked <- as.matrix(A)
  bslacked <- b
  Cslacked <- as.matrix(C)
  vslacked <- v
  nbeq <- nrow(C)
  if (is.null(rownames(A))) {
    rownames(A) <- paste("inequality", seq_len(nrow(A)))
  }
  if (is.null(rownames(C)) & nrow(C) > 0) {
    rownames(C) <- paste("equality", seq_len(nrow(C)))
  }
  if (is.null(colnames(A)))
    colnames(A) <- paste("p", seq_len(nbparam), sep = "")
  param_name <- colnames(A)
  ####add slack variable for inequality constraint
  for (s in seq_len(nrow(A))) {
    slack <- rep(0, nbineq)
    slack[s] <- -1
    Aslacked <- cbind(Aslacked, slack)
    Cslacked <- cbind(Cslacked, rep(0, nbeq))
    param_name <- c(param_name,
                    paste("slack", rownames(A)[s]))
  }
  for (s in seq_len(nrow(C))) {
    slack <- rep(0, nbeq)
    slack[s] <- -1
    Aslacked <- cbind(Aslacked, rep(0 ,nbineq), rep(0,nbineq))
    Cslacked <- cbind(Cslacked, slack, -slack)
    param_name <- c(param_name,
                    paste("slack", rownames(C)[s]),
                    paste("slackbis", rownames(C)[s]))
  }

  lp_model <- defineLPMod(Aslacked, bslacked, Cslacked, vslacked,
                          ob = c(rep(1, nbparam), rep(1000, nbineq + 2 * nbeq)),
                          maximum = FALSE)
  res <- ROI_solve(lp_model, solver = "lpsolve",
                   control = list(presolve = c("rows",
                                                "lindep",
                                                "rowdominate",
                                                "mergerows")))
  file.remove(lp_model$lp_model)

  if (requireNamespace("ROI.plugin.clp", quietly = TRUE)
      & res$status$code == 5 ){
    res <- ROI_solve(lp_model, solver = "clp", control = list(amount = 0))
  }

  solutions <- res$solution
  problematic <-
    param_name[which(solutions > 0 & (seq_len(length(solutions))) > (nbparam))]
  if (length(problematic) > 0) {
    results <- lapply(seq_len(length(problematic)), function(p) {
      Aslacked <- as.matrix(A)
      bslacked <- b
      Cslacked <- as.matrix(C)
      vslacked <- v
      param_name <- colnames(A)
      for (s in seq_len(nrow(A))) {
        if (paste("slack", rownames(A)[s]) != problematic[p]) {
          slack <- rep(0, nbineq)
          slack[s] <- -1
          Aslacked <- cbind(Aslacked, slack)
          Cslacked <- cbind(Cslacked, rep(0, nbeq))
          param_name <- c(param_name,
                          paste("slack", rownames(A)[s]))
        }
      }
      for (s in seq_len(nrow(C))) {
        if (paste("slack", rownames(C)[s]) != problematic[p] &
            paste("slackbis", rownames(C)[s]) != problematic[p]) {
          slack <- rep(0, nbeq)
          slack[s] <- -1
          Cslacked <- cbind(Cslacked, slack, -slack)
          Aslacked <- cbind(Aslacked, rep(0, nbineq), rep(0, nbineq))
          param_name <- c(param_name,
                          paste("slack", rownames(C)[s]),
                          paste("slackbis", rownames(C)[s]))
        }
      }
      lp_model <- defineLPMod(Aslacked, bslacked, Cslacked, vslacked,
                              ob= c(rep(1, nbparam),
                                    rep(1000,
                                        ncol(Aslacked) - nbparam)),
                              maximum = FALSE)

      res <- ROI_solve(lp_model, solver = "lpsolve",
                       control <- list(presolve = c("rows",
                                                    "lindep",
                                                    "rowdominate",
                                                    "mergerows")))
      if (requireNamespace("ROI.plugin.clp", quietly = TRUE) &
          res$status$code == 5){
        res <- ROI_solve(lp_model, solver = "clp", control = list(amount = 0))
      }
      file.remove(lp_model$lp_model)

      solutions <- res$solution
      c(gsub("^\\s*\\w*", "", problematic[p]),
        gsub("^\\s*\\w*", "",
             param_name[which(solutions > 0 &
                                (seq_len(length(solutions))) > (nbparam))]))
    })
    print("###polytope is ok when following constraints are relaxed:")
    print(paste(gsub("^\\s*\\w*", "", problematic), collapse = ", "))
    print("####Those constraints seem incompatible with:")
    print(lapply(results, function(x)
      paste(x[1], ": ", paste(x[-1], collapse = ", "), sep = "")))
    return(results)
  } else{
    return("no problem detected")
  }
}
