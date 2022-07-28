
#' checkPolytopeStatus
#' check if the polytope has solution or not
#' @param x either a CaNmod oject or a named list with at least a matrix A and
#' a vector b (A.x<=b) and optionnally a matrix C and a vector v (C.x=v)
#' @return print a message to tell if the polygon is ok or not
#'
#' @examples
#' n <- 20
#' A1 <- -diag(n)
#' b1 <- as.matrix(rep(0,n))
#' A2 <- diag(n)
#' b2 <- as.matrix(rep(1,n))
#' A <- rbind(A1,A2)
#' b <- rbind(b1,b2)
#' X0 <- checkPolytopeStatus(list(A = A, b = b))
#'
#' #this one is empty
#' C <- matrix(c(1,rep(0,n-1)),1)
#' v <- 3
#' X0 <- checkPolytopeStatus(list(A = A, b = b, C = C, v = v))
#'
#' #example with a CaNmod object
#' myCaNmod <- buildCaN(system.file("extdata", "CaN_template_mini.xlsx",
#'  package = "RCaN"))
#' checkPolytopeStatus(myCaNmod)
#'
#' #we artificially add incompatible constraints (negative flow)
#' myCaNmod$A <- rbind(myCaNmod$A,c(1,rep(0,ncol(myCaNmod$A)-1)))
#' rownames(myCaNmod$A)[nrow(myCaNmod$A)]<-"neg_flow"
#' myCaNmod$b <- c(myCaNmod$b,-1)
#' checkPolytopeStatus(myCaNmod)
#'
#' @export

checkPolytopeStatus <- function(x) {
  x <- reformatX(x)
  A <- x$A
  b <- x$b
  C <- x$C
  v <- x$v


  nbparam <- ncol(A)
  lp_model <- defineLPMod(A, b, C, v, maximum = FALSE)
  if (requireNamespace("ROI.plugin.cbc", quietly = TRUE)){
    res <- ROI_solve(lp_model,
                     solver = "cbc",
                     control = list(logLevel = 0))
  } else {
    res <- ROI_solve(lp_model, solver = "lpsolve",
                     control = list(presolve = c("rows",
                                                 "lindep",
                                                 "rowdominate",
                                                 "mergerows"),
                                    scaling = c("extreme",
                                                "equilibrate",
                                                "integers")))
  }

  if (res$status$code == 0) {
    lp_model <- defineLPMod(A, b, C, v, maximum = TRUE)
    if (requireNamespace("ROI.plugin.cbc", quietly = TRUE)){
      res <- ROI_solve(lp_model,
                       solver = "cbc",
                       control = list(logLevel = 0))
    } else {
      res <- ROI_solve(lp_model, solver = "lpsolve",
                       control = list(presolve = c("rows",
                                                   "lindep",
                                                   "rowdominate",
                                                   "mergerows"),
                                      scaling = c("extreme",
                                                  "equilibrate",
                                                  "integers")))
    }


  }
  if (res$status$code == 0) {
    print("polytope ok")
  } else if (res$status$code == 2) {
    print("empty polytope")
  } else if (res$status$code == 3) {
    print("polytope not bounded")
  } else if (res$status$code == 9) {
    print("unique solution")
  } else if (res$status$code == 5) {
    print("numerical error")
  } else {
    print("potential problem")
  }
}
