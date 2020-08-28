
#' checkPolytopeStatus
#' check if the polytope has solution or not
#' @param A the matrix of inequality A.x<=b
#' @param b the vector A.x<=b
#' @param C the matrix of equality C.x=v (default NULL for no equality)
#' @param v the vector of equality C.x=v (default NULL for no equality
#' @return print a message to tell if the polygon is ok or not
#'
#' @importFrom utils setTxtProgressBar
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_polygon
#' @importFrom lpSolveAPI lp.control
#' @importFrom lpSolveAPI solve.lpExtPtr
#' @importFrom lpSolveAPI get.constr.value
#' @importFrom lpSolveAPI set.objfn
#' @examples
#' n <- 20
#' A1 <- -diag(n)
#' b1 <- as.matrix(rep(0,n))
#' A2 <- diag(n)
#' b2 <- as.matrix(rep(1,n))
#' A <- rbind(A1,A2)
#' b <- rbind(b1,b2)
#' X0 <- checkPolytopeStatus(A,b)
#'
#' #this one is empty
#' C <- matrix(c(1,rep(0,n-1)),1)
#' v <- 3
#' X0 <- checkPolytopeStatus(A,b,C,v)
#'
#' @export

checkPolytopeStatus <- function(A,
                                b,
                                C = NULL,
                                v = NULL) {
  nbparam <- ncol(A)
  lp_model <- defineLPMod(A, b, C, v)
  lp.control(lp_model, sense = "min")
  set.objfn(lp_model, rep(1, nbparam))
  res <- solve.lpExtPtr(lp_model)
  if (res == 0) {
    nbparam <- ncol(A)
    lp_model <- defineLPMod(A, b, C, v)
    lp.control(lp_model, sense = "max")
    set.objfn(lp_model, rep(1, nbparam))
    res <- solve.lpExtPtr(lp_model)
  }
  if (res == 0) {
    print("polytope ok")
  } else if (res == 2) {
    print("empty polytope")
  } else if (res == 3) {
    print("polytope not bounded")
  } else if (res == 9) {
    print("unique solution")
  } else if (res == 5) {
    print("numerical error")
  } else {
    print("potential problem")
  }
}
