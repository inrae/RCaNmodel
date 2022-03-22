#' getAllBoundsParam
#' Computes the possible bounds for all parameters of a polytope defined by
#' A.x<=b and C.x=v or by a CaNmod object
#' @param x either a CaNmod oject or a named list with at least a matrix A and
#' a vector b (A.x<=b) and optionnally a matrix C and a vector v (C.x=v)
#' @param progressBar should a progress bar be displayed (default TRUE)

#'
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @importFrom ROI objective
#' @importFrom ROI L_objective
#'
#' @return a datafame with first column corresponding to colnames(A), and
#' corresponding lower bounds (column 2) and upper bounds (column 3)
#' @examples
#' n <- 20
#' A1 <- -diag(n)
#' b1 <- as.matrix(rep(0,n))
#' A2 <- diag(n)
#' b2 <- as.matrix(rep(1,n))
#' A <- rbind(A1,A2)
#' b <- rbind(b1,b2)
#' X0 <- getAllBoundsParam(list(A = A, b = b))
#' @export

getAllBoundsParam <- function(x,
                              progressBar = TRUE) {
  x <- reformatX(x)
  A <- x$A
  b <- x$b
  C <- x$C
  v <- x$v


  nbparam <- ncol(A)
  if (is.null(colnames(A))) {
    colnames(A) <- paste("col", seq_len(ncol(A)), sep = "")
  }
  if (is.null(colnames(C)) & !is.null(C))
    colnames(C) <- colnames(A)

  presolvedmin <- presolveLPMod(A, b, C, v, sense = "min")
  presolvedmax <- presolveLPMod(A, b, C, v, sense = "max")




  if (progressBar)
    pb <- txtProgressBar(min = 0, max = nbparam, style = 3)
  bounds <- sapply(1:nbparam, function(p) {
    if (progressBar)
      setTxtProgressBar(pb, p)
    sapply(c("min", "max"), function(s){
      if (s == "min"){
        maximum <- FALSE
        presolved <- presolvedmin
      } else {
        maximum <- TRUE
        presolved <- presolvedmax
      }
      if (!colnames(A)[p] %in% names(presolved$fixed)){
        ip <- match(colnames(A)[p], colnames(presolved$A))
        ob <- rep(0, ncol(presolved$A))
        ob[ip] <- 1
        ROI::objective(presolved$OP) <- L_objective(ob)

        presolved$OP$lp_model <- defineLPSolveMod(presolved$A,
                                                  presolved$b,
                                                  presolved$C,
                                                  presolved$v,
                                                  presolved$lower,
                                                  presolved$upper,
                                                  maximum,
                                                  ob)
        set.objfn(presolved$OP$lp_model, ob)
        getParamMinMax(presolved$OP, ip)
      } else {
        presolved$fixed[colnames(A)[p]]
      }
    })
  })
  data.frame(
    param = colnames(A),
    lowerbound = bounds[1, ],
    upperbound = bounds[2, ]
  )
}
