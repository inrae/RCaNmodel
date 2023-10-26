#' getAllBoundsParam
#' Computes the possible bounds for all parameters of a polytope defined by
#' A.x<=b and C.x=v or by a CaNmod object
#' @param x either a CaNmod oject or a named list with at least a matrix A and
#' a vector b (A.x<=b) and optionnally a matrix C and a vector v (C.x=v)
#' @param lower minimal bounds for paramaters, by default set to zero
#' @param upper maximum bounds for paramaters, by default set to Inf
#' @param progressBar should a progress bar be displayed (default TRUE)
#' @param solution if TRUE returns vectors of solutions
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @importFrom ROI objective
#' @importFrom ROI L_objective
#' @importFrom lpSolveAPI set.objfn
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
                              lower = NULL,
                              upper = NULL,
                              progressBar = TRUE,
                              solution = FALSE) {
  
  x <- reformatX(x)
  A <- x$A
  b <- x$b
  C <- x$C
  v <- x$v
  
  if (is.null(lower)) lower <- rep(0, ncol(A))
  if (is.null(upper)) upper <- rep(Inf, ncol(A))
  nbparam <- ncol(A)
  if (is.null(colnames(A))) {
    colnames(A) <- paste("col", seq_len(ncol(A)), sep = "")
  }
  if (is.null(colnames(C)) & !is.null(C))
    colnames(C) <- colnames(A)
  
  presolvedmin <- presolveLPMod(A, b, C, v, 
                                sense = "min",
                                lower = lower,
                                upper = upper)
  presolvedmax <- presolveLPMod(A, b, C, v, sense = "max",
                                lower = lower,
                                upper = upper)
  if (nrow(presolvedmin$A) == 0){
    presolvedmin <- list(A = A,
                         b = b,
                         C = C,
                         V = v,
                         lower = lower,
                         upper = upper,
                         fixed = integer(0),
                         OP = defineLPMod(A, b, C, v, 
                                          maximum = FALSE,
                                          lower = lower,
                                          upper = upper))
  }
  
  if (nrow(presolvedmax$A) == 0){
    presolvedmax <- list(A = A,
                         b = b,
                         C = C,
                         V = v,
                         lower = lower,
                         upper = upper,
                         fixed = integer(0),
                         OP = defineLPMod(A, b, C, v, 
                                          maximum = TRUE,
                                          lower = lower,
                                          upper = upper))
  }
  
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
      copy_lp_mod <- copy.lp(presolved$OP$lp_model)
      if (!colnames(A)[p] %in% names(presolved$fixed)){
        ip <- match(colnames(A)[p], colnames(presolved$A))
        ob <- rep(0, ncol(presolved$A))
        ob[ip] <- 1
        ROI::objective(presolved$OP) <- L_objective(ob)
        
        set.objfn(presolved$OP$lp_model, ob)
        res <- getParamMinMax(presolved$OP, ip, solution)
        if (solution) {
          res2 <- rep(NA, ncol(A))
          res2[colnames(A) %in% names(presolved$fixed)] <- presolved$fixed
          res2[!colnames(A) %in% names(presolved$fixed)] <- res
          res <- res2
        }
      } else {
        res <- presolved$fixed[colnames(A)[p]]
        if (solution) {
          res2 <- rep(NA, ncol(A))
          res2[colnames(A) %in% names(presolved$fixed)] <- presolved$fixed
          res <- res2
        }
      }
      presolved$OP$lp_model <- copy_lp_mod
      res
    })
  })
  if (!solution){
    return(data.frame(
      param = colnames(A),
      lowerbound = bounds[1, ],
      upperbound = bounds[2, ]))
  } else {
    return(bounds)
  }
  
}
