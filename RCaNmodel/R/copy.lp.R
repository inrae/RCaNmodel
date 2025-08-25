#' copy.lp
#' This function is used to make a copy of a lpmodel that can be modified 
#' without changing the orginal lp
#' @param lps.model the original lpmodel
#'
#' @return a pointer to a copy of the original lpmodel
#' @export
#'
#' @examples
#' lps.model <- lpSolveAPI::make.lp(4, 0)
#' res <- copy.lp(lps.model)

copy.lp <- function(lps.model){
  res <- .Call(lpSolveAPI:::RlpSolve_copy_lp, lps.model)
  if(!is.null(res)) {
    reg.finalizer(res, lpSolveAPI::delete.lp, TRUE)
    oldClass(res) <- "lpExtPtr"
  }
  
  res
}
