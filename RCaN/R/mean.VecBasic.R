#' mean.VecBasic
#' this function is for internal use
#' @param x a VecBasic
#' @param ... Additional arguments
#'
#'
#' @return the symbolic expression corresponding to the mean of x
#' @export


mean.VecBasic <- function(x, ...) {
  sum(x) / length(x)
}
