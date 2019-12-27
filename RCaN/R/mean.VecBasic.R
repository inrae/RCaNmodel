#' mean.VecBasic
#' this function is for internal use
#' @param x a VecBasic
#'
#'
#' @return the symbolic expression corresponding to the mean of x


mean.VecBasic <- function(x) {
  sum(x) / length(x)
}
