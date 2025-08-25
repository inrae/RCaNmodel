#' getConstraintWord
#'
#' @param constraint the equation of the constraint
#'
#' @return a vector with words of the formula
#' @export
#'
getConstraintWord <- function(constraint){
  strsplit(gsub('[[:space:]]', "", constraint),
           split = "(?=[/|\\+|=|<|\\*|>|\\-|\\)|\\(|\\[|\\]|\\:|, ])",
           perl=TRUE)[[1]]
}
