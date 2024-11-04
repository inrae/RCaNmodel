#' createEmptyMetaObs
#' creates an empty meta obs data frame
#' @return a data frame
#' @export
#'
createEmptyMetaObs <- function(){
  data.frame(id = character(),
             Observation = character(),
             Comment = integer())
}
