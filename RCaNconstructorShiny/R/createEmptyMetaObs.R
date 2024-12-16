#' createEmptyMetaObs
#' creates an empty meta obs data frame
#' @return a data frame
#' 
#' @importFrom tibble tibble
#' @export
#' @example 
#' createEmptyMetaObs()
createEmptyMetaObs <- function(){
  tibble(id = character(),
             Observation = character(),
             Comment = character())
}
