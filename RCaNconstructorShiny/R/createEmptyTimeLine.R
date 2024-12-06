#' createEmptyTimeLine
#'
#' @return an empty timeline datafame
#' @export
#'
#' @examples
#' createEmptyTimeLine
#' 
createEmptyTimeLine <- function(){
  data.frame(Date = vector("character"),
             Task = vector("character"),
             Annotation = vector("character"))
}