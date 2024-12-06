#' createEmptyTimeLine
#'
#' @return an empty timeline datafame
#' @export
#' @importFrom tibble tibble
#'
#' @examples
#' createEmptyTimeLine()
#' 
createEmptyTimeLine <- function(){
  tibble(Date = vector("character"),
             Task = vector("character"),
             Annotation = vector("character"))
}