#' createEmptyAliases
#'
#' @return an empty aliases data.frame
#' @export
#'
createEmptyAliases <- function(){
  data.frame(Alias = character(0),
             Formula = character(0),
             Comment = character(0),
             id = character(0))
}
