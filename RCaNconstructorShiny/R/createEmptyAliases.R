#' createEmptyAliases
#'
#' @return an empty aliases data.frame
#' @importFrom tibble tibble
#' @export
#' @examples 
#' createEmptyAliases()
#'
createEmptyAliases <- function(){
  tibble(Alias = character(0),
             Formula = character(0),
             Comment = character(0),
             id = character(0),
             idconstraint = character(0),
             valid = logical(0),
             validity_comments = character(0))
}
