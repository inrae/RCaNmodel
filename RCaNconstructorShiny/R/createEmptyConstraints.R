#' createEmptyConstraints
#' this function creates an empty constraints data.frame
#' @return an empty component data frame
#' @importFrom tibble tibble
#' @export
#'
createEmptyConstraints <- function(){
  tibble(id = character(0),
         Id = character(0),
         Constraint = character(0),
         `Time-range` = character(0),
         Active = logical(0),
         Comment = character(0),
         idconstraint = character(0),
         valid = logical(0),
         validity_comments = character(0))
}
