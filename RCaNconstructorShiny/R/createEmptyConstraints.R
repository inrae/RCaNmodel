#' createEmptyConstraints
#' this function creates an empty constraints data.frame
#' @return an empty component data frame
#' @export
#'
createEmptyConstraints <- function(){
  data.frame(id = character(),
             Constraint = character(),
             `Time-range` = character(),
             Active = logical(),
             Comment = character(),
             idconstraint = character(),
             valid = logical())
}
