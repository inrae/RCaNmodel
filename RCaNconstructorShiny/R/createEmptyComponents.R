#' createEmptyComponents
#' this function creates an empty components data.reme
#' @return an empty component data frame
#' @importFrom tibble tibble
#' @export
#'
createEmptyComponents <- function(){
  tibble(id = character(),
             Component = character(),
             Inside = integer(),
             AssimilationE = numeric(),
             Digestibility = numeric(),
             OtherLosses = numeric(),
             Inertia = numeric(),
             Satiation = numeric(),
             RefugeBiomass = numeric(),
             x = numeric(),
             y = numeric())
}
