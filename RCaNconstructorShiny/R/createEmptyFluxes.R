#' createEmptyFluxes
#' creates an empty fluxes data frame
#' @return a data frame
#' @export
#'
createEmptyFluxes <- function(){
  data.frame(id = character(),
             Flux = character(),
             to = character(),
             from = character(),
             From = character(),
             To = character(),
             Trophic = integer())
}
