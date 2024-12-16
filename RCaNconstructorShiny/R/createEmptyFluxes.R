#' createEmptyFluxes
#' creates an empty fluxes data frame
#' @return a data frame
#' @export
#' @example 
#' createEmptyFluxes()
#'
createEmptyFluxes <- function(){
  tibble(id = character(),
             Flux = character(),
             to = character(),
             from = character(),
             From = character(),
             To = character(),
             Trophic = integer())
}
