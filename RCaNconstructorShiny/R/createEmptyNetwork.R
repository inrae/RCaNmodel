#' createEmptyNetwork
#'
#' @return a reactive network with empty components
#' @importFrom shiny reactiveValues
#' @export
#'

createEmptyNetwork <- function(){
  reactiveValues(
    model = NULL,
    components = createEmptyComponents(),
    fluxes = createEmptyFluxes(),
    constraints = createEmptyConstraints(),
    dictionary = character(),
    observations = data.frame(Year = numeric()),
    aliases = createEmptyAliases(),
    envir = new.env()
  )
}
