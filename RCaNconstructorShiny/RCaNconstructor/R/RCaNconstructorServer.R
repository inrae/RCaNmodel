#' RCaNconstructorServer
#' build the server of the Construtor
#' @param input the shiny input
#' @param output the shiny output
#' @param session the shiny server
#' @return a module server
#' @importFrom shiny reactiveValues
#' @importFrom magrittr %>%
#' @importFrom shiny isolate
#' @export
#'
RCaNconstructorServer <- function(input, output, session){
  network <- reactiveValues(
    file = NULL,
    components = data.frame(id = character(),
                            component = character(),
                            In = integer(),
                            AssimilationE = numeric(),
                            Digestibility = numeric(),
                            OtherLosses = numeric(),
                            Inertia = numeric(),
                            Satiation = numeric(),
                            RefugeBiomass = numeric()),
    fluxes = data.frame(id = character(),
                        flux = character(),
                        to = character(),
                        from = character(),
                        From = character(),
                        To = character(),
                        Trophic = integer()),
    dictionary = character()
  )


  updateNetwork <- function(newnetwork){
    if (nrow(isolate(network$components)) > 0 |
        nrow((newnetwork$components)) > 0) {
      if (!identical(isolate(network$components),
                     (newnetwork$components))){
        network$components <<- (newnetwork$components)
        network$dictionary <<- c(isolate(network$fluxes$flux),
                                 isolate(network$components$component))
        names(network$dictionary) <<- c(isolate(network$fluxes$id),
                                        isolate(network$components$id))
      }
    }
    if (nrow(isolate(network$fluxes)) > 0 |
        nrow((newnetwork$fluxes)) > 0) {
      if (!identical(isolate(network$fluxes),
                     (newnetwork$fluxes))){
        network$fluxes <<- (newnetwork$fluxes)
        network$dictionary <<- c(isolate(network$fluxes$flux),
                                 isolate(network$components$component))
        names(network$dictionary) <<- c(isolate(network$fluxes$id),
                                        isolate(network$components$id))
      }
    }
  }

  newnetworkviz <- visNetworkServer("visnetwork", network)
  observe({
    newnetworkviz$components
    newnetworkviz$fluxes
    updateNetwork(isolate(newnetworkviz))

  })




  newnetwork_component <- tableEditorServer("components", network, "components")
  observe({
    newnetwork_component$components
    newnetwork_component$fluxes
    updateNetwork(isolate(newnetwork_component))

  })

  newnetwork_fluxes <- tableEditorServer("fluxes", network, "fluxes")
  observe({
    newnetwork_fluxes$components
    newnetwork_fluxes$fluxes
    updateNetwork(isolate(newnetwork_fluxes))

  })





}
