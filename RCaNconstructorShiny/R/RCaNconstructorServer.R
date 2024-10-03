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
  network <- createEmptyNetwork()

  tab <- reactiveValues(panel = "")
  observe({
    tab$panel <<- input$editpanel
  }
  )

  updateNetwork <- function(newnetwork){
    varnames <- setdiff(isolate(names(newnetwork)), "dictionary")
    if (any(sapply(varnames, function(v) isolate(!identical(network[[v]],
                                                            newnetwork[[v]]))
    ))){
      for (v in varnames){
        network[[v]] <<- isolate(newnetwork[[v]])
      }
      network$dictionary <<- c(isolate(network$fluxes$Flux),
                               isolate(network$components$Component))
      names(network$dictionary) <<- c(isolate(network$fluxes$id),
                                      isolate(network$components$id))
    }
  }

  newnetwork_file <- fileInteractionServer("files", network)
  observe({
    newnetwork_file$components
    newnetwork_file$fluxes
    newnetwork_file$model
    updateNetwork(isolate(newnetwork_file))

  })
  observe({
    newnetwork_file$model
    req(!is.null(newnetwork_file$model))
    if (newnetwork_file$model != isolate(network$model))
      network$model <- newnetwork_file$model
  })


  newnetworkviz <- visNetworkServer("visnetwork", network, tab)
  observe({
    newnetworkviz$components
    newnetworkviz$fluxes
    updateNetwork(isolate(newnetworkviz))

  })




  newnetwork_component <- tableEditorServer("components", network, "components", tab)
  observe({
    newnetwork_component$components
    newnetwork_component$fluxes
    updateNetwork(isolate(newnetwork_component))

  })

  newnetwork_fluxes <- tableEditorServer("fluxes", network, "fluxes", tab)
  observe({
    newnetwork_fluxes$components
    newnetwork_fluxes$fluxes
    updateNetwork(isolate(newnetwork_fluxes))

  })












}
