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
    if (input$mainpanel == "Edit TrophicNetwork"){
      tab$panel <<- input$editpanel
    } else if (input$mainpanel == "Constraints"){
      tab$panel <<- input$editconstraits
    } else {
      tab$panel <<- ""
    }
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

      network$dictionary <<- generateDictionary(isolate(network$components),
                                                isolate(network$fluxes),
                                                isolate(network$observations))
    }
  }

  newnetwork_file <- fileInteractionServer("files", network)
  observe({
    newnetwork_file$components
    newnetwork_file$fluxes
    newnetwork_file$model
    newnetwork_file$observations
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
    newnetworkviz$observations
    updateNetwork(isolate(newnetworkviz))

  })




  newnetwork_component <- tableEditorServer("components", network, "components", tab)
  observe({
    newnetwork_component$components
    newnetwork_component$fluxes
    newnetwork_component$observations
    updateNetwork(isolate(newnetwork_component))

  })

  newnetwork_fluxes <- tableEditorServer("fluxes", network, "fluxes", tab)
  observe({
    newnetwork_fluxes$components
    newnetwork_fluxes$fluxes
    newnetwork_fluxes$observations
    updateNetwork(isolate(newnetwork_fluxes))

  })


  newnetwork_observations <- tableObsServer("obs", network, tab)
  observe({
    newnetwork_observations$components
    newnetwork_observations$fluxes
    newnetwork_observations$observations
    updateNetwork(isolate(newnetwork_observations))

  })

  newnetwork_constraints <- tabConstrServer("tabconstraints", network, tab)
  observe({
    newnetwork_constraints$components
    newnetwork_constraints$fluxes
    newnetwork_constraints$observations
    newnetwork_constraints$constraints
    updateNetwork(isolate(newnetwork_constraints))

  })


  newnetwork_editedconstraints <- constrEditorServer("constreditor", network, tab)
  observe({
    newnetwork_editedconstraints$components
    newnetwork_editedconstraints$fluxes
    newnetwork_editedconstraints$observations
    newnetwork_editedconstraints$constraints
    updateNetwork(isolate(newnetwork_editedconstraints))

  })















}
