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
      tab$panel <<- input$editconstraints
    } else if (input$mainpanel == "Observations"){
      tab$panel <<- input$editobservations
    }else {
      tab$panel <<- ""
    }
  }
  )

  updateNetwork <- function(newnetwork){
    isolate({
    varnames <- setdiff(isolate(names(newnetwork)), c("dictionary", "envir"))
    if (any(sapply(varnames, function(v) isolate(!identical(network[[v]],
                                                            newnetwork[[v]]))
    ))){
      for (v in varnames){
        network[[v]] <<- isolate(newnetwork[[v]])
      }
      
      oldic <- sort(isolate(network$dictionary))

      network$dictionary <<- generateDictionary(isolate(network$components),
                                                isolate(network$fluxes),
                                                isolate(network$observations),
                                                isolate(network$metaobs),
                                                isolate(network$aliases))
      if (!identical(oldic, isolate(sort(network$dictionary))))
        network$envir <<- generateSymbolicEnvir(isolate(network))

    }})
  }

  newnetwork_file <- fileInteractionServer("files", network)
  observe({
    newnetwork_file$components
    newnetwork_file$aliases
    newnetwork_file$fluxes
    newnetwork_file$model
    newnetwork_file$observations
    newnetwork_file$metaobs
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
    newnetworkviz$aliases
    newnetworkviz$constraints
    newnetworkviz$fluxes
    newnetworkviz$observations
    newnetworkviz$metaobs
    updateNetwork(isolate(newnetworkviz))

  })




  newnetwork_component <- tableEditorServer("components", network, "components", tab)
  observe({
    newnetwork_component$components
    newnetwork_component$constraints
    newnetwork_component$aliases
    newnetwork_component$fluxes
    newnetwork_component$observations
    newnetwork_component$metaobs
    updateNetwork(isolate(newnetwork_component))

  })

  newnetwork_fluxes <- tableEditorServer("fluxes", network, "fluxes", tab)
  observe({
    newnetwork_fluxes$components
    newnetwork_fluxes$constraints
    newnetwork_fluxes$aliases
    newnetwork_fluxes$fluxes
    newnetwork_fluxes$observations
    newnetwork_fluxes$metaobs
    updateNetwork(isolate(newnetwork_fluxes))

  })


  newnetwork_observations <- tableObsServer("obs", network, tab)
  observe({
    newnetwork_observations$components
    newnetwork_observations$fluxes
    newnetwork_observations$constraints
    newnetwork_observations$aliases
    newnetwork_observations$observations
    newnetwork_observations$metaobs
    updateNetwork(isolate(newnetwork_observations))

  })

  newnetwork_constraints <- tabConstrServer("tabconstraints",
                                            network,
                                            "constraints",
                                            tab)
  observe({
    newnetwork_constraints$components
    newnetwork_constraints$fluxes
    newnetwork_constraints$aliases
    newnetwork_constraints$observations
    newnetwork_constraints$constraints
    newnetwork_constraints$metaobs
    updateNetwork(isolate(newnetwork_constraints))

  })


  newnetwork_editedconstraints <- constrEditorServer("constreditor",
                                                     network,
                                                     "constraints",
                                                     tab)
  observe({
    newnetwork_editedconstraints$components
    newnetwork_editedconstraints$fluxes
    newnetwork_editedconstraints$aliases
    newnetwork_editedconstraints$observations
    newnetwork_editedconstraints$constraints
    newnetwork_editedconstraints$metaobs
    updateNetwork(isolate(newnetwork_editedconstraints))

  })


  newnetwork_editedmetaobs <- tabObsMetaServer("tabmetaobs",
                                               network,
                                               tab)
  observe({
    newnetwork_editedmetaobs$components
    newnetwork_editedmetaobs$fluxes
    newnetwork_editedmetaobs$aliases
    newnetwork_editedmetaobs$observations
    newnetwork_editedmetaobs$constraints
    newnetwork_editedmetaobs$metaobs
    updateNetwork(isolate(newnetwork_editedmetaobs))

  })


  newnetwork_aliases <- tabConstrServer("tabaliases",
                                            network,
                                            "aliases",
                                            tab)
  observe({
    newnetwork_aliases$components
    newnetwork_aliases$fluxes
    newnetwork_aliases$aliases
    newnetwork_aliases$observations
    newnetwork_aliases$constraints
    newnetwork_aliases$metaobs
    updateNetwork(isolate(newnetwork_aliases))

  })



  newnetwork_editedaliases <- constrEditorServer("editaliases",
                                                     network,
                                                     "aliases",
                                                     tab)
  observe({
    newnetwork_editedaliases$components
    newnetwork_editedaliases$fluxes
    newnetwork_editedaliases$aliases
    newnetwork_editedaliases$observations
    newnetwork_editedaliases$constraints
    newnetwork_editedaliases$metaobs
    updateNetwork(isolate(newnetwork_editedaliases))

  })


















}
