#' RCaNconstructorServer
#' build the server of the Construtor
#' @param input the shiny input
#' @param output the shiny output
#' @param session the shiny server
#' @return a module server
#' @importFrom shiny reactiveValues
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @importFrom shiny isolate exportTestValues
#' @importFrom dplyr filter left_join inner_join anti_join bind_rows
#' @export
#'
RCaNconstructorServer <- function(input, output, session){
  network <- createEmptyNetwork()
  
  timeline <- reactiveValues(timeline = createEmptyTimeLine())
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
  
  updateObsTimeLine <- function(newnetwork){
    isolate({
      newtimeline <- tibble(Task = character(0))
      #we check only series that already exist
      commonseries <- setdiff(intersect(names(newnetwork$observations),
                                        names(network$observations)),
                              "Year")
      newYear <- newnetwork$observations %>%
        filter(!.data[["Year"]] %in% network$observations$Year)
      if (nrow(newYear) > 0){
        for (i in seq_len(nrow(newYear))){
          newtimeline <- newtimeline %>%
            bind_rows(writeTimeLine("observations",
                                    newYear[i, , drop = FALSE],
                                    NULL))
        }
      }
      
      oldYear <- network$observations %>%
        filter(!.data[["Year"]] %in% newnetwork$observations$Year)
      if (nrow(oldYear) > 0){
        for (i in seq_len(nrow(oldYear))){
          newtimeline <- newtimeline %>%
            bind_rows(writeTimeLine("observations",
                                    NULL,
                                    oldYear[i, , drop = FALSE]))
        }
      }
      
      newcommon <- tibble(newnetwork$observations) %>%
        dplyr::select(all_of(c("Year", commonseries))) %>%
        filter(.data[["Year"]] %in% network$observations$Year) %>%
        dplyr::arrange(.data[["Year"]])
      
      oldcommon <- tibble(network$observations) %>%
        dplyr::select(all_of(c("Year", commonseries))) %>%
        filter(.data[["Year"]] %in% newnetwork$observations$Year) %>%
        dplyr::arrange(.data[["Year"]])
      
      if(!ident_tol(oldcommon, newcommon)){
        for (i in seq_len(nrow(newcommon))){
          if (!ident_tol(newcommon[i, , drop = FALSE], 
                         oldcommon[i, , drop = FALSE])){
            newtimeline <- newtimeline %>%
              bind_rows(writeTimeLine("observations",
                                      newcommon[i, , drop = FALSE],
                                      oldcommon[i, , drop = FALSE]))
          }
        }
      }
      
      if (nrow(newtimeline) > 0){
        timeline$timeline <- timeline$timeline %>%
          bind_rows(tibble(Date = format(Sys.time()),
                           Task = paste0(newtimeline$Task, 
                                         collapse ="; "),
                           Annotation = ""))
      }
      
    })
  }
  
  updateTimeLine <- function(newnetwork){
    isolate({
      newtimeline <- tibble(Task =  character(0))
      varnames <- setdiff(names(newnetwork), 
                          c("dictionary", "envir", "timeline"))
      if (any(sapply(varnames, 
                     function(v) !ident_tol(network[[v]],
                                            newnetwork[[v]])
      ))){
        for (v in varnames){
          if (!ident_tol(newnetwork[[v]], network[[v]])){
            if (v %in% c("components", "aliases", "constraints",
                         "metaobs")){
              new <- isolate(newnetwork[[v]] %>%
                               filter(!.data[["id"]] %in% network[[v]]$id))
              if (nrow(new) > 0){
                for (i in seq_len(nrow(new))){
                  newtimeline <- newtimeline %>%
                    bind_rows(writeTimeLine(v, new[i, , drop = FALSE], NULL))
                }
              }
              
              old <- isolate(network[[v]] %>%
                               filter(!.data[["id"]] %in% newnetwork[[v]]$id))
              if (nrow(old) > 0){
                for (i in seq_len(nrow(old))){
                  newtimeline <- newtimeline %>%
                    bind_rows(writeTimeLine(v, NULL, old[i, , drop = FALSE]))
                }
              }
              notupdated <- inner_join(network[[v]],
                                       newnetwork[[v]])
              newupdated <- newnetwork[[v]] %>%
                filter(.data[["id"]] %in% network[[v]]$id &
                         !.data[["id"]] %in% notupdated$id)
              oldupdated <- network[[v]] %>%
                filter(.data[["id"]] %in% newnetwork[[v]]$id &
                         !.data[["id"]] %in% notupdated$id)
              
              for (i in seq_len(nrow(newupdated))){
                newtimeline <- newtimeline %>%
                  bind_rows(writeTimeLine(v,
                                          newupdated[i, , drop = FALSE],
                                          oldupdated[oldupdated$id == newupdated$id[i], , drop = FALSE]))
              }
            }
          }
        }
      }
      if (nrow(newtimeline) > 0){
        timeline$timeline <- timeline$timeline %>%
          bind_rows(tibble(Date = format(Sys.time()),
                           Task = paste0(newtimeline$Task, 
                                         collapse ="; "),
                           Annotation = ""))
      }
    })
  }
  
  
  updateNetwork <- function(newnetwork){
    isolate({
      varnames <- setdiff(names(newnetwork), 
                          c("dictionary", "envir", "timeline"))
      if (any(sapply(varnames, 
                     function(v) !ident_tol(network[[v]],
                                            newnetwork[[v]])
      ))){
        for (v in varnames){
          if (!ident_tol(newnetwork[[v]], network[[v]])){
            network[[v]] <<- newnetwork[[v]]
          }
        }
        
        oldic <- sort(network$dictionary)
        
        network$dictionary <<- generateDictionary(network$components,
                                                  network$fluxes,
                                                  network$observations,
                                                  network$metaobs,
                                                  network$constraints,
                                                  network$aliases)
        if (!ident_tol(oldic, sort(network$dictionary)))
          network$envir <<- generateSymbolicEnvir(network)
        
      }})
  }
  
  newnetwork_file <- fileInteractionServer("files", network, timeline)
  observe({
    newnetwork_file$timeline
    newnetwork_file$components
    newnetwork_file$aliases
    newnetwork_file$fluxes
    newnetwork_file$model
    newnetwork_file$observations
    newnetwork_file$metaobs
    isolate({
      tline <- newnetwork_file$timeline
      if (!ident_tol(tibble(tline), tibble(timeline$timeline)))
        timeline$timeline <<- tline
      updateNetwork(isolate(newnetwork_file))
      
    })
    
    
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
    updateTimeLine(isolate(newnetworkviz))
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
    updateTimeLine(isolate(newnetwork_component))
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
    updateTimeLine(isolate(newnetwork_fluxes))
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
    updateObsTimeLine(isolate(newnetwork_observations))
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
    updateTimeLine(isolate(newnetwork_constraints))
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
    updateTimeLine(isolate(newnetwork_editedconstraints))
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
    updateTimeLine(isolate(newnetwork_editedmetaobs))
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
    updateTimeLine(isolate(newnetwork_aliases))
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
    updateTimeLine(isolate(newnetwork_editedaliases))
    updateNetwork(isolate(newnetwork_editedaliases))
    
  })
  
  
  exportTestValues(
    components = network$components[, c("Component", "Inside", "Digestibility",
                                        "Satiation", "AssimilationE",
                                        "RefugeBiomass", "OtherLosses", 
                                        "Inertia")],
    fluxes = network$fluxes[, c("Flux", "From", "To", "Trophic")],
    constraints = network$constraints[, c("Id", "Constraint")],
    aliases = network$aliases[, c("Alias", "Formula")],
    metaobs = network$metaobs[, c("Observation", "Comment")],
    observations = network$observations,
    timeline = timeline$timeline[, c("Task", "Annotation")]
  )
  
  newtimeline <- timeLineServer("tlui", timeline)
  observe({
    newtimeline
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}
