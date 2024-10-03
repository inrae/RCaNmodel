#' visNetworkServer
#'
#' @param id the id of the visnetworkUI
#' @param network the data of the graph
#' @param tab selected tab
#'
#' @return updated newtork
#'
#' @importFrom visNetwork renderVisNetwork visNetwork visGroups visGetPositions
#' @importFrom visNetwork visOptions visEdges visNetworkProxy visUpdateEdges
#' @importFrom visNetwork visNodes visClusteringByGroup visUpdateNodes
#' @importFrom visNetwork visEvents visIgraphLayout visRemoveNodes
#' @importFrom dplyr bind_rows pull filter select anti_join
#' @importFrom magrittr %>%
#' @importFrom spsComps shinyCatch
#' @importFrom shiny req
#' @importFrom shiny isolate
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

visNetworkServer <- function(id, network, tab){
  shiny::moduleServer(id,
                      function(input, output, session) {
                        newnetwork <- createEmptyNetwork()
                        tmpnetwork <- list()


                        currenttab <- "Visualise Trophic Network"
                        observe({
                          network$components
                          network$fluxes
                          network$model
                          network$dictionary
                          req(isolate(tab$panel) == currenttab)

                          updateVis <- TRUE
                          if (identical(isolate(network$components),
                                        tmpnetwork$components) &
                              identical(isolate(network$fluxes),
                                        tmpnetwork$fluxes))
                            updateVis <- FALSE
                          for (v in names(isolate(network))){
                            if(!identical(isolate(network[[v]]),
                                          tmpnetwork[[v]]))
                              tmpnetwork[[v]] <<- isolate(network[[v]])
                          }

                          if (updateVis){
                            nodes <- createNodes()
                            edges <- createEdges()

                            #output$networkviz_proxy <- renderVisNetwork(drawNet())
                            visNetworkProxy(session$ns("networkviz_proxy")) %>%
                              visNetwork::visUpdateNodes(nodes = nodes) %>%
                              visNetwork::visUpdateEdges(edges = edges) %>%
                              visGetPositions()
                          } else{
                            visNetworkProxy(session$ns("networkviz_proxy")) %>%
                              visEvents(dragEnd = paste0("function(nodes) {
                                  Shiny.onInputChange('",
                                                         session$ns('dragging_node_id'),
                                                         "', nodes);
                              ;}"))
                          }


                        }
                      )





                        createNodes <- function(){
                          nodes <- tmpnetwork$components %>%
                            dplyr::mutate(groups = as.character(.data[["Inside"]])) %>%
                            dplyr::mutate(color = ifelse(
                              .data[["Inside"]],
                              "orange",
                              "grey"))
                          if (isolate(input$shownodes)){
                            nodes <- nodes %>%
                              mutate(label = .data[["Component"]])
                          } else{
                            nodes <- nodes %>%
                              mutate(label = "")
                          }
                          if (any(is.na(nodes$x)) | any(is.na(nodes$y)) | !input$fixposition)
                            nodes <- nodes %>%
                              dplyr::select(!all_of(c("x", "y")))
                          return (nodes)
                        }

                        createEdges <- function(){
                          edges <- tmpnetwork$fluxes %>%
                            dplyr::mutate(
                              color = ifelse(
                                .data[["Trophic"]],
                                "red",
                                "blue"))
                          if (isolate(input$showedges)){
                            edges <- edges %>%
                              mutate(label = .data[["Flux"]])
                          } else {
                            edges <- edges %>%
                              mutate(label = " ")
                          }
                          return(edges)
                        }



                        output$networkviz_proxy <- renderVisNetwork({
                          visNetwork(nodes = createEmptyComponents(),
                                     edges = createEmptyFluxes()) %>%
                            visNodes() %>%
                            visIgraphLayout() %>%
                            visGroups(groupname = "1", shape = "triangle") %>%
                            visGroups(groupname = "0", shape = "circle") %>%
                            visClusteringByGroup(groups= c("1","0")) %>%
                            visEdges(arrows = "to") %>%
                            visOptions(manipulation = list(enabled = TRUE,
                                                           editEdgeCols = c("label", "Trophic"),
                                                           editNodeCols = c("label","Inside"),
                                                           addNodeCols = c("label", "Inside")))
                        })

                        visNetworkProxy(session$ns("networkviz_proxy")) %>%
                          visRemoveNodes(id = "mock") %>%
                          visGetPositions() %>%

                          visEvents(dragEnd = paste0("function(nodes) {
                                  Shiny.onInputChange('",
                                                     session$ns('dragging_node_id'),
                                                     "', nodes);
                              ;}"))








                        observe({
                          input$dragging_node_id
                          visNetworkProxy(session$ns("networkviz_proxy")) %>%
                            visNetwork::visGetPositions(isolate(input$dragging_node_id$nodes[[1]]))
                        })

                        observe({
                          req(input$networkviz_proxy_positions)
                          newnodes <- dplyr::bind_cols(id = isolate(names(input$networkviz_proxy_positions)),
                                                       do.call(bind_rows,
                                                               lapply(
                                                                 isolate(input$networkviz_proxy_positions), function(n){
                                                                   data.frame(
                                                                     x = n$x,
                                                                     y = n$y)
                                                                 })))
                          oldnodes <- tmpnetwork$components %>%
                                                filter(.data[["id"]] %in% newnodes$id)

                          updatednodes <- anti_join(newnodes, oldnodes, by = c("id", "x", "y"))
                          if (nrow(updatednodes) > 0)
                            tmpnetwork$components[match(updatednodes$id,
                                                        tmpnetwork$components$id),
                                                  c("x", "y")] <<-
                            updatednodes[, c("x", "y")]
                        })


                        observe({
                          req(!is.null(input$networkviz_proxy_graphChange$cmd))

                          shinyCatch({

                            cmd <- input$networkviz_proxy_graphChange$cmd
                            if (cmd %in% c("addNode", "editNode")){
                              newnode <- isolate(input$networkviz_proxy_graphChange)
                              newnode <- newnode[-which(names(newnode) == "cmd")]
                              if (!"Inside" %in% names(newnode))
                                newnode$Inside <- 1
                              if (!newnode$Inside %in% c(0,1))
                                stop(paste("Inside should be 0 or 1 for", newnode$label))
                              newnode <- data.frame(id = newnode$id,
                                                    Component = newnode$label,
                                                    Inside = as.integer(newnode$Inside))
                              if (cmd == "addNode"){
                                if (newnode$Component %in% c(tmpnetwork$components$Component,
                                                             tmpnetwork$components$Flux)){
                                  visNetworkProxy(session$ns("networkviz_proxy")) %>%
                                    visNetwork::visRemoveNodes(newnode$id)
                                  stop("this name is alreay used")
                                }
                                tmpnetwork$components <<- bind_rows(tmpnetwork$components,
                                                                    newnode)
                                visNetworkProxy(session$ns("networkviz_proxy")) %>%
                                  visGetPositions(node = newnode$id)
                              } else {
                                if (newnode$Component %in% tmpnetwork$fluxes$flux |
                                    newnode$Component %in% (tmpnetwork$components %>%
                                                            filter(.data[["id"]] != newnode$id) %>%
                                                            dplyr::select(all_of("Component")) %>%
                                                            dplyr::pull())) {
                                  visNetworkProxy(session$ns("networkviz_proxy")) %>%
                                    visNetwork::visUpdateNodes(tmpnetwork$components %>%
                                                                         filter(.data[["id"]] == newnode$id) %>%
                                                                         mutate(label = .data[["Component"]]))
                                  stop("this name is alreay used")
                                }
                                tmpnetwork$components <<- bind_rows(tmpnetwork$components %>%
                                                                      dplyr::filter(.data[["id"]] != newnode$id),
                                                                    newnode)
                              }
                              visNetworkProxy(session$ns("networkviz_proxy")) %>%
                                visNetwork::visUpdateNodes(nodes = createNodes())



                            }
                            if (cmd %in% c("addEdge", "editEdge")){
                              newedge <- isolate(input$networkviz_proxy_graphChange)
                              newedge <- newedge[-which(names(newedge) == "cmd")]



                              if (is.null(newedge$Trophic)){
                                newedge$Trophic <- 1
                              } else {
                                newedge$Trophic <- as.integer(newedge$Trophic)
                              }
                              if (!newedge$Trophic %in% c(0, 1))
                                stop(paste0("Trophic should be 0 or 1 for", newedge$label))
                              if (!"label" %in% names(newedge))
                                newedge$label <- paste(isolate(tmpnetwork$components$Component[tmpnetwork$components$id == newedge$from]),
                                                       isolate(tmpnetwork$components$Component[tmpnetwork$components$id == newedge$to]),
                                                       sep = "_")

                              if (cmd == "addEdge"){
                                if (newedge$label %in% c(tmpnetwork$components$Component, tmpnetwork$components$Flux)){
                                  visNetworkProxy(session$ns("networkviz_proxy")) %>%
                                    visNetwork::visRemoveEdges(newedge$id)
                                  stop("this name is alreay used")
                                }
                                newedge <- data.frame(id = newedge$id,
                                                      Flux = newedge$label,
                                                      from = newedge$from,
                                                      to = newedge$to,
                                                      From = tmpnetwork$components$Component[tmpnetwork$components$id == newedge$from],
                                                      To = tmpnetwork$components$Component[tmpnetwork$components$id == newedge$to],
                                                      Trophic = ifelse(is.null(newedge$Trophic),
                                                                       1,
                                                                       as.integer(newedge$Trophic)))
                                tmpnetwork$fluxes <<- bind_rows(tmpnetwork$fluxes,
                                                                newedge)
                              } else {
                                newedge <- as.data.frame(newedge)
                                if (newedge$label %in% tmpnetwork$components$Component |
                                    newedge$label %in% (tmpnetwork$fluxes %>%
                                                        filter(.data[["id"]] != newedge$id) %>%
                                                        dplyr::select(all_of("Flux")) %>%
                                                        dplyr::pull())) {
                                  visNetworkProxy(session$ns("networkviz_proxy")) %>%
                                    visNetwork::visUpdateEdges(tmpnetwork$fluxes %>%
                                                                         filter(.data[["id"]] == newedge$id) %>%
                                                                         mutate(label = .data[["Flux"]]))
                                  stop("this name is alreay used")
                                }


                                oldedges <- tmpnetwork$fluxes
                                for (i in seq(nrow(newedge))){
                                  for (j in names(newedge)[-1]){
                                    oldedges[oldedges$id == newedge$id[i],
                                             ifelse(j == "label", "Flux", j)] <-
                                      newedge[i, j]
                                  }
                                }
                                tmpnetwork$fluxes <<- oldedges
                              }

                              visNetworkProxy(session$ns("networkviz_proxy")) %>%
                                visNetwork::visUpdateEdges(edges = createEdges())

                            }

                            if (cmd %in% c("deleteElements")){
                              nodesdeleted <- isolate(input$networkviz_proxy_graphChange$nodes[[1]])
                              edgesdeleted <- isolate(input$networkviz_proxy_graphChange$edges[[1]])
                              if (length(nodesdeleted) > 1){
                                tmpnetwork$components <<- tmpnetwork$components %>%
                                  filter(!.data[["id"]] %in% nodesdeleted)
                              }
                              if (length(edgesdeleted) > 1){
                                tmpnetwork$fluxes <<- tmpnetwork$fluxes %>%
                                  filter(!.data[["id"]] %in% edgesdeleted)
                              }
                            }
                          })
                        })


                        observeEvent(input$shownodes, {
                          req(nrow(tmpnetwork$components) > 0)
                          nodes <- createNodes()
                          visNetworkProxy(session$ns("networkviz_proxy")) %>%
                            visNetwork::visUpdateNodes(nodes = nodes)
                          #output$networkviz_proxy <- renderVisNetwork(drawNet())
                        })



                        observeEvent(input$showedges, {
                          req(nrow(tmpnetwork$components) > 0)
                          req(nrow(tmpnetwork$fluxes) > 0)
                          edges <- createEdges()
                          #output$networkviz_proxy <- renderVisNetwork(drawNet())
                          visNetworkProxy(session$ns("networkviz_proxy")) %>%
                            visNetwork::visUpdateEdges(edges = edges)
                        })


                        observe({
                          ntab <- tab$panel
                          if (currenttab == "Visualise Trophic Network" & ntab != "Visualise Trophic Network"){
                            for (v in names(tmpnetwork)){
                              if (!identical(tmpnetwork[[v]],
                                             isolate(newnetwork[[v]])))
                                newnetwork[[v]] <- tmpnetwork[[v]]
                            }
                          }
                          currenttab <<- ntab
                        })


                        return(newnetwork)

}
  )
  }
