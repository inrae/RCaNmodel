#' visNetworkServer
#'
#' @param id the id of the visnetworkUI
#' @param network the data of the graph
#'
#' @return updated newtork
#'
#' @importFrom visNetwork renderVisNetwork visNetwork visGroups
#' @importFrom visNetwork visOptions visEdges visNetworkProxy visUpdateEdges
#' @importFrom visNetwork visNodes visClusteringByGroup visUpdateNodes
#' @importFrom dplyr bind_rows
#' @importFrom magrittr %>%
#' @importFrom spsComps shinyCatch
#' @importFrom shiny req
#' @importFrom shiny isolate
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

visNetworkServer <- function(id, network){
  shiny::moduleServer(id,
                      function(input, output, session) {
                        newnetwork <- createEmptyNetwork()
                        observe({
                          network$components
                          network$fluxes
                          network$model
                          network$dictionary
                          updateVis <- TRUE
                          if (identical(isolate(network$components),
                                        isolate(newnetwork$components)) &
                              identical(isolate(network$fluxes),
                                        isolate(newnetwork$fluxes)))
                            upateVis <- FALSE
                          for (v in names(isolate(network))){
                            if(!identical(isolate(network[[v]]),
                                          isolate(newnetwork[[v]])))
                              newnetwork[[v]] <<- isolate(network[[v]])
                          }

                          if (updateVis){
                            nodes <- createNodes()
                            edges <- createEdges()
                            #output$networkviz_proxy <- renderVisNetwork(drawNet())
                            visNetworkProxy(session$ns("networkviz_proxy")) %>%
                              visNetwork::visUpdateEdges(edges = edges) %>%
                              visNetwork::visUpdateNodes(nodes = nodes)
                          }
                        })





                        createNodes <- function(){
                          nodes <- isolate(newnetwork$components) %>%
                            dplyr::mutate(groups = as.character(.data[["Inside"]])) %>%
                            dplyr::mutate(color = ifelse(
                              .data[["Inside"]],
                              "orange",
                              "grey"))
                          if (isolate(input$shownodes)){
                            nodes <- nodes %>%
                              mutate(label = .data[["Component"]])
                          }
                          return (nodes)
                        }

                        createEdges <- function(){
                          edges <- isolate(newnetwork$fluxes) %>%
                            dplyr::mutate(
                              color = ifelse(
                                .data[["Trophic"]],
                                "red",
                                "blue"))
                          if (isolate(input$showedges)){
                            edges <- edges %>%
                              mutate(label = .data[["Flux"]])
                          }
                          return(edges)
                        }

                        drawNet <- function(){

                          nodes <- createNodes()
                          edges <- createEdges()

                          visNetwork(nodes =  nodes,
                                     edges = edges) %>%
                            visNodes() %>%
                            visGroups(groupname = "1", shape = "triangle") %>%
                            visGroups(groupname = "0", shape = "circle") %>%
                            visClusteringByGroup(groups= c("1","0")) %>%
                            visEdges(arrows = "to") %>%
                            visOptions(manipulation = list(enabled = TRUE,
                                                           editEdgeCols = c("label", "Trophic"),
                                                           editNodeCols = c("label","Inside"),
                                                           addNodeCols = c("label", "Inside")))

                        }


                        output$networkviz_proxy <- renderVisNetwork({
                          drawNet()
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
                                newnetwork$components <<- bind_rows(isolate(newnetwork$components),
                                                                    newnode)
                              } else {
                                newnetwork$components <<- bind_rows(isolate(newnetwork$components) %>%
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
                                newedge$label <- paste(isolate(network$dictionary[newedge$from]),
                                                       isolate(network$dictionary[newedge$to]),
                                                       sep = "_")

                              if (cmd == "addEdge"){
                                newedge <- data.frame(id = newedge$id,
                                                      Flux = newedge$label,
                                                      from = newedge$from,
                                                      to = newedge$to,
                                                      From = isolate(network$dictionary[newedge$from]),
                                                      To = isolate(network$dictionary[newedge$to]),
                                                      Trophic = ifelse(is.null(newedge$Trophic),
                                                                       1,
                                                                       as.integer(newedge$Trophic)))
                                newnetwork$fluxes <<- bind_rows(isolate(newnetwork$fluxes),
                                                                newedge)
                              } else {
                                newedge <- as.data.frame(newedge)
                                oldedges <- isolate(newnetwork$fluxes)
                                for (i in seq(nrow(newedge))){
                                  for (j in names(newedge)[-1]){
                                    oldedges[oldedges$id == newedge$id[i], j] <-
                                      newedge[i, j]
                                  }
                                }
                                newnetwork$fluxes <<- oldedges
                              }

                              visNetworkProxy(session$ns("networkviz_proxy")) %>%
                                visNetwork::visUpdateEdges(edges = createEdges())

                            }

                            if (cmd %in% c("deleteElements")){
                              nodesdeleted <- isolate(input$networkviz_proxy_graphChange$nodes[[1]])
                              edgesdeleted <- isolate(input$networkviz_proxy_graphChange$edges[[1]])
                              if (length(nodesdeleted) > 1){
                                newnetwork$components <<- isolate(newnetwork$components) %>%
                                  filter(!.data[["id"]] %in% nodesdeleted)
                              }
                              if (length(edgesdeleted) > 1){
                                newnetwork$fluxes <<- isolate(newnetwork$fluxes) %>%
                                  filter(!.data[["id"]] %in% edgesdeleted)
                              }
                            }
                          })
                        })


                        observeEvent(input$shownodes, {
                          req(nrow(isolate(newnetwork$components)) > 0)
                          nodes <- createNodes()
                          visNetworkProxy(session$ns("networkviz_proxy")) %>%
                            visNetwork::visUpdateNodes(nodes = nodes)
                          #output$networkviz_proxy <- renderVisNetwork(drawNet())
                        })



                        observeEvent(input$showedges, {
                          req(nrow(isolate(newnetwork$components)) > 0)
                          edges <- createEdges()
                          #output$networkviz_proxy <- renderVisNetwork(drawNet())
                          visNetworkProxy(session$ns("networkviz_proxy")) %>%
                            visNetwork::visUpdateEdges(edges = edges)
                        })


                        return(newnetwork)

                      }
  )
}
