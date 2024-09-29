#' visNetworkServer
#'
#' @param id the id of the visnetworkUI
#' @param datanet the data of the graph
#'
#' @return updated newtork
#'
#' @importFrom visNetwork renderVisNetwork visNetwork visGroups
#' @importFrom visNetwork visOptions visEdges visNetworkProxy
#' @importFrom visNetwork visNodes visClusteringByGroup
#' @importFrom dplyr bind_rows
#' @importFrom magrittr %>%
#' @importFrom spsComps shinyCatch
#' @importFrom shiny req
#' @importFrom shiny isolate
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

visNetworkServer <- function(id, datanet){
  shiny::moduleServer(id,
                      function(input, output, session) {
                        currentnet <- createEmptyNetwork()
                        observe({
                          datanet$components
                          datanet$fluxes
                          datanet$model
                          datanet$dictionary
                          for (n in names(currentnet))
                            currentnet[[n]] <- isolate(datanet[[n]])
                        })

                        drawNet <- function(){

                          visNetwork(nodes = currentnet$components %>%
                                       dplyr::mutate(label = .data[["Component"]],
                                                     groups = as.character(.data[["Inside"]])) %>%
                                       dplyr::mutate(color = ifelse(
                                         currentnet$components$Inside,
                                         "orange",
                                         "grey")),
                                     edges = currentnet$fluxes %>%
                                       dplyr::mutate(
                                         label = .data[["Flux"]],
                                         color = ifelse(
                                           currentnet$fluxes$Trophic,
                                           "red",
                                           "blue"))) %>%
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


                          newnodes <- input$networkviz_proxy_nodes
                          req(!is.null(newnodes))
                          shinyCatch({
                            tmpcomponents <- do.call(
                              bind_rows,
                              lapply(newnodes,
                                     function(n)
                                     { if (is.null(n$Inside))
                                       n$Inside <- 1
                                     if (!n$Inside %in% c(0,1))
                                       stop(paste("Inside should be 0 or 1 for", n$label))
                                     data.frame(id = n$id,
                                                Component = n$label,
                                                Inside = ifelse(is.null(n$Inside),
                                                            1,
                                                            as.integer(n$Inside)))}))
                            if (max(table(c(tmpcomponents$Component,
                                            isolate(currentnet$fluxes$Flux)))) > 1){
                              output$networkviz_proxy <- renderVisNetwork(drawNet())
                              stop("Component names should be unique")
                            }
                            currentnet$components <<- tmpcomponents
                          }
                          )
                        })

                        observe({
                          newfluxes <- input$networkviz_proxy_edges
                          req(!is.null(newfluxes))
                          shinyCatch({

                            tmpfluxes <- do.call(
                              bind_rows,
                              lapply(newfluxes,
                                     function(n){
                                       if (is.null(n$Trophic))
                                         n$Trophic <- 1
                                       if (!n$Trophic %in% c(0, 1))
                                         stop(paste0("Trophic should be 0 or 1 for", n$label))
                                       if (!"label" %in% names(n))
                                         n$label <- paste(isolate(datanet$dictionary[n$from]),
                                                          isolate(datanet$dictionary[n$to]),
                                                          sep = "_")
                                       data.frame(id = n$id,
                                                  Flux = n$label,
                                                  from = n$from,
                                                  to = n$to,
                                                  From = isolate(datanet$dictionary[n$from]),
                                                  To = isolate(datanet$dictionary[n$to]),
                                                  Trophic = ifelse(is.null(n$Trophic),
                                                                   1,
                                                                   as.integer(n$Trophic)))
                                     }
                              ))
                            if (max(table(c(tmpfluxes$Flux,
                                            isolate(currentnet$components$Component)))) > 1){
                              output$networkviz_proxy <- renderVisNetwork(drawNet())
                              stop("fluxes names should be unique")
                            }
                            currentnet$fluxes <<- tmpfluxes


                          }
                          )
                        })

                        observe({
                          req(!is.null(input$networkviz_proxy_graphChange$cmd))
                          cmd <- input$networkviz_proxy_graphChange$cmd
                          if (cmd %in% c("addNode", "editNode")){
                            visNetworkProxy(session$ns("networkviz_proxy")) %>%
                              visNetwork::visGetNodes()
                          }
                          if (cmd %in% c("addEdge", "editEdge")){
                            visNetworkProxy(session$ns("networkviz_proxy")) %>%
                              visNetwork::visGetEdges()
                          }
                          if (cmd %in% c("addEdge", "editEdge")){
                            visNetworkProxy(session$ns("networkviz_proxy")) %>%
                              visNetwork::visGetEdges() %>%
                              visNetwork::visGetNodes()
                          }

                        })
                        return(currentnet)

                      }
  )
}
